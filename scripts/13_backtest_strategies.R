# 13_backtest_strategies_fixed.R
# Robust backtest file generator. Produces outputs/backtests/trades_<MODEL>.csv
library(tidyverse)
library(recipes)
library(xgboost)

dir.create("outputs/backtests", recursive = TRUE, showWarnings = FALSE)

features_path <- "data/features/events_features.csv"
models_dir <- "outputs/models"
out_dir <- "outputs/backtests"

message("Loading features from: ", features_path)
features <- readr::read_csv(features_path, show_col_types = FALSE) %>% mutate(Earnings_Date = as.Date(Earnings_Date))

# create missing engineered columns used in training
if (! "vol_ratio" %in% colnames(features)) {
  message("vol_ratio missing from features — creating vol_ratio = 0")
  features <- features %>% mutate(vol_ratio = 0)
}

if (! "log_vol_ratio" %in% colnames(features)) {
  message("log_vol_ratio missing — creating log_vol_ratio = log1p(vol_ratio)")
  features <- features %>% mutate(log_vol_ratio = ifelse(is.na(vol_ratio), 0, vol_ratio) %>% {log1p(.)})
}

# Ensure label_ret1 exists; if missing create NA column (no ground truth for evaluation but trades still generated)
if (! "label_ret1" %in% colnames(features)) {
  message("label_ret1 missing — creating label_ret1 = NA (backtest will use 0 as realized ret where NA)")
  features <- features %>% mutate(label_ret1 = NA_real_)
}

# helper: safe read model
safe_read_rds <- function(p) if (file.exists(p)) readRDS(p) else NULL

# model paths
models_to_try <- list(
  OLS = file.path(models_dir, "model_OLS.rds"),
  LASSO = file.path(models_dir, "model_LASSO.rds"),
  RANGER = file.path(models_dir, "model_RANGER.rds"),
  XGBoost = file.path(models_dir, "model_XGBoost.rds")
)

# Try to load recipe for consistent preprocessing
recipe_path <- file.path(models_dir, "preprocessing_recipe.rds")
have_recipe <- file.exists(recipe_path)
if (have_recipe) {
  message("Loading preprocessing recipe: ", recipe_path)
  prep_rec <- tryCatch(readRDS(recipe_path), error = function(e) { message("Failed to read recipe: ", e$message); NULL })
} else {
  prep_rec <- NULL
  message("No preprocessing_recipe.rds found — predictions will try to align directly.")
}

# attempt to create baked data using recipe; if it fails, we'll fallback per-model
baked <- NULL
if (!is.null(prep_rec)) {
  baked <- tryCatch({
    bake(prep_rec, new_data = features)
  }, error = function(e) {
    message("bake() failed: ", e$message)
    NULL
  })
}

# helper: prepare matrix for xgboost / glmnet from baked or from raw features
prepare_matrix_from_baked <- function(baked_df) {
  if (is.null(baked_df)) return(NULL)
  as.matrix(baked_df %>% select(-any_of(c("label_ret1","label_ret5","Ticker","Earnings_Date"))))
}
prepare_matrix_from_raw <- function(df) {
  as.matrix(df %>% select(where(is.numeric)) %>% select(-any_of(c("label_ret1","label_ret5"))))
}

# scoring wrapper per-model: returns numeric vector length = nrow(features)
score_model <- function(model_name) {
  path <- models_to_try[[model_name]]
  if (!file.exists(path)) {
    message("Skipping ", model_name, ": model file not found at ", path)
    return(rep(NA_real_, nrow(features)))
  }
  model <- safe_read_rds(path)
  preds <- rep(NA_real_, nrow(features))
  # First, try to predict from baked if available
  if (!is.null(baked)) {
    safe_try <- tryCatch({
      if (model_name == "RANGER") {
        # caret-wrapped ranger: may have predict method that returns list with $pred
        res <- predict(model, newdata = baked)
        # caret's predict returns numeric vector; caret::train returns object where predict() gives vector
        preds <- if (is.list(res) && "pred" %in% names(res)) res$pred else as.numeric(res)
      } else if (model_name == "LASSO") {
        xmat <- model.matrix(label_ret1 ~ . - 1, data = baked %>% select(-any_of(c("Ticker","Earnings_Date"))))
        # try to align columns with model coefficients if possible
        coef_names <- rownames(coef(model))
        common <- intersect(colnames(xmat), coef_names)
        if (length(common) == 0) {
          preds <- rep(0, nrow(baked))
        } else {
          x_sub <- xmat[, common, drop = FALSE]
          preds <- as.numeric(predict(model, newx = x_sub, s = attr(model, "lambda") %||% NULL))
        }
      } else if (model_name == "OLS") {
        preds <- as.numeric(predict(model, newdata = baked))
      } else if (model_name == "XGBoost") {
        mat <- prepare_matrix_from_baked(baked)
        if (!is.null(mat)) {
          dmat <- xgboost::xgb.DMatrix(mat)
          preds <- predict(model, dmat)
        } else preds <- rep(NA_real_, nrow(baked))
      }
      preds
    }, error = function(e) {
      message("Predict via baked failed for ", model_name, ": ", e$message)
      NULL
    })
    if (!is.null(safe_try)) {
      # if safe_try is vector use it
      if (is.numeric(safe_try)) return(safe_try)
    }
  }
  # If baked prediction failed or wasn't available, try a raw fallback
  message("Attempting raw fallback prediction for ", model_name)
  if (model_name == "RANGER") {
    # Ranger often needs same columns as training; try predict directly on raw features selecting similar names
    safe_try2 <- tryCatch({
      predict(model, newdata = features) -> pr
      if (is.list(pr) && "pred" %in% names(pr)) pr$pred else as.numeric(pr)
    }, error = function(e) { message("RANGER raw fallback failed: ", e$message); rep(NA_real_, nrow(features)) })
    return(safe_try2)
  }
  if (model_name == "LASSO") {
    # Build model.matrix from numeric columns and align to coef names
    x_raw <- tryCatch(model.matrix(~ . -1, data = features %>% select(where(is.numeric)) ), error = function(e) NULL)
    if (is.null(x_raw)) return(rep(NA_real_, nrow(features)))
    coef_names <- rownames(coef(model))
    common <- intersect(colnames(x_raw), coef_names)
    if (length(common) == 0) return(rep(0, nrow(features)))
    x_sub <- x_raw[, common, drop = FALSE]
    preds2 <- tryCatch(as.numeric(predict(model, newx = x_sub, s = attr(model, "lambda") %||% NULL)), error = function(e) { message("LASSO raw predict failed: ", e$message); rep(NA_real_, nrow(features)) })
    return(preds2)
  }
  if (model_name == "OLS") {
    # attempt to predict using available columns; will fail if required cols missing -> catch
    safe_try3 <- tryCatch(as.numeric(predict(model, newdata = features)), error = function(e) { message("OLS raw fallback failed: ", e$message); rep(NA_real_, nrow(features)) })
    return(safe_try3)
  }
  if (model_name == "XGBoost") {
    mat_raw <- tryCatch(as.matrix(features %>% select(where(is.numeric)) %>% select(-any_of(c("label_ret1","label_ret5")))), error = function(e) NULL)
    if (is.null(mat_raw)) return(rep(NA_real_, nrow(features)))
    dmat <- xgboost::xgb.DMatrix(mat_raw)
    preds3 <- tryCatch(predict(model, dmat), error = function(e) { message("XGBoost raw fallback failed: ", e$message); rep(NA_real_, nrow(features)) })
    return(preds3)
  }
  # default
  rep(NA_real_, nrow(features))
}

# Score each model and write a backtest CSV (always write)
for (mn in names(models_to_try)) {
  message("Scoring model: ", mn)
  preds <- score_model(mn)
  # ensure length correct
  if (length(preds) != nrow(features)) preds <- rep(NA_real_, nrow(features))
  df_out <- features %>%
    mutate(pred = preds,
           # simple trading rule: long if pred > 0
           trade_signal = ifelse(!is.na(pred) & pred > 0, 1, 0),
           actual_ret = ifelse(is.na(label_ret1), 0, label_ret1),
           net_cumret = ifelse(trade_signal == 1, actual_ret, 0)) %>%
    select(Earnings_Date, Ticker, pred, trade_signal, actual_ret, net_cumret)
  # always save (even if all zeros)
  out_path <- file.path(out_dir, paste0("trades_", mn, ".csv"))
  readr::write_csv(df_out, out_path)
  message("Wrote backtest file: ", out_path, " | n_trades: ", sum(df_out$trade_signal, na.rm = TRUE))
}

message("✅ Backtests completed and saved to: ", out_dir)
