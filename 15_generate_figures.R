# 15_generate_figures.R
# Purpose: Produce a focused set of publication-ready figures and diagnostics
# that explain the modelling and backtesting steps (features -> models -> backtests).
# The script is defensive: it validates inputs, always writes outputs when possible
# and produces interpretable visualisations for clients and internal stakeholders.

library(tidyverse)
library(lubridate)
library(ggplot2)

# Local path to auxiliary screenshot (if needed in the report)
screenshot_path <- "/mnt/data/Screenshot 2025-11-25 at 21.05.33.png"

# Input / output paths (adjust if your project layout differs)
merged_mktadj_path <- "data/merged/all_tickers_event_windows_mktadj.csv"
merged_macro_path  <- "data/merged/event_windows_macro.csv"
features_path      <- "data/features/events_features.csv"
models_dir         <- "outputs/models"
backtests_dir      <- "outputs/backtests"
out_fig_dir        <- "outputs/figures"

dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)

message("Loading inputs (if present)...")

events_mktadj <- if (file.exists(merged_mktadj_path)) readr::read_csv(merged_mktadj_path, show_col_types = FALSE) %>% mutate(Date = as.Date(Date), Earnings_Date = as.Date(Earnings_Date)) else tibble()
events_macro  <- if (file.exists(merged_macro_path))  readr::read_csv(merged_macro_path, show_col_types = FALSE) %>% mutate(Date = as.Date(Date), Earnings_Date = as.Date(Earnings_Date)) else tibble()
features      <- if (file.exists(features_path))      readr::read_csv(features_path, show_col_types = FALSE) %>% mutate(Earnings_Date = as.Date(Earnings_Date)) else tibble()

# read backtest CSVs (trades_<MODEL>.csv). Each should contain net_cumret per event (per-trade)
trade_files <- if (dir.exists(backtests_dir)) list.files(backtests_dir, pattern = "^trades_.*\\.csv$", full.names = TRUE) else character(0)

# helper to log and save plots
safe_save <- function(plot_obj, filename, width = 9, height = 5) {
  outp <- file.path(out_fig_dir, filename)
  tryCatch({ ggsave(outp, plot_obj, width = width, height = height); message("Saved: ", filename) }, error = function(e) message("Could not save ", filename, ": ", e$message))
}

# 0) Summary of inputs
message("Inputs summary:")
message(" - events_mktadj rows: ", nrow(events_mktadj))
message(" - events_macro rows:  ", nrow(events_macro))
message(" - features rows:      ", nrow(features))
message(" - backtest files:     ", if (length(trade_files)>0) paste(basename(trade_files), collapse = ", ") else "none")

# ------------------ A: High-level event and CAR plots ------------------
if (nrow(events_mktadj) > 0) {
  avg_event_curve_adj <- events_mktadj %>% group_by(Ticker, Days_From_Earnings) %>% summarise(avg_mkt_adj_return = mean(Mkt_Adj_Return, na.rm = TRUE), .groups = "drop")
  p1 <- ggplot(avg_event_curve_adj, aes(Days_From_Earnings, avg_mkt_adj_return, color = Ticker)) +
    geom_line(size = 1) + geom_vline(xintercept = 0, linetype = "dashed") +
    labs(title = "Average Market-Adjusted Daily Return Around Earnings", x = "Days From Earnings", y = "Avg Market-Adj Return") + theme_minimal()
  safe_save(p1, "01_avg_event_curve_mktadj.png", width = 10, height = 4)
  
  cumulative_adj <- events_mktadj %>% arrange(Ticker, Earnings_Date, Date) %>% group_by(Ticker, Earnings_Date) %>% mutate(cum_mkt_adj_return = cumsum(replace_na(Mkt_Adj_Return, 0))) %>% ungroup()
  avg_cum_adj <- cumulative_adj %>% group_by(Ticker, Days_From_Earnings) %>% summarise(avg_cum = mean(cum_mkt_adj_return, na.rm = TRUE), .groups = "drop")
  p2 <- ggplot(avg_cum_adj, aes(Days_From_Earnings, avg_cum, color = Ticker)) + geom_line(size = 1.1) + geom_vline(xintercept = 0, linetype = "dashed") + labs(title = "Average Cumulative Market-Adjusted Return (CAR)", x = "Days From Earnings", y = "Avg CAR") + theme_minimal()
  safe_save(p2, "02_avg_car.png", width = 10, height = 4)
  
  # CAR heatmap to visualise where most alpha concentrates (good for selecting holding windows)
  car_tbl <- cumulative_adj %>% group_by(Ticker, Days_From_Earnings) %>% summarise(mean_cum = mean(cum_mkt_adj_return, na.rm = TRUE), .groups = "drop")
  pA <- ggplot(car_tbl, aes(Days_From_Earnings, Ticker, fill = mean_cum)) + geom_tile() + scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) + labs(title = "CAR heatmap by day and ticker", x = "Days From Earnings", y = "") + theme_minimal()
  safe_save(pA, "03_car_heatmap.png", width = 10, height = 3)
}

# ------------------ B: Macro regime diagnostics ------------------
if (nrow(events_macro) > 0 && all(c("CPI_regime","VIX_regime") %in% colnames(events_macro))) {
  pre_drift <- events_macro %>% filter(Days_From_Earnings %in% -5:-1) %>% group_by(Ticker, CPI_regime) %>% summarise(cum5 = mean(Mkt_Adj_Return, na.rm = TRUE) * 5, .groups = "drop")
  p3 <- ggplot(pre_drift, aes(CPI_regime, cum5, fill = CPI_regime)) + geom_col() + facet_wrap(~Ticker) + labs(title = "Pre-earnings 5-day drift by CPI regime", x = "CPI regime", y = "5-day cum Mkt-Adj Return") + theme_minimal() + theme(legend.position = "none")
  safe_save(p3, "04_pre_drift_by_cpi.png", width = 9, height = 4)
  
  day0_vix <- events_macro %>% filter(Days_From_Earnings == 0) %>% group_by(Ticker, VIX_regime) %>% summarise(avg_day0 = mean(Mkt_Adj_Return, na.rm = TRUE), .groups = "drop")
  p4 <- ggplot(day0_vix, aes(VIX_regime, avg_day0, fill = VIX_regime)) + geom_col() + facet_wrap(~Ticker) + labs(title = "Earnings-day market-adjusted return by VIX regime", x = "VIX regime", y = "Avg day0 return") + theme_minimal() + theme(legend.position = "none")
  safe_save(p4, "05_day0_by_vix.png", width = 9, height = 4)
}

# ------------------ C: Feature diagnostics ------------------
if (nrow(features) > 0) {
  numeric_feats <- features %>% select(where(is.numeric))
  numeric_feats <- numeric_feats %>% select(-any_of(c("label_ret1","label_ret5","label_dir1","label_dir5")))
  if (ncol(numeric_feats) >= 2) {
    cor_m <- cor(numeric_feats, use = "pairwise.complete.obs")
    cor_df <- as.data.frame(as.table(cor_m))
    names(cor_df) <- c("x","y","corr")
    p5 <- ggplot(cor_df, aes(x = x, y = y, fill = corr)) + geom_tile() + scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Feature correlation matrix (numeric features)")
    safe_save(p5, "06_feature_corr.png", width = 8, height = 6)
  }
}

# ------------------ D: Load backtests and compute model-level summaries ------------------
if (length(trade_files) == 0) message("No backtest CSV files found; backtest plots will be skipped")

trade_list <- list()
for (tf in trade_files) {
  nm <- basename(tf) %>% str_remove("^trades_") %>% str_remove("\\.csv$")
  df <- tryCatch(readr::read_csv(tf, show_col_types = FALSE), error = function(e) tibble())
  if (nrow(df) == 0) next
  # ensure columns
  if (!"net_cumret" %in% colnames(df)) df <- df %>% mutate(net_cumret = 0)
  df <- df %>% mutate(trade_idx = row_number(), model = nm)
  trade_list[[nm]] <- df
}

if (length(trade_list) > 0) {
  trade_all <- bind_rows(trade_list)
  
  # Equity curves (per model)
  eq_df <- trade_all %>% group_by(model) %>% arrange(trade_idx) %>% mutate(cum_pnl = cumprod(1 + replace_na(net_cumret, 0)) - 1) %>% ungroup()
  p6 <- ggplot(eq_df, aes(trade_idx, cum_pnl, color = model)) + geom_line() + labs(title = "Backtest cumulative PnL (per trade)", x = "Trade index", y = "Cumulative PnL") + theme_minimal()
  safe_save(p6, "07_backtest_equity_curves.png", width = 10, height = 5)
  
  # Bootstrap bands per model if enough trades
  boots <- map_dfr(names(trade_list), function(nm) {
    df <- trade_list[[nm]]
    if (nrow(df) < 5) return(tibble())
    reps <- 300
    map_dfr(seq_len(reps), function(i) {
      samp <- df %>% slice_sample(n = nrow(df), replace = TRUE) %>% arrange(row_number()) %>% mutate(net_cumret = replace_na(net_cumret, 0))
      cum <- cumprod(1 + samp$net_cumret) - 1
      tibble(model = nm, rep = i, trade_idx = seq_along(cum), cum_pnl = cum)
    })
  })
  if (nrow(boots) > 0) {
    bands <- boots %>% group_by(model, trade_idx) %>% summarise(lo = quantile(cum_pnl, 0.05, na.rm = TRUE), med = median(cum_pnl, na.rm = TRUE), hi = quantile(cum_pnl, 0.95, na.rm = TRUE), .groups = "drop")
    p7 <- ggplot() + geom_ribbon(data = bands, aes(trade_idx, ymin = lo, ymax = hi, fill = model), alpha = 0.12) + geom_line(data = eq_df, aes(trade_idx, cum_pnl, color = model)) + labs(title = "Backtest cumulative PnL with bootstrap bands", x = "Trade index", y = "Cumulative PnL") + theme_minimal()
    safe_save(p7, "08_backtest_bootstrap.png", width = 10, height = 5)
  }
  
  # Trade-level stats summary (win rate, mean, median, n_trades)
  stats <- trade_all %>% group_by(model) %>% summarise(n_trades = n(), win_rate = mean(net_cumret > 0, na.rm = TRUE), mean_ret = mean(net_cumret, na.rm = TRUE), median_ret = median(net_cumret, na.rm = TRUE), sd_ret = sd(net_cumret, na.rm = TRUE), .groups = "drop")
  readr::write_csv(stats, file.path(out_fig_dir, "backtest_trade_stats_summary.csv"))
  
  # Distribution plots
  for (nm in unique(trade_all$model)) {
    dfm <- trade_all %>% filter(model == nm)
    p_hist <- ggplot(dfm, aes(net_cumret)) + geom_histogram(bins = 40) + labs(title = paste0("Trade return distribution: ", nm), x = "Net trade return", y = "Count") + theme_minimal()
    safe_save(p_hist, paste0("09_trade_dist_", nm, ".png"), width = 6, height = 4)
  }
}

# ------------------ E: Model diagnostics (importance, coefficients, PDPs) ------------------
# load models if present and create diagnostics to explain why a model produced signals (or not)
model_files <- list.files(models_dir, pattern = "\\.rds$", full.names = TRUE)
model_names <- basename(model_files) %>% str_remove("\\.rds$")

# helper to locate model by prefix
find_model <- function(prefix) {
  hits <- model_files[grepl(prefix, basename(model_files), fixed = TRUE)]
  if (length(hits) >= 1) return(hits[1])
  return(NULL)
}


# Random forest importance
rf_path <- find_model("model_RANGER")
if (!is.null(rf_path)) {
  rf_obj <- readRDS(rf_path)
  rf_final <- if (is.list(rf_obj) && !is.null(rf_obj$finalModel)) rf_obj$finalModel else rf_obj
  if (is.list(rf_final) && !is.null(rf_final$variable.importance)) {
    vi <- tibble(feature = names(rf_final$variable.importance), importance = as.numeric(rf_final$variable.importance)) %>% arrange(desc(importance))
    p_vi <- ggplot(vi %>% slice_head(n = 30), aes(reorder(feature, importance), importance)) + geom_col() + coord_flip() + labs(title = "Random forest: top variable importance", x = "", y = "Importance") + theme_minimal()
    safe_save(p_vi, "10_rf_variable_importance.png", width = 8, height = 6)
  }
}

# LASSO coefficients
lasso_path <- find_model("model_LASSO")
if (!is.null(lasso_path)) {
  lmod <- readRDS(lasso_path)
  coefs <- tryCatch(as.matrix(coef(lmod)), error = function(e) NULL)
  if (!is.null(coefs)) {
    coefs_df <- tibble(feature = rownames(coefs), coef = as.numeric(coefs[,1])) %>% filter(feature != "(Intercept)") %>% arrange(desc(abs(coef)))
    p_lasso <- ggplot(coefs_df %>% slice_head(n = 30), aes(reorder(feature, coef), coef)) + geom_col() + coord_flip() + labs(title = "LASSO: top coefficients by magnitude", x = "", y = "Coefficient") + theme_minimal()
    safe_save(p_lasso, "11_lasso_top_coeffs.png", width = 8, height = 6)
  }
}

# OLS summary: coefficients and adjusted R^2
ols_path <- find_model("model_OLS")
if (!is.null(ols_path)) {
  omod <- readRDS(ols_path)
  s <- broom::tidy(omod)
  s_glance <- broom::glance(omod)
  p_ols <- ggplot(s %>% filter(term != "(Intercept)"), aes(reorder(term, estimate), estimate)) + geom_col() + coord_flip() + labs(title = paste0("OLS coefficients (adj.R2=", round(s_glance$adj.r.squared,3), ")"), x = "", y = "Estimate") + theme_minimal()
  safe_save(p_ols, "12_ols_coeffs.png", width = 8, height = 6)
}

# Partial dependence proxies (EPS_Surprise and pre5) using the saved recipe and preferred model
recipe_path <- file.path(models_dir, "preprocessing_recipe.rds")
if (file.exists(recipe_path) && nrow(features) > 0) {
  prep <- readRDS(recipe_path)
  # choose best-available model for PDP (RANGER > XGBoost > LASSO > OLS)
  pdp_model_path <- find_model("model_RANGER") %||% find_model("model_XGBoost") %||% find_model("model_LASSO") %||% find_model("model_OLS")
  if (!is.null(pdp_model_path) && all(c("EPS_Surprise","pre5") %in% colnames(features))) {
    model_label <- basename(pdp_model_path) %>% str_remove("\\.rds$")
    pred_fun <- function(new_df) {
      baked <- tryCatch(bake(prep, new_data = new_df), error = function(e) NULL)
      if (is.null(baked)) return(rep(NA_real_, nrow(new_df)))
      mod <- readRDS(pdp_model_path)
      if (str_detect(basename(pdp_model_path), "RANGER")) {
        return(tryCatch(predict(mod, newdata = baked)$pred, error = function(e) rep(NA_real_, nrow(baked))))
      } else if (str_detect(basename(pdp_model_path), "XGBoost")) {
        m <- as.matrix(baked %>% select(-any_of(c("label_ret1","Ticker","Earnings_Date"))))
        d <- xgboost::xgb.DMatrix(m)
        return(tryCatch(predict(mod, d), error = function(e) rep(NA_real_, nrow(baked))))
      } else if (str_detect(basename(pdp_model_path), "LASSO")) {
        x <- model.matrix(label_ret1 ~ . -1, data = baked %>% select(-any_of(c("Ticker","Earnings_Date"))))
        common <- intersect(colnames(x), rownames(coef(mod)))
        if (length(common) == 0) return(rep(NA_real_, nrow(x)))
        x2 <- x[, common, drop = FALSE]
        return(tryCatch(as.numeric(predict(mod, newx = x2, s = attr(mod, "lambda") %||% NULL)), error = function(e) rep(NA_real_, nrow(x2))))
      } else if (str_detect(basename(pdp_model_path), "OLS")) {
        return(tryCatch(predict(mod, newdata = baked), error = function(e) rep(NA_real_, nrow(baked))))
      }
      rep(NA_real_, nrow(new_df))
    }
    eps_grid <- quantile(features$EPS_Surprise, probs = seq(0.05,0.95,length.out = 20), na.rm = TRUE) %>% unique()
    pre5_grid <- quantile(features$pre5, probs = seq(0.05,0.95,length.out = 20), na.rm = TRUE) %>% unique()
    pdp_eps <- map_dfr(eps_grid, function(v) { d2 <- features; d2$EPS_Surprise <- v; tibble(var = v, pred = mean(pred_fun(d2), na.rm = TRUE)) })
    pdp_pre5 <- map_dfr(pre5_grid, function(v) { d2 <- features; d2$pre5 <- v; tibble(var = v, pred = mean(pred_fun(d2), na.rm = TRUE)) })
    p_pdp_eps <- ggplot(pdp_eps, aes(var, pred)) + geom_line() + labs(title = paste0(model_label, ": partial effect - EPS_Surprise"), x = "EPS Surprise", y = "Avg predicted return") + theme_minimal()
    p_pdp_pre5 <- ggplot(pdp_pre5, aes(var, pred)) + geom_line() + labs(title = paste0(model_label, ": partial effect - pre5 momentum"), x = "pre5 (cum ret)", y = "Avg predicted return") + theme_minimal()
    safe_save(p_pdp_eps, "13_pdp_eps.png", width = 7, height = 4)
    safe_save(p_pdp_pre5, "14_pdp_pre5.png", width = 7, height = 4)
  }
}

# ------------------ F: Model performance summary table for reporting ------------------
metrics_file <- file.path(models_dir, "model_metrics.csv")
if (file.exists(metrics_file)) {
  metrics <- readr::read_csv(metrics_file, show_col_types = FALSE)
  # ensure human-friendly ordering
  metrics <- metrics %>% mutate(model = as.character(model))
  readr::write_csv(metrics, file.path(out_fig_dir, "model_metrics_summary.csv"))
  # quick bar chart of RMSE and MAE
  if (all(c("rmse","mae") %in% colnames(metrics))) {
    m_long <- metrics %>% pivot_longer(cols = c(rmse, mae), names_to = "metric", values_to = "value")
    p_metrics <- ggplot(m_long, aes(model, value, fill = metric)) + geom_col(position = position_dodge()) + labs(title = "Model error metrics (RMSE, MAE)") + theme_minimal()
    safe_save(p_metrics, "15_model_metrics_errors.png", width = 8, height = 4)
  }
}

# ------------------ G: Pipeline summary image ------------------
pipeline_tbl <- tribble(
  ~step, ~script, ~output,
  "ETL: prices & earnings", "00-03_etl.R", "data/merged/*",
  "Feature engineering", "11_build_features.R", "data/features/events_features.csv",
  "Model training", "12_train_models.R", "outputs/models/*",
  "Backtests", "13_backtest_strategies.R", "outputs/backtests/*",
  "Robustness", "14_robustness_checks.R", "outputs/robustness/*",
  "Figures", "15_generate_figures.R", "outputs/figures/*"
)

p_pipeline <- ggplot(pipeline_tbl, aes(x = factor(step, levels = pipeline_tbl$step), y = 1, label = paste(step, "
", script, "
", output))) + geom_tile(fill = "gray98") + geom_text(size = 3) + theme_void() + labs(title = "Project pipeline: scripts and outputs") + theme(plot.title = element_text(hjust = 0.5))
safe_save(p_pipeline, "16_pipeline_summary.png", width = 10, height = 3)

message("Figure generation finished. Check: ", out_fig_dir)

