# 14_robustness_checks.R
# Purpose: run robustness & sensitivity checks for models and backtests.
# Outputs: outputs/robustness/robustness_summary.csv and diagnostic plots.
# Assumes: ETL, features, models and backtests are produced by earlier scripts.

library(tidyverse)
library(lubridate)
library(glue)

# Paths (use 'paths' if available)
if (exists("paths")) {
  features_path <- file.path(paths$outputs, "features", "events_features.csv")
  models_dir <- file.path(paths$outputs, "models")
  out_dir <- file.path(paths$outputs, "robustness")
} else {
  features_path <- "data/features/events_features.csv"
  models_dir <- "outputs/models"
  out_dir <- "outputs/robustness"
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

message("Loading features from: ", features_path)
features <- readr::read_csv(features_path, show_col_types = FALSE) %>% mutate(Earnings_Date = as.Date(Earnings_Date))

# Helper: metrics
rmse_vec <- function(truth, pred) sqrt(mean((truth - pred)^2, na.rm = TRUE))
mae_vec  <- function(truth, pred) mean(abs(truth - pred), na.rm = TRUE)
rsq_vec  <- function(truth, pred) { if (all(is.na(truth) | is.na(pred))) return(NA_real_) else cor(truth, pred, use = "complete.obs")^2 }

# 1) Subperiod analysis ------------------------------------------------------
# Define subperiods (adjust as you wish)
subperiods <- tibble(
  name = c("pre_2020", "covid_2020", "post_2020", "tightening_2022_onwards"),
  start = as.Date(c("2015-01-01", "2020-03-01", "2021-01-01", "2022-01-01")),
  end   = as.Date(c("2019-12-31", "2020-12-31", "2021-12-31", "2025-12-31"))
)

# Utility to run a lightweight model (OLS and Ranger) on a subset and return metrics
run_models_on_subset <- function(df_subset) {
  # keep rows with label_ret1
  df <- df_subset %>% filter(!is.na(label_ret1))
  if (nrow(df) < 20) return(tibble(model = c("OLS","RANGER"), rmse = NA_real_, mae = NA_real_, rsq = NA_real_))
  
  # simple split: 80/20 by date
  df <- df %>% arrange(Earnings_Date)
  cut_ix <- max(1, floor(0.8 * nrow(df)))
  train <- df %>% slice_head(n = cut_ix)
  test  <- df %>% slice_tail(n = nrow(df) - cut_ix)
  
  # simple feature set
  vars <- c("pre1","pre3","pre5","vol10","log_vol_ratio","EPS_Surprise","Surprise_Pct","VIX","CPI","FedFunds")
  vars <- intersect(vars, colnames(df))
  if (length(vars) == 0) return(tibble(model = c("OLS","RANGER"), rmse = NA_real_, mae = NA_real_, rsq = NA_real_))
  
  # Impute medians in-place for quick runs
  for (v in vars) {
    med <- median(train[[v]], na.rm = TRUE)
    train[[v]] <- ifelse(is.na(train[[v]]), med, train[[v]])
    test[[v]]  <- ifelse(is.na(test[[v]]), med, test[[v]])
  }
  
  # OLS
  fmla <- as.formula(paste("label_ret1 ~", paste(vars, collapse = " + ")))
  ols <- tryCatch(lm(fmla, data = train), error = function(e) NULL)
  ols_pred <- if (!is.null(ols)) predict(ols, newdata = test) else rep(NA_real_, nrow(test))
  
  # Ranger (quick default settings) - only if package present
  ranger_available <- requireNamespace("ranger", quietly = TRUE)
  if (ranger_available) {
    rf <- tryCatch(ranger::ranger(fmla, data = train, num.trees = 200), error = function(e) NULL)
    rf_pred <- if (!is.null(rf)) predict(rf, data = test)$predictions else rep(NA_real_, nrow(test))
  } else {
    rf_pred <- rep(NA_real_, nrow(test))
  }
  
  tibble(
    model = c("OLS","RANGER"),
    rmse = c(rmse_vec(test$label_ret1, ols_pred), rmse_vec(test$label_ret1, rf_pred)),
    mae  = c(mae_vec(test$label_ret1, ols_pred), mae_vec(test$label_ret1, rf_pred)),
    rsq  = c(rsq_vec(test$label_ret1, ols_pred), rsq_vec(test$label_ret1, rf_pred))
  )
}

# Run subperiod checks
sub_results <- list()
for (i in seq_len(nrow(subperiods))) {
  sp <- subperiods[i,]
  df_sub <- features %>% filter(Earnings_Date >= sp$start, Earnings_Date <= sp$end)
  res <- run_models_on_subset(df_sub) %>% mutate(period = sp$name)
  sub_results[[sp$name]] <- res
}
sub_results_tbl <- bind_rows(sub_results)
readr::write_csv(sub_results_tbl, file.path(out_dir, "subperiod_model_metrics.csv"))

# 2) Window sensitivity ------------------------------------------------------
# Vary event window label choices: use label_ret1 (default), label_ret5, plus differences
window_tests <- list(
  list(label = "label_ret1", hold = 1),
  list(label = "label_ret5", hold = 5)
)

window_results <- list()
for (wt in window_tests) {
  lab <- wt$label
  dfw <- features %>% filter(!is.na(.data[[lab]]))
  # quick split
  dfw <- dfw %>% arrange(Earnings_Date)
  cut_ix <- max(1, floor(0.8 * nrow(dfw)))
  train <- dfw %>% slice_head(n = cut_ix)
  test  <- dfw %>% slice_tail(n = nrow(dfw) - cut_ix)
  
  preds <- c()
  # OLS on same features
  vars <- intersect(c("pre1","pre3","pre5","vol10","log_vol_ratio","EPS_Surprise","Surprise_Pct","VIX","CPI","FedFunds"), colnames(dfw))
  if (length(vars)>0) {
    fmla <- as.formula(paste(lab, "~", paste(vars, collapse = " + ")))
    ols <- tryCatch(lm(fmla, data = train), error = function(e) NULL)
    if (!is.null(ols)) {
      ols_pred <- predict(ols, newdata = test)
      window_results[[paste0(lab,"_OLS")]] <- tibble(label = lab, model = "OLS", rmse = rmse_vec(test[[lab]], ols_pred), mae = mae_vec(test[[lab]], ols_pred), rsq = rsq_vec(test[[lab]], ols_pred))
    }
  }
}
window_results_tbl <- bind_rows(window_results)
readr::write_csv(window_results_tbl, file.path(out_dir, "window_sensitivity_metrics.csv"))

# 3) Holding period sensitivity for backtests --------------------------------
holdings <- c(1,3,5,10)
backtest_summaries <- list()

bt_dir <- if (exists("paths")) file.path(paths$outputs, "backtests") else "outputs/backtests"

if (dir.exists(bt_dir)) {
  trade_files <- list.files(bt_dir, pattern = "trades_.*\\.csv$", full.names = TRUE)
  
  for (tf in trade_files) {
    mname <- str_remove(basename(tf), "trades_(.*)\\.csv")
    trades <- readr::read_csv(tf, show_col_types = FALSE)
    
    # --- FIX: create hold_days if missing ---
    if (!"hold_days" %in% colnames(trades)) {
      if ("entry_date" %in% colnames(trades) & "exit_date" %in% colnames(trades)) {
        trades <- trades %>%
          mutate(
            entry_date = as.Date(entry_date),
            exit_date  = as.Date(exit_date),
            hold_days = as.numeric(exit_date - entry_date)
          )
      } else {
        warning(glue("Skipping {tf}: cannot derive hold_days"))
        next
      }
    }
    
    # summarise
    s <- trades %>%
      group_by(hold_days) %>%
      summarise(
        n = n(),
        avg_cumret = mean(cumret, na.rm = TRUE),
        avg_net    = mean(net_cumret, na.rm = TRUE),
        winrate    = mean(net_cumret > 0, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(model = mname)
    
    backtest_summaries[[mname]] <- s
  }
  
  if (length(backtest_summaries) > 0) {
    readr::write_csv(
      bind_rows(backtest_summaries),
      file.path(out_dir, "backtest_hold_summary.csv")
    )
  }
}

# 4) Output unified robustness summary --------------------------------------
robustness_summary <- list(
  subperiods = sub_results_tbl,
  window_sensitivity = window_results_tbl
)

# Flatten and save key tables
readr::write_csv(sub_results_tbl, file.path(out_dir, "robustness_subperiods.csv"))
readr::write_csv(window_results_tbl, file.path(out_dir, "robustness_window_sensitivity.csv"))

message("✅ Robustness checks done. Outputs saved to: ", out_dir)

# End of 14_robustness_checks.R
