#  main for model training and scripts

message("=== VP Analytics: Full Pipeline Execution ===")

source_if_exists <- function(path) {
  if (file.exists(path)) {
    message("Sourcing: ", path)
    source(path, local = new.env())  # source into a new environment to reduce side-effects
  } else {
    warning("Missing file (skipped): ", path)
  }
}


# --------------------------
# 0. ETL (prices + earnings)
# --------------------------
# 01: Clean raw price CSVs -> saves per-ticker and combined cleaned prices
source_if_exists("ETL/01_prices_clean.R")


# 02: Clean raw earnings CSVs -> saves per-ticker cleaned earnings
source_if_exists("ETL/02_earnings_clean.R")


# 03: Build event windows per earnings date (joins prices + earnings)
source_if_exists("ETL/03_build_event_windows.R")


# 04: Basic exploratory analysis & plots (avg returns, cumulative returns)
source_if_exists("ETL/04_basic_analysis.R")

# 05: Download SP500 daily prices (or load cached file)
source_if_exists("ETL/05_download_sp500.R")

# 06: Compute market-adjusted returns and save merged dataset
source_if_exists("ETL/06_market_adjusted_analysis.R")


# 08: Download macro series from FRED (CPI, FedFunds, VIX, UNRATE, GDP)
source_if_exists("ETL/08_download_macro.R")

# 09: Merge macro onto event windows and compute regimes (HighVIX, HighInfl, Fed regime)
source_if_exists("ETL/09_merge_macro_with_events.R")

# 10: Macro effects analysis and plotting (pre-drift by regime, day0 by VIX, regressions)
source_if_exists("ETL/10_macro_effects_analysis.R")

message("ETL pipeline execution finished (check console messages / warnings).")

# --------------------------
# 1. Feature Engineering
# --------------------------
source("scripts/11_build_features.R")     # creates per-event features used for ML (pre-returns, vols, macro)

# --------------------------
# 2. Model Training
# --------------------------
source("scripts/12_train_models.R")       # trains OLS, LASSO, Random Forest, XGBoost + saves metrics

# --------------------------
# 3. Backtesting
# --------------------------
source("scripts/13_backtest_strategies.R") # generates long/short signals and evaluates per-event returns

# --------------------------
# 4. Robustness Checks
# --------------------------
source("scripts/14_robustness_checks.R")   # stability across periods and feature sensitivity

# --------------------------
# 5. Figures for Client
# --------------------------
source("scripts/15_generate_figures.R")    # saves explanatory charts for report & presentation

message("=== Pipeline complete. Outputs in /outputs ===")
