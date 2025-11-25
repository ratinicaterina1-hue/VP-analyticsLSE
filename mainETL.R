#  main

#  # ETL/run_all_etl.R

source_if_exists <- function(path) {
  if (file.exists(path)) {
    message("Sourcing: ", path)
    source(path, local = new.env())  # source into a new environment to reduce side-effects
  } else {
    warning("Missing file (skipped): ", path)
  }
}

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




