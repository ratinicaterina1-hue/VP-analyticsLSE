library(tidyverse)
library(lubridate)
library(zoo)

# Load event windows
events <- read_csv("data/merged/all_tickers_event_windows_mktadj.csv",
                   show_col_types = FALSE)

# Helper to load macro CSVs
load_macro <- function(file, varname) {
  read_csv(paste0("data/macro/", file), show_col_types = FALSE) %>%
    mutate(Date = as.Date(Date)) %>%
    rename(!!varname := Value)
}

cpi      <- load_macro("cpi.csv", "CPI")
fedfunds <- load_macro("fedfunds.csv", "FedFunds")
vix      <- load_macro("vix.csv", "VIX")
unemp    <- load_macro("unemployment.csv", "Unemployment")
gdp      <- load_macro("gdp.csv", "GDP_Q")

# ---- Convert macro data to DAILY frequency ----

expand_daily <- function(df, var) {
  df %>%
    complete(Date = seq(min(Date), max(Date), by = "day")) %>%
    arrange(Date) %>%
    mutate(!!var := na.locf(!!sym(var), na.rm = FALSE))
}

cpi_daily      <- expand_daily(cpi, "CPI")
fedfunds_daily <- expand_daily(fedfunds, "FedFunds")
vix_daily      <- expand_daily(vix, "VIX")
unemp_daily    <- expand_daily(unemp, "Unemployment")
gdp_daily      <- expand_daily(gdp, "GDP_Q")

# ---- Merge macro onto event windows ----

events_macro <- events %>%
  left_join(cpi_daily, by = "Date") %>%
  left_join(fedfunds_daily, by = "Date") %>%
  left_join(vix_daily, by = "Date") %>%
  left_join(unemp_daily, by = "Date") %>%
  left_join(gdp_daily, by = "Date")

# ---- Create macro regimes ----

events_macro <- events_macro %>%
  mutate(
    CPI_regime = if_else(CPI > median(CPI, na.rm = TRUE), "HighInfl", "LowInfl"),
    VIX_regime = if_else(VIX > median(VIX, na.rm = TRUE), "HighVIX", "LowVIX"),
    Fed_regime = case_when(
      FedFunds > lag(FedFunds) ~ "Tightening",
      FedFunds < lag(FedFunds) ~ "Easing",
      TRUE ~ "Neutral"
    )
  )

write_csv(events_macro, "data/merged/event_windows_macro.csv")
message("✔ Macro merged and regimes computed.")
