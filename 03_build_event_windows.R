library(tidyverse)
library(lubridate)

event_window <- 20  # days before and after earnings

# ---------- 1. LOAD CLEANED PRICES FROM CSV ----------

# This is the file created by 01_clean_prices.R
prices_all <- read_csv("data/prices_clean/all_prices_clean.csv",
                       show_col_types = FALSE)

# Just to be sure:
# head(prices_all)

# ---------- 2. LOAD CLEANED EARNINGS FROM CSV ----------

nvda_earnings_clean  <- read_csv("data/earnings_clean/nvda_earnings_clean.csv",
                                 show_col_types = FALSE)
aapl_earnings_clean  <- read_csv("data/earnings_clean/aapl_earnings_clean.csv",
                                 show_col_types = FALSE)
googl_earnings_clean <- read_csv("data/earnings_clean/googl_earnings_clean.csv",
                                 show_col_types = FALSE)

earnings_all <- bind_rows(
  nvda_earnings_clean,
  aapl_earnings_clean,
  googl_earnings_clean
)

# Optional: drop future earnings without actual EPS
earnings_all <- earnings_all %>%
  filter(!is.na(EPS_Actual))

# ---------- 3. FUNCTION TO BUILD EVENT WINDOWS PER TICKER ----------

build_windows_for_ticker <- function(ticker_symbol) {
  
  p <- prices_all %>% filter(Ticker == ticker_symbol)
  e <- earnings_all %>% filter(Ticker == ticker_symbol)
  
  message("Building windows for ", ticker_symbol, " with ",
          nrow(e), " earnings events")
  
  map_dfr(seq_len(nrow(e)), function(i) {
    edate <- e$Earnings_Date[i]
    
    start_date <- edate - days(event_window)
    end_date   <- edate + days(event_window)
    
    p %>%
      filter(Date >= start_date, Date <= end_date) %>%
      mutate(
        Days_From_Earnings = as.integer(Date - edate),
        Earnings_Date      = edate,
        EPS_Estimate       = e$EPS_Estimate[i],
        EPS_Actual         = e$EPS_Actual[i],
        EPS_Surprise       = e$EPS_Surprise[i],
        Surprise_Pct       = e$Surprise_Pct[i]
      )
  })
}

# ---------- 4. BUILD WINDOWS FOR ALL THREE TICKERS ----------

nvda_events  <- build_windows_for_ticker("NVDA")
aapl_events  <- build_windows_for_ticker("AAPL")
googl_events <- build_windows_for_ticker("GOOGL")

events_all <- bind_rows(nvda_events, aapl_events, googl_events)

# ---------- 5. SAVE MERGED DATASET ----------

dir.create("data/merged", showWarnings = FALSE)
write_csv(events_all, "data/merged/all_tickers_event_windows.csv")

message("✅ Event-window dataset saved to data/merged/all_tickers_event_windows.csv")
