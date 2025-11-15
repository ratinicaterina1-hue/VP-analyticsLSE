nvda_earnings  <- read_csv("data/earnings/nvda_earnings.csv", show_col_types = FALSE)
aapl_earnings  <- read_csv("data/earnings/aapl_earnings.csv", show_col_types = FALSE)          
googl_earnings <- read_csv("data/earnings/googl_earnings.csv", show_col_types = FALSE)         


library(tidyverse)
library(lubridate)

clean_earnings <- function(df, ticker){
  
  df_clean <- df %>%
    mutate(
      Ticker = ticker,
      
      # Convert the datetime to Date only
      Earnings_Date = as.Date(`Earnings Date`),
      
      EPS_Estimate = `EPS Estimate`,
      EPS_Actual   = `Reported EPS`,
      
      # EPS Surprise in absolute terms
      EPS_Surprise = EPS_Actual - EPS_Estimate,
      
      Surprise_Pct = `Surprise(%)`
    ) %>%
    select(Ticker, Earnings_Date, EPS_Estimate, EPS_Actual, EPS_Surprise, Surprise_Pct)
  
  return(df_clean)
}

# Load raw earnings
nvda_raw  <- read_csv("data/earnings/nvda_earnings.csv", show_col_types = FALSE)
aapl_raw  <- read_csv("data/earnings/aapl_earnings.csv", show_col_types = FALSE)
googl_raw <- read_csv("data/earnings/googl_earnings.csv", show_col_types = FALSE)

# Clean them
nvda_earnings_clean  <- clean_earnings(nvda_raw,  "NVDA")
aapl_earnings_clean  <- clean_earnings(aapl_raw,  "AAPL")
googl_earnings_clean <- clean_earnings(googl_raw, "GOOGL")

# Save cleaned versions
dir.create("data/earnings_clean", showWarnings = FALSE)

write_csv(nvda_earnings_clean,  "data/earnings_clean/nvda_earnings_clean.csv")
write_csv(aapl_earnings_clean,  "data/earnings_clean/aapl_earnings_clean.csv")
write_csv(googl_earnings_clean, "data/earnings_clean/googl_earnings_clean.csv")

message("✔ Earnings cleaned and saved to data/earnings_clean/")

