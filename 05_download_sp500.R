# library(tidyverse)
library(lubridate)
# install.packages("quantmod")
library(quantmod)

# Download SP500 prices
getSymbols("^GSPC", src = "yahoo",
           from = "2015-01-01",
           to   = "2025-12-31",
           auto.assign = TRUE)

sp500 <- GSPC %>%
  as_tibble() %>%
  mutate(Date = as.Date(index(GSPC))) %>%
  select(Date,
         Open = GSPC.Open,
         High = GSPC.High,
         Low  = GSPC.Low,
         Close = GSPC.Close,
         Volume = GSPC.Volume) %>%
  arrange(Date)

sp500 <- sp500 %>%
  mutate(Return = Close / lag(Close) - 1)

# Save to CSV
dir.create("data/sp500", showWarnings = FALSE)
write_csv(sp500, "data/sp500/sp500_prices.csv")

message("✔ SP500 downloaded and saved.")
