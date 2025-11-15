
library(tidyverse)

nvda_prices  <- read_csv("data/prices/nvda_prices_2015_2025.csv", show_col_types = FALSE)
aapl_prices  <- read_csv("data/prices/aapl_prices_2015_2025.csv", show_col_types = FALSE)
googl_prices <- read_csv("data/prices/googl_prices_2015_2025.csv", show_col_types = FALSE)

head(nvda_prices)



library(tidyverse)
library(lubridate)

prices_dir <- "data/prices"

# Function to clean these "weird" price CSVs
clean_prices <- function(filename, ticker_label = NULL) {
  path <- file.path(prices_dir, filename)
  message("Cleaning: ", path)
  
  raw <- read_csv(path, show_col_types = FALSE)
  
  # 1) Drop the first two metadata rows (Ticker / Date row)
  df <- raw |> 
    slice(-(1:2)) |>   # remove rows 1 and 2
    rename(Date = Price)  # Price column is actually the date
  
  # 2) Convert types
  df <- df |>
    mutate(
      Date   = as.Date(Date),
      Close  = as.numeric(Close),
      High   = as.numeric(High),
      Low    = as.numeric(Low),
      Open   = as.numeric(Open),
      Volume = as.numeric(Volume)
    ) |>
    arrange(Date) |>
    mutate(
      Return = Close / lag(Close) - 1,
      Ticker = ticker_label
    )
  
  df
}

# Clean each ticker
nvda_clean  <- clean_prices("nvda_prices_2015_2025.csv", "NVDA")
aapl_clean  <- clean_prices("aapl_prices_2015_2025.csv", "AAPL")
googl_clean <- clean_prices("googl_prices_2015_2025.csv", "GOOGL")

# Optional: bind all into one tibble
prices_all <- bind_rows(nvda_clean, aapl_clean, googl_clean)

# Quick sanity checks
print(head(nvda_clean))
print(head(aapl_clean))
print(head(googl_clean))

# Save a cleaned version (optional but useful)
dir.create("data/prices_clean", showWarnings = FALSE)
write_csv(prices_all, "data/prices_clean/all_prices_clean.csv")

message("✅ Cleaned prices saved to data/prices_clean/all_prices_clean.csv")


#  checks


aapl_clean %>% 
  arrange(Date) %>% 
  slice(1:10)

aapl_clean %>% 
  arrange(desc(Date)) %>% 
  slice(1:10)
