library(tidyverse)
library(lubridate)

prices_dir <- "data/prices"

# ================================================================
# Clean price files exported with 2 metadata rows at the top
# ================================================================
clean_prices <- function(filename, ticker_label = NULL) {
  
  path <- file.path(prices_dir, filename)
  message("Cleaning: ", path)
  
  # read and coerce to tibble to ensure dplyr methods work
  raw <- readr::read_csv(path, show_col_types = FALSE) %>% as_tibble()
  
  # remove first two metadata rows and rename Price → Date
  df <- raw %>%
    dplyr::slice(-c(1,2)) %>%
    dplyr::rename(Date = Price)
  
  # type conversion
  df <- df %>%
    mutate(
      Date   = as.Date(Date),
      Close  = as.numeric(Close),
      High   = as.numeric(High),
      Low    = as.numeric(Low),
      Open   = as.numeric(Open),
      Volume = as.numeric(Volume)
    ) %>%
    arrange(Date) %>%
    mutate(
      Return = Close / lag(Close) - 1,
      Ticker = ticker_label
    )
  
  df
}

# ================================================================
# Apply cleaning to NVDA / AAPL / GOOGL
# ================================================================
nvda_clean  <- clean_prices("nvda_prices_2015_2025.csv",  "NVDA")
aapl_clean  <- clean_prices("aapl_prices_2015_2025.csv",  "AAPL")
googl_clean <- clean_prices("googl_prices_2015_2025.csv", "GOOGL")

prices_all <- bind_rows(nvda_clean, aapl_clean, googl_clean)

# Save output
dir.create("data/prices_clean", showWarnings = FALSE)
write_csv(prices_all, "data/prices_clean/all_prices_clean.csv")

message("✅ Cleaned prices saved to data/prices_clean/all_prices_clean.csv")

# quick sanity checks
print(head(nvda_clean))
print(head(aapl_clean))
print(head(googl_clean))
