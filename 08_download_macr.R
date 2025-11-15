library(quantmod)
library(tidyverse)
library(lubridate)

# Where to save data
dir.create("data/macro", showWarnings = FALSE)

start_date <- "2010-01-01"   # long enough for your 2015–2025 window

get_macro <- function(symbol, name) {
  tryCatch({
    data <- getSymbols(symbol, src = "FRED", auto.assign = FALSE)
    df <- data.frame(
      Date = index(data),
      Value = as.numeric(data[, 1])
    )
    write_csv(df, paste0("data/macro/", name, ".csv"))
    message("✔ Saved: ", name)
  },
  error = function(e) {
    message("❌ Error downloading ", name, ": ", e$message)
  })
}

# Download macro series
get_macro("CPIAUCSL", "cpi")
get_macro("FEDFUNDS", "fedfunds")
get_macro("VIXCLS", "vix")
get_macro("UNRATE", "unemployment")
get_macro("GDP", "gdp")

message("🎉 Finished downloading all macro data.")
