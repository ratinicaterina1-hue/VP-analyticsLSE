# scripts/11_build_features.R
# Run after ETL. Produces: data/features/events_features.csv

library(tidyverse)
library(lubridate)
library(zoo)

# Paths (use project `paths` if available)
if (exists("paths")) {
  merged_path <- file.path(paths$merged, "event_windows_macro.csv")
  out_dir     <- file.path(paths$outputs, "features")
} else {
  merged_path <- "data/merged/event_windows_macro.csv"
  out_dir     <- "data/features"
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

message("Loading merged event windows from: ", merged_path)

# --- LOAD EVENTS AS PLAIN TIBBLE (defensive against spec_tbl_df) ---
events <- readr::read_csv(merged_path, show_col_types = FALSE) %>%
  as_tibble() %>%
  mutate(
    Date = as.Date(Date),
    Earnings_Date = as.Date(Earnings_Date)
  ) %>%
  as_tibble() # ensure we are a plain tibble

# quick check
if (!("Ticker" %in% names(events))) stop("events missing Ticker column - check merged input")

# Precompute a (clean) price-like table if present in events
price_tbl <- events %>%
  select(any_of(c("Ticker", "Date", "Close", "Return", "Volume", "Mkt_Adj_Return"))) %>%
  distinct() %>%
  as_tibble()

# safe helpers
safe_first <- function(x) if (length(x) >= 1) x[[1]] else NA_real_
safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

# compute features for one event
compute_event_features <- function(ticker, edate) {
  # restrict to ticker
  p <- events %>% filter(Ticker == ticker) %>% arrange(Date) %>% as_tibble()
  # event-row(s) for this earnings date
  e <- p %>% filter(Earnings_Date == edate) %>% as_tibble()
  
  if (nrow(e) == 0) {
    return(tibble(
      Ticker = ticker,
      Earnings_Date = edate,
      pre1 = NA_real_, pre3 = NA_real_, pre5 = NA_real_,
      vol10 = NA_real_, vol_ratio = NA_real_,
      EPS_Surprise = NA_real_, Surprise_Pct = NA_real_,
      VIX = NA_real_, CPI = NA_real_, FedFunds = NA_real_,
      ret1 = NA_real_, ret5 = NA_real_,
      weekday = wday(edate, label = TRUE, abbr = TRUE),
      quarter = quarter(edate)
    ))
  }
  
  # ---- PRE WINDOW: last up-to-10 calendar days before edate (from available rows) ----
  pre_all <- p %>%
    filter(Date < edate & Date >= edate - days(10)) %>%
    arrange(Date) %>%
    as_tibble()
  
  if (nrow(pre_all) == 0) {
    pre1 <- 0
    pre3 <- 0
    pre5 <- 0
    vol10 <- 0
    vol_ratio <- 0
  } else {
    # use Mkt_Adj_Return when available, otherwise fallback to Return or NA
    rvec <- if ("Mkt_Adj_Return" %in% names(pre_all)) pre_all$Mkt_Adj_Return else (pre_all$Return %||% rep(NA_real_, nrow(pre_all)))
    rvec <- as.numeric(rvec)
    
    pre1 <- ifelse(length(rvec) >= 1, tail(rvec, 1), NA_real_)
    pre3 <- ifelse(length(rvec) >= 1, sum(tail(rvec, min(3, length(rvec))), na.rm = TRUE), NA_real_)
    pre5 <- ifelse(length(rvec) >= 1, sum(tail(rvec, min(5, length(rvec))), na.rm = TRUE), NA_real_)
    vol10 <- ifelse(length(rvec) >= 2, sd(tail(rvec, min(10, length(rvec))), na.rm = TRUE), NA_real_)
    # volume ratio: use e$Volume vs avg prior volume if available
    vol_today <- safe_first(p %>% filter(Date == edate) %>% pull(Volume))
    avg_vol  <- if (nrow(pre_all) >= 1) mean(pre_all$Volume, na.rm = TRUE) else NA_real_
    vol_ratio <- if (is.na(vol_today) || is.na(avg_vol) || avg_vol == 0) NA_real_ else vol_today / avg_vol
    # fallback vol_ratio using market-adj move if available
    if (is.na(vol_ratio) && "Mkt_Adj_Return" %in% names(e)) {
      vol_ratio <- abs(safe_first(e$Mkt_Adj_Return)) / (vol10 + 1e-6)
    }
    # final defensive numeric casts
    pre1 <- as.numeric(pre1); pre3 <- as.numeric(pre3); pre5 <- as.numeric(pre5); vol10 <- as.numeric(vol10); vol_ratio <- as.numeric(vol_ratio)
  }
  
  # ---- POST WINDOW (labels): next 1..5 calendar days ----
  post_all <- p %>%
    filter(Date > edate & Date <= edate + days(5)) %>%
    arrange(Date) %>%
    as_tibble()
  
  if (nrow(post_all) == 0) {
    ret1 <- NA_real_
    ret5 <- NA_real_
  } else {
    r_post <- if ("Mkt_Adj_Return" %in% names(post_all)) post_all$Mkt_Adj_Return else (post_all$Return %||% rep(NA_real_, nrow(post_all)))
    r_post <- as.numeric(r_post)
    ret1 <- ifelse(length(r_post) >= 1, r_post[1], NA_real_)
    ret5 <- ifelse(length(r_post) >= 1, sum(head(r_post, min(5, length(r_post))), na.rm = TRUE), NA_real_)
    if (all(is.na(r_post))) { ret1 <- NA_real_; ret5 <- NA_real_ }
  }
  
  # macro / surprise values from event row (take first if duplicates)
  EPS_Surprise <- safe_first(e$EPS_Surprise %||% NA_real_)
  Surprise_Pct <- safe_first(e$Surprise_Pct %||% NA_real_)
  VIX         <- safe_first(e$VIX %||% NA_real_)
  CPI         <- safe_first(e$CPI %||% NA_real_)
  FedFunds    <- safe_first(e$FedFunds %||% NA_real_)
  
  tibble(
    Ticker = ticker,
    Earnings_Date = edate,
    pre1 = pre1,
    pre3 = pre3,
    pre5 = pre5,
    vol10 = vol10,
    vol_ratio = vol_ratio,
    EPS_Surprise = EPS_Surprise,
    Surprise_Pct = Surprise_Pct,
    VIX = VIX,
    CPI = CPI,
    FedFunds = FedFunds,
    ret1 = ret1,
    ret5 = ret5,
    weekday = wday(edate, label = TRUE, abbr = TRUE),
    quarter = quarter(edate)
  )
}

# Build the features dataset by iterating unique events
events_unique <- events %>%
  distinct(Ticker, Earnings_Date) %>%
  arrange(Ticker, Earnings_Date) %>%
  as_tibble()

message("Computing features for ", nrow(events_unique), " events. This may take a moment...")

# use pmap_dfr but ensure the inputs are plain vectors
features_list <- pmap_dfr(
  list(as.character(events_unique$Ticker), as.Date(events_unique$Earnings_Date)),
  compute_event_features
)

# Create target variables and clean
features_final <- features_list %>%
  mutate(
    label_ret1 = ret1,
    label_ret5 = ret5,
    label_dir1 = case_when(!is.na(label_ret1) & label_ret1 > 0 ~ 1,
                           !is.na(label_ret1) & label_ret1 <= 0 ~ 0,
                           TRUE ~ NA_real_),
    label_dir5 = case_when(!is.na(label_ret5) & label_ret5 > 0 ~ 1,
                           !is.na(label_ret5) & label_ret5 <= 0 ~ 0,
                           TRUE ~ NA_real_)
  ) %>%
  arrange(Ticker, Earnings_Date)

# Ensure required engineered columns exist for downstream recipe/backtests
if (!"vol_ratio" %in% colnames(features_final)) features_final <- features_final %>% mutate(vol_ratio = 0)
if (!"log_vol_ratio" %in% colnames(features_final)) features_final <- features_final %>% mutate(log_vol_ratio = log1p(replace_na(vol_ratio, 0)))

# Save output
out_path <- file.path(out_dir, "events_features.csv")
readr::write_csv(features_final, out_path)
message("✅ Features saved to: ", out_path)
glimpse(features_final)
