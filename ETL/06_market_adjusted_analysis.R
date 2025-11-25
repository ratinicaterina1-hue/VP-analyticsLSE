library(tidyverse)
library(lubridate)

# ---------- 1. LOAD RAW EVENT WINDOWS ----------
events_all_raw <- read_csv("data/merged/all_tickers_event_windows.csv",
                           show_col_types = FALSE)

# ---------- 2. LOAD SP500 RETURNS ----------
sp500 <- read_csv("data/sp500/sp500_prices.csv",
                  show_col_types = FALSE) %>%
  select(Date, SP500_Return = Return)

# ---------- 3. MERGE & COMPUTE MARKET-ADJUSTED RETURNS ----------
events_all_mktadj <- events_all_raw %>%
  left_join(sp500, by = "Date") %>%
  mutate(
    Mkt_Adj_Return = Return - SP500_Return
  )

# (Optional) save a separate CSV with the market-adjusted column
dir.create("data/merged", showWarnings = FALSE)
write_csv(events_all_mktadj,
          "data/merged/all_tickers_event_windows_mktadj.csv")

message("✅ Saved market-adjusted event windows to data/merged/all_tickers_event_windows_mktadj.csv")

# ---------- 4. EVENT STUDY USING MARKET-ADJUSTED RETURNS ----------

# 4a. Average market-adjusted daily returns around earnings
avg_event_curve_adj <- events_all_mktadj %>%
  group_by(Ticker, Days_From_Earnings) %>%
  summarise(
    avg_mkt_adj_return = mean(Mkt_Adj_Return, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(avg_event_curve_adj,
       aes(Days_From_Earnings, avg_mkt_adj_return, color = Ticker)) +
  geom_line(size = 1.1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Market-Adjusted Daily Return Around Earnings",
    x = "Days From Earnings",
    y = "Average Market-Adjusted Return"
  ) +
  theme_minimal()

# 4b. Average cumulative market-adjusted returns
cumulative_adj <- events_all_mktadj %>%
  arrange(Ticker, Earnings_Date, Date) %>%
  group_by(Ticker, Earnings_Date) %>%
  mutate(
    cum_mkt_adj_return = cumsum(replace_na(Mkt_Adj_Return, 0))
  ) %>%
  ungroup()

avg_cum_adj <- cumulative_adj %>%
  group_by(Ticker, Days_From_Earnings) %>%
  summarise(
    avg_cum_mkt_adj_return = mean(cum_mkt_adj_return),
    .groups = "drop"
  )

ggplot(avg_cum_adj,
       aes(Days_From_Earnings, avg_cum_mkt_adj_return, color = Ticker)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Market-Adjusted Cumulative Return Around Earnings",
    x = "Days From Earnings",
    y = "Cumulative Market-Adjusted Return"
  ) +
  theme_minimal()

# 4c. EPS surprise vs market-adjusted earnings-day return
earnings_day <- events_all_mktadj %>%
  filter(Days_From_Earnings == 0)

model_mkt_adj <- lm(Mkt_Adj_Return ~ EPS_Surprise, data = earnings_day)
print(summary(model_mkt_adj))

earnings_day %>%
  group_by(Ticker) %>%
  summarise(
    cor_eps_mkt_adj = cor(EPS_Surprise, Mkt_Adj_Return, use = "complete.obs"),
    .groups = "drop"
  ) %>%
  print()

ggplot(earnings_day,
       aes(EPS_Surprise, Mkt_Adj_Return, color = Ticker)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "EPS Surprise vs. Market-Adjusted Earnings-Day Return",
    x = "EPS Surprise (Actual - Estimate)",
    y = "Market-Adjusted Return on Day 0"
  ) +
  theme_minimal()
