library(tidyverse)
library(lubridate)

events <- read_csv("data/merged/event_windows_macro.csv",
                   show_col_types = FALSE)

# ---- A. Pre-earnings drift by macro regime -----------------------

pre_drift <- events %>%
  filter(Days_From_Earnings %in% -5:-1) %>%
  group_by(Ticker, CPI_regime) %>%
  summarise(
    avg_pre5 = mean(Mkt_Adj_Return, na.rm = TRUE) * 5,
    .groups = "drop"
  )

print(pre_drift)

ggplot(pre_drift,
       aes(CPI_regime, avg_pre5, fill = CPI_regime)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~Ticker) +
  labs(
    title = "Pre-Earnings Drift in High vs Low Inflation Regimes",
    y = "Cumulative 5-Day Market-Adjusted Return",
    x = "Inflation Regime"
  ) +
  theme_minimal()

# ---- B. Earnings-day return by VIX regime ------------------------

day0_vix <- events %>%
  filter(Days_From_Earnings == 0) %>%
  group_by(Ticker, VIX_regime) %>%
  summarise(
    avg_day0 = mean(Mkt_Adj_Return, na.rm = TRUE),
    .groups = "drop"
  )

print(day0_vix)

ggplot(day0_vix,
       aes(VIX_regime, avg_day0, fill = VIX_regime)) +
  geom_bar(stat="identity") +
  facet_wrap(~Ticker) +
  labs(
    title = "Earnings-Day Alpha in High vs Low Volatility Regimes",
    y = "Market-Adjusted Return on Day 0"
  ) +
  theme_minimal()

# ---- C. Regression: Do macro vars explain returns? --------------

model_macro <- lm(
  Mkt_Adj_Return ~ CPI + FedFunds + VIX + Unemployment + Ticker + Days_From_Earnings,
  data = events %>% filter(abs(Days_From_Earnings) <= 5)
)

print(summary(model_macro))
