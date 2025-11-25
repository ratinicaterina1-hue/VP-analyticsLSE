library(tidyverse)
library(lubridate)

# Load merged event-window data
events_all <- read_csv("data/merged/all_tickers_event_windows.csv",
                       show_col_types = FALSE)

# ============================================
# 1. BASIC EVENT STUDY: AVERAGE RETURNS BY DAY
# ============================================

avg_event_curve <- events_all %>%
  group_by(Ticker, Days_From_Earnings) %>%
  summarise(
    avg_return = mean(Return, na.rm = TRUE),
    .groups = "drop"
  )

# Plot: Average return around earnings
ggplot(avg_event_curve, aes(Days_From_Earnings, avg_return, color = Ticker)) +
  geom_line(size = 1.1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Average Daily Return Around Earnings",
    x = "Days From Earnings",
    y = "Average Return"
  ) +
  theme_minimal()


# ============================================
# 2. CUMULATIVE RETURNS
# ============================================

cumulative <- events_all %>%
  arrange(Ticker, Earnings_Date, Date) %>%
  group_by(Ticker, Earnings_Date) %>%
  mutate(
    cum_return = cumsum(replace_na(Return, 0))
  ) %>%
  ungroup()

avg_cum <- cumulative %>%
  group_by(Ticker, Days_From_Earnings) %>%
  summarise(
    avg_cum_return = mean(cum_return),
    .groups = "drop"
  )

# Plot: Average cumulative return
ggplot(avg_cum, aes(Days_From_Earnings, avg_cum_return, color = Ticker)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Average Cumulative Return Around Earnings",
    x = "Days From Earnings",
    y = "Cumulative Return"
  ) +
  theme_minimal()


# ============================================
# 3. DOES EPS SURPRISE PREDICT RETURNS?
# (the core hedge-fund question)
# ============================================

earnings_day <- events_all %>%
  filter(Days_From_Earnings == 0)

model <- lm(Return ~ EPS_Surprise, data = earnings_day)

summary(model)

# Also check R² per company
earnings_day %>%
  group_by(Ticker) %>%
  summarise(
    cor_surprise_return = cor(EPS_Surprise, Return, use = "complete.obs")
  )


# ============================================
# 4. SCATTERPLOT: EPS Surprise vs Earnings-Day Return
# ============================================

ggplot(earnings_day, aes(EPS_Surprise, Return, color = Ticker)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "EPS Surprise vs. Earnings-Day Return",
    x = "EPS Surprise (Actual - Estimate)",
    y = "Return on Day 0"
  ) +
  theme_minimal()
