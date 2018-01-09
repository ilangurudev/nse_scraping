pacman::p_load(tidyverse, rvest, lubridate)


all_data <- read_csv("all_data.csv")

filter_date <- today()

all_data <- 
  all_data %>% 
  filter(date <= filter_date)

current_month <- month(filter_date)
current_year <- year(filter_date)
current_quarter <- quarter(filter_date)

# volume 200%

# todos
# DONE: previous month (from today) - don't hardcode
# DONE: current day of this month's close > last day of prev's close
vol_200p_month <- 
  all_data %>% 
  filter(if(current_month == 1){
    ((month == 1 & year == current_year) | (month == 12 & year == (current_year - 1)))
  } else {
    (year == current_year & between(month, current_month - 1, current_month))
  }) %>% 
  group_by(symbol, isin, year, month) %>% arrange(symbol, isin, year, month, day(date)) %>% 
  summarise(month_val = sum(close * volume), 
            volume = sum(as.numeric(volume)), 
            last_close = last(close)) %>%
  ungroup() %>% distinct() %>% group_by(isin) %>% arrange(symbol, year, month) %>% 
  mutate(volume_ratio = month_val/lag(month_val), 
         close_ratio = last_close/lag(last_close))  %>% 
  filter(!is.na(volume_ratio)) %>% 
  filter(volume_ratio > 2 & close_ratio >= 1 & month_val > 2500000)

# same filter as above repeated over quarter with volume criterion at 75 lakhs
vol_200p_quarter <- 
  all_data %>% 
  mutate(quarter = quarter(date)) %>% 
  filter(if(current_quarter == 1){
    ((quarter == 1 & year == current_year) | (quarter == 4 & year == (current_year - 1)))
  } else {
    (year == current_year & between(quarter, current_quarter - 1, current_quarter))
  }) %>% 
  group_by(symbol, isin, year, quarter) %>% arrange(symbol, isin, year, month, day(date)) %>% 
  summarise(quarter_val = sum(close * volume), 
            volume = sum(as.numeric(volume)), 
            last_close = last(close)) %>%
  ungroup() %>% distinct() %>% group_by(isin) %>% arrange(symbol, year, quarter) %>% 
  mutate(volume_ratio = quarter_val/lag(quarter_val), 
         close_ratio = last_close/lag(last_close))  %>% 
  filter(!is.na(volume_ratio)) %>% 
  filter(volume_ratio > 2 & close_ratio >= 1 & quarter_val > 7500000)

# 52week high
# current HIGH should be the highest in the last 12 months. 
# 25 lakh volume criterion
year_high <- 
  all_data %>% 
  group_by(symbol, isin) %>% 
  filter(date > max(date) - duration(52, units = "weeks")) %>%  #52 weeks different answer) 
  mutate(max_high = max(high),
         high_ratio = high/max_high) %>% 
  filter(date == max(date) & 
           high_ratio == 1 &
           year == current_year)  

shortlisted_isin <- 
  bind_rows(vol_200p_month, vol_200p_quarter, year_high) %>% 
    pull(isin) %>% unique()

shortlisted_stocks <- 
  all_data %>% 
  select(symbol, isin, exchange) %>% distinct() %>% 
  filter(isin %in% shortlisted_isin)

# join the three tables to get shortlisted stocks
write_csv(shortlisted_stocks, "results/shortlisted_stocks.csv")

rm(list = ls())
