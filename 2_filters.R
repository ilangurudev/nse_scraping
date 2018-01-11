pacman::p_load(tidyverse, rvest, lubridate, rebus)

#replace today with the date in quotes as in ymd( "2018-01-10" )
backtrack_date <- ymd(today())

all_data <- read_csv("all_data.csv")

#provision for date filter
max_date <- max(all_data$date)

filter_date <- min(backtrack_date, today(), max_date)

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
(vol_200p_month <- 
  all_data %>% 
  filter(if(current_month == 1){
    ((month == 1 & year == current_year) | (month == 12 & year == (current_year - 1)))
  } else {
    (year == current_year & between(month, current_month - 1, current_month))
  }) %>% 
  group_by(symbol, isin, year, month, exchange) %>% arrange(symbol, isin, year, month, day(date)) %>% 
  summarise(month_val = sum(close * volume), 
            volume = sum(as.numeric(volume)), 
            last_close = last(close)) %>%
  ungroup() %>% distinct() %>% group_by(isin) %>% arrange(symbol, year, month) %>% 
  mutate(volume_ratio = month_val/lag(month_val), 
         close_ratio = last_close/lag(last_close),
         filter = "vol200p_month")  %>% 
  filter(!is.na(volume_ratio)) %>% 
  filter(volume_ratio > 2 & close_ratio >= 1 & month_val > 2500000))

# same filter as above repeated over quarter with volume criterion at 75 lakhs
(vol_200p_quarter <- 
  all_data %>% 
  mutate(quarter = quarter(date)) %>% 
  filter(if(current_quarter == 1){
    ((quarter == 1 & year == current_year) | (quarter == 4 & year == (current_year - 1)))
  } else {
    (year == current_year & between(quarter, current_quarter - 1, current_quarter))
  }) %>% 
  group_by(symbol, isin, year, quarter, exchange) %>% arrange(symbol, isin, year, month, day(date)) %>% 
  summarise(quarter_val = sum(close * volume), 
            volume = sum(as.numeric(volume)), 
            last_close = last(close)) %>%
  ungroup() %>% distinct() %>% group_by(isin) %>% arrange(symbol, year, quarter) %>% 
  mutate(volume_ratio = quarter_val/lag(quarter_val), 
         close_ratio = last_close/lag(last_close),
         filter = "vol200p_quarter")  %>% 
  filter(!is.na(volume_ratio)) %>% 
  filter(volume_ratio > 2 & close_ratio >= 1 & quarter_val > 7500000))

# 52week high
# current HIGH should be the highest in the last 12 months. 
# 25 lakh volume criterion
(year_high <- 
  all_data %>% 
  group_by(symbol, isin, exchange) %>% 
  filter(date > max(date) - duration(52, units = "weeks")) %>%  #52 weeks different answer) 
  mutate(max_high = max(high),
         high_ratio = high/max_high,
         filter = "52_week_high") %>% 
  filter(date == max(date) & 
           high_ratio == 1 &
           year == current_year))  

##exclude already checked stocks based on certain params

  
candidate_metric_files <- function(unit = c("quarter", "month")){
  
  filter_months <- if(unit == "month"){
    current_month 
  } else {
    (current_quarter*3 - 2):(current_quarter*3)
  } 
  
  filter_months <- filter_months %>% str_pad(2, "left", "0")
  
  #candiate_metric_files
  all_shortlisted_files <- list.files(path = "results", pattern = "shortlisted_stocks_")
  
  if(length(all_shortlisted_files) > 1){
    
    all_shortlisted_files <- 
      all_shortlisted_files %>% 
      str_subset(START %R% 
                   "shortlisted_stocks_" %R% 
                   current_year %R% 
                   or1(filter_months) %R% 
                   DIGIT %R% 
                   DIGIT %R%
                   DOT %R%
                   "csv"%R%
                   END)
    
    shortlisted_dates_chrs <- 
      all_shortlisted_files %>% 
      str_extract(one_or_more(DIGIT)) 
    
    shortlisted_dates <- shortlisted_dates_chrs[ymd(shortlisted_dates_chrs) < filter_date]
    
    if(length(shortlisted_dates) == 0){
      "results/shortlisted_stocks_empty.csv"
    } else {
      shortlisted_dates %>% str_c("results/shortlisted_stocks_",.,".csv")
    }
    
  } else {
    "results/shortlisted_stocks_empty.csv"
    # tibble(symbol = NA, isin = NA, exchange = NA, filter = NA, date = NA) %>%
    #   write_csv("results/shortlisted_stocks_empty.csv")
  }
}

#eliminate _prev_quarter_data
  
all_quarter_data <- 
 map_df(candidate_metric_files("quarter"), read_csv) %>% 
 filter(filter == "vol200p_quarter")  

vol_200p_quarter <- 
  vol_200p_quarter %>% 
    anti_join(all_quarter_data, by = "isin")

all_month_data <-  
  map_df(candidate_metric_files("month"), read_csv) %>% 
  filter(filter != "vol200p_quarter")  

vol_200p_month <- 
  vol_200p_month %>% 
  anti_join(all_month_data, by = "isin")

year_high <- 
  year_high %>% 
  anti_join(all_month_data, by = "isin")


volume_p <- 
  anti_join(vol_200p_month, 
            vol_200p_quarter, 
            by = "isin") %>% 
  bind_rows(vol_200p_quarter) 

# stocks prioritised as quarter, month and year_high
shortlisted_stocks <- 
  anti_join(year_high, volume_p, by = "isin") %>% 
  bind_rows(volume_p)

shortlisted_stocks <- 
  shortlisted_stocks %>% 
  anti_join(map_df(candidate_metric_files("month"), read_csv), by = "isin") 

(shortlisted_stocks <-
  shortlisted_stocks %>%
  arrange(symbol) %>% 
  select(symbol, isin, exchange, filter) %>%
  mutate(date = filter_date))
 
# shortlisted_isin <-
#   bind_rows(vol_200p_month, vol_200p_quarter, year_high) %>%
#     pull(isin) %>% unique()
# 
# shortlisted_stocks <-
#   all_data %>%
#   select(symbol, isin, exchange) %>% distinct() %>%
#   filter(isin %in% shortlisted_isin) %>%
#   mutate(date = filter_date)

# join the three tables to get shortlisted stocks
write_csv(shortlisted_stocks, 
          str_c("results/shortlisted_stocks_", filter_date %>% format("%Y%m%d"), ".csv"))

rm(list = ls())

