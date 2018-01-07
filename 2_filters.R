pacman::p_load(tidyverse, rvest, lubridate)


all_data <- read_csv("all_data.csv")

today_ratio <- 
  all_data %>% 
  filter(series == "EQ") %>% 
  filter(date > ymd(20171130)) %>% 
    group_by(symbol,isin, year, month) %>% 
    summarise(month_val = sum(close * volume)) %>% 
    ungroup() %>% group_by(symbol) %>% arrange(year, month) %>% 
    mutate(volume_ratio = month_val/lag(month_val))  %>% 
    filter(!is.na(volume_ratio)) %>% 
    filter(volume_ratio > 2)

write_csv(today_ratio, "today_ratio.csv")
