
filter(date > ymd(20171130)) %>% 
  group_by(symbol, ym) %>% 
  summarise(month_val = sum(close * volume)) %>% 
  ungroup() %>% group_by(symbol) %>% 
  mutate(volume_ratio = month_val/lag(month_val))  %>% 
  filter(!is.na(volume_ratio)) %>% 
  filter(volume_ratio > 2)

