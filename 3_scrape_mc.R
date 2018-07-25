pacman::p_load(rvest, tidyverse, rebus, lubridate)

#replace today with the date in quotes as in ymd( "2018-01-10" )
user_date <- ymd( today() )


#scraping delay in seconds (~ delay between one request and another)
scrap_delay_secs <- 0

available_dates <- 
  list.files(path = "results", pattern = "shortlisted_stocks_") %>% 
  str_extract(one_or_more(DIGIT)) %>% 
  ymd() %>% .[!is.na(.)]

max_available_date <- available_dates %>% max()

if(user_date %in% available_dates) {
  scrape_date <- user_date
} else{
  message("Shortlisted comapnies list not available for given date. 
          Using date closest to mentioned date as scrape date.")
  
  scrape_date <- 
    available_dates[abs(available_dates - user_date) == min(abs(available_dates - user_date))]
  
  message(str_c("scrape date: ", scrape_date))
}

message(str_c("Scraping for stocks filtered as on ", 
              scrape_date, 
              " with a scraping delay of ",
              scrap_delay_secs, 
              " seconds"))

shortlisted_stocks <- 
  read_csv(str_c("results/shortlisted_stocks_", scrape_date %>% format("%Y%m%d"), ".csv"))

read_html_safe <- function(...){
  Sys.sleep(scrap_delay_secs)
  read_html(...)
}

# function to extract market cap from the stock  page
extract_market_cap <- function(html){
  html %>% 
    html_node("#mktdet_2 .FL:nth-child(1) .brdb:nth-child(1) .gD_12") %>% 
    html_text() %>%
    str_replace_all(",", "") %>% 
    as.double()
}

make_landing_url <- function(param){
  url <- 
  "http://www.moneycontrol.com/stocks/cptmarket/compsearchnew.php?topsearch_type=1&search_str=" %>% 
    str_c(param) %>% 
    str_replace_all(" ", "")
  # %>%
    # url()
  curl::curl(url, handle = curl::new_handle("useragent" = "Mozilla/5.0"))
} 

test_landing <- function(html) {
  if(is.na(extract_market_cap(html))){
    return(NA)
  }else{
    return(html)
  }
}

fetch_page_and_test <- function(str){
  str %>% 
    make_landing_url() %>% 
    read_html_safe() %>% 
    test_landing()
} 

get_sc_html <- function(symbol, isin){
  # browser()
  message(str_c(symbol, "-", isin))
  
  html <- fetch_page_and_test(isin)
  if(!is.na(html)){
    message("downloaded")
    return(html)
  }
  closeAllConnections()
  
  
  for(i in seq(str_length(symbol),3)){
    html <- fetch_page_and_test(str_sub(symbol, 1, i))
    if(!is.na(html)){
      message("downloaded")
      return(html)
    }
    closeAllConnections()
  }
  
  message("------failed------")
  NA
}

# from the shortlisted stocks, use the search url and isin number to get the stock's url and read thpse pages. 
# has slice to test on first ten only. PLEASE REMOVE.  
(stock_htmls <- 
  shortlisted_stocks  %>% 
  arrange(symbol) %>%
  # slice(1:5) %>% 
  mutate(html = map2(symbol, isin, get_sc_html),
         market_cap = map_dbl(html, possibly(extract_market_cap, NA))))

message("Downloaded market cap and fetched landing page...")

# get the ratios url and html from the stock page
get_ratios_url <- function(html) html_node(html, "td:nth-child(4) li:nth-child(9) a") %>% html_attr("href")

message("Fetching the ratios standalone page...")

stock_ratio_html <- 
  stock_htmls %>%  
  mutate(standalone_ratios_url = 
           map_chr(html, 
                   possibly(get_ratios_url, NA)),
         standalone_ratios_url = str_c("http://www.moneycontrol.com", standalone_ratios_url),
         standalone_ratios_html = map(standalone_ratios_url, possibly(read_html_safe, NA)))

# stock_ratio_html <- stock_ratio_html %>% rename(standalone_ratios_url = ratios_url, standalone_ratios_html = ratios_html)

message("Fetching the ratios consolidated page...")

# get consolidated ratios url and html and replace ratios url and html 
# wherever consolidated is present. 
# Get the return on networth from the respective ratio page. 
extract_ratios_ron <- function(x){
  ratios <- html_nodes(x, "tr:nth-child(21) .det+ .det") %>% html_text() 
  if(length(ratios) == 0){
    NA
  } else{
    ratios
  }
}


get_consolidated_ratios_url <- function(x, y){
  if(!is.na(y)){
    ratio_href <- 
      html_nodes(x, ".tabnsdn li+ li a") %>% 
      html_attr("href") %>% 
      str_replace_all(" ", "")
    if(length(ratio_href) == 0){
      NA
    } else {
      paste0("http://www.moneycontrol.com", ratio_href)
    }
  } else {
    NA 
  }
}

message("Extracting the ratios from the fetched pages...")

stock_ratio_cons_standalone <- 
  stock_ratio_html %>% 
  mutate(consolidated_ratios_url = map2_chr(standalone_ratios_html, standalone_ratios_url, possibly(get_consolidated_ratios_url, NA)),
         consolidated_ratios_html = map(consolidated_ratios_url, possibly(read_html_safe, NA)),
         consolidated_return_on_net_worth = map(consolidated_ratios_html, possibly(extract_ratios_ron, NA)),
         standalone_return_on_net_worth = map(standalone_ratios_html, possibly(extract_ratios_ron, NA))) 

message("Fetching the yearly pages...")

# go to the appropriate financials page (consolidated wherever applicable) by tweaking ratios url
stock_financials <- 
  stock_ratio_cons_standalone %>% 
  mutate(standalone_financials_url = str_replace(standalone_ratios_url, "ratios", "results/yearly"),
         consolidated_financials_url = str_replace(consolidated_ratios_url, "consolidated-ratios", "results/consolidated-yearly"),
         standalone_financials_html = map(standalone_financials_url, possibly(read_html_safe, NA)),
         consolidated_financials_html = map(consolidated_financials_url, possibly(read_html_safe, NA)))

message("Extracting the yearly measures from the fetched pages...")

get_net_si_operations <- function(x){
  if(!is.na(x)){
    net_si_operation <- html_nodes(x, "tr:nth-child(5) .det+ .det") %>% html_text() 
    if(length(net_si_operation) == 0){
      NA
    } else {
      net_si_operation
    }
  } else {
    NA
  }
}

get_net_pl_standalone <- function(x){
  if(!is.na(x)){
    net <- html_nodes(x, "tr:nth-child(32) .det+ .det") %>% html_text() 
    if(length(net) == 0){
      NA
    } else{
      net
    }
  } else {
    NA
  }
}

get_net_pl_consolidated <- function(x){
  if(!is.na(x)){
    net <- html_nodes(x, "tr:nth-child(35) .det+ .det") %>% html_text() 
    if(length(net) == 0){
      NA
    } else{
      net
    }
  } else {
    NA
  }
}


# get the required metrics from the financial yearly pages
all_metrics_unformatted <- 
  stock_financials %>% 
  mutate(
    standalone_net_si_operations = map(standalone_financials_html, get_net_si_operations),
    consolidated_net_si_operations = map(consolidated_financials_html, get_net_si_operations),
    standalone_net_pl = map(standalone_financials_html, get_net_pl_standalone),
    consolidated_net_pl = map(consolidated_financials_html, get_net_pl_consolidated))

message("Formatting the output...")

collapse_format <- function(x){
  if(length(x) == 0){
    NA
  } else {
    str_c(x, collapse = " | ")
  }
}

sepf <- function(x) paste0(x, c("5","4","3","2","1"))

all_metrics <- 
  all_metrics_unformatted %>% 
  select(-contains("url"), -contains("html")) %>% 
  mutate(standalone_return_on_net_worth = map_chr(standalone_return_on_net_worth, collapse_format),
         consolidated_return_on_net_worth = map_chr(consolidated_return_on_net_worth, collapse_format),
         standalone_net_si_operations = map_chr(standalone_net_si_operations, collapse_format),
         standalone_net_pl = map_chr(standalone_net_pl, collapse_format),
         consolidated_net_si_operations = map_chr(consolidated_net_si_operations, collapse_format),
         consolidated_net_pl = map_chr(consolidated_net_pl, collapse_format),
         blank1 = "  ", 
         blank2 = blank1,
         blank3 = blank1,
         blank4 = blank1,
         blank5 = blank1,
         blank6 = blank1) %>% 
  separate(consolidated_return_on_net_worth, into = sepf("consolidated_RNW"), sep = rebus::literal(" | "), fill = "right") %>%
  separate(consolidated_net_si_operations, into = sepf("consolidated_SR"), sep = rebus::literal(" | "), fill = "right") %>%
  separate(consolidated_net_pl, into = sepf("consolidated_NP"), sep = rebus::literal(" | "), fill = "right") %>%
  separate(standalone_return_on_net_worth, into = sepf("standalone_RNW"), sep = rebus::literal(" | "), fill = "right") %>%
  separate(standalone_net_si_operations, into = sepf("standalone_SR"), sep = rebus::literal(" | "), fill = "right") %>%
  separate(standalone_net_pl, into = sepf("standalone_NP"), sep = rebus::literal(" | "), fill = "right") %>%
  select(symbol, 
         isin,
         filter,
         market_cap, 
         contains("consolidated_RNW"),
         blank1,
         contains("consolidated_SR"), 
         blank2,
         contains("consolidated_NP"),
         blank3,
         contains("standalone_RNW"),
         blank4,
         contains("standalone_SR"), 
         blank5,
         contains("standalone_NP"),
         blank6,
         date)

write_csv(all_metrics, 
          str_c("results/all_metrics_", scrape_date %>% format("%Y%m%d"), ".csv"))  

message("-----------------------Data Scraping Complete---------------------------")
