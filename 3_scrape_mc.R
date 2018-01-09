pacman::p_load(rvest, tidyverse, rebus)
shortlisted_stocks <- read_csv("results/shortlisted_stocks.csv")

# function to extract market cap from the stock  page
extract_market_cap <- function(html){
  html %>% 
    html_node("#mktdet_2 .FL:nth-child(1) .brdb:nth-child(1) .gD_12") %>% 
    html_text() %>%
    str_replace_all(",", "") %>% 
    as.double()
}

make_landing_url <- function(param){
  "http://www.moneycontrol.com/stocks/cptmarket/compsearchnew.php?topsearch_type=1&search_str=" %>% 
    str_c(param) %>% 
    str_replace_all(" ", "") %>% 
    url()
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
    read_html() %>% 
    test_landing()
} 

get_sc_html <- function(symbol, isin){
  message(str_c(symbol, "-", isin))
  for(i in seq(str_length(isin),3)){
    html <- fetch_page_and_test(str_sub(isin, 1, i))
    if(!is.na(html)){
      return(html)
    }
  }
  
  for(i in seq(str_length(symbol),3)){
    html <- fetch_page_and_test(str_sub(symbol, 1, i))
    if(!is.na(html)){
      return(html)
    }
  }
  
  NA
}

# from the shortlisted stocks, use the search url and isin number to get the stock's url and read thpse pages. 
# has slice to test on first ten only. PLEASE REMOVE.  
(stock_htmls <- 
  shortlisted_stocks %>% 
  ungroup() %>% 
  mutate(html = map2(symbol, isin, get_sc_html),
         market_cap = map_dbl(html, possibly(extract_market_cap, NA))))


# get the ratios url and html from the stock page
get_ratios_url <- function(html) html_node(html, "td:nth-child(4) li:nth-child(9) a") %>% html_attr("href")

stock_ratio_html <- 
  stock_htmls %>%  
  mutate(ratios_url = 
           map_chr(html, 
                   possibly(get_ratios_url, NA)),
         ratios_url = str_c("http://www.moneycontrol.com", ratios_url),
         ratios_html = map(ratios_url, possibly(read_html, NA)))

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


stock_ratio_cons_standalone <- 
stock_ratio_html %>% 
  mutate(ratios_url = map2_chr(ratios_html, ratios_url, function(x, y){
    if(!is.na(y)){
      ratio_href <- 
        html_nodes(x, ".tabnsdn li+ li a") %>% 
        html_attr("href") %>% 
        str_replace_all(" ", "")
      if(length(ratio_href) == 0){
        y
      } else {
        paste0("http://www.moneycontrol.com", ratio_href)
      }
    } else {
      NA 
    }
  }),
  ratios_html = map(ratios_url, possibly(read_html, NA)),
  return_on_net_worth = map(ratios_html, possibly(extract_ratios_ron, NA)),
  isStandalone = !str_detect(ratios_url, "consolidated")) 

# go to the appropriate financials page (consolidated wherever applicable) by tweaking ratios url
stock_financials <- 
  stock_ratio_cons_standalone %>% 
  mutate(financials_url = if_else(isStandalone,
                                  str_replace(ratios_url, "ratios", "results/yearly"),
                                  str_replace(ratios_url, "consolidated-ratios", "results/consolidated-yearly")
                                  ),
         financials_html = map(financials_url, possibly(read_html, NA)))


# get the required metrics from the financial yearly pages
all_metrics_unformatted <- 
  stock_financials %>% 
  mutate(
    net_si_operations = map(financials_html, function(x){
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
    }),
    net_pl = map2(financials_html, isStandalone, function(x, y){
      if(!is.na(x)){
        net <- html_nodes(x, ifelse(y, "tr:nth-child(32) .det+ .det", "tr:nth-child(35) .det+ .det")) %>% html_text() 
        if(length(net) == 0){
          NA
        } else{
          net
        }
      } else {
        NA
      }
    }))

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
  mutate(return_on_net_worth = map_chr(return_on_net_worth, collapse_format),
         net_si_operations = map_chr(net_si_operations, collapse_format),
         net_pl = map_chr(net_pl, collapse_format),
         blank1 = "  ", blank2 = blank1) %>% 
  separate(return_on_net_worth, into = sepf("RNW"), sep = rebus::literal(" | "), fill = "right") %>%
  separate(net_si_operations, into = sepf("SR"), sep = rebus::literal(" | "), fill = "right") %>%
  separate(net_pl, into = sepf("NP"), sep = rebus::literal(" | "), fill = "right") %>% 
  select(symbol, 
         market_cap, 
         starts_with("RNW"), 
         blank1,
         starts_with("SR"), 
         blank2,
         starts_with("NP"),
         isStandalone, 
         isin, 
         exchange)

write_csv(all_metrics, "results/all_metrics.csv")  

