pacman::p_load(rvest, rebus)

extract_market_cap <- function(htm){
  htm %>% 
    html_node("#mktdet_2 .FL:nth-child(1) .brdb:nth-child(1) .gD_12") %>% 
    html_text() %>%
    str_replace_all(",", "") %>% 
    as.double()
}

stock_htmls <- 
today_ratio %>% 
  mutate(url = str_c("http://www.moneycontrol.com/stocks/cptmarket/compsearchnew.php?topsearch_type=1&search_str=",
                     isin),
         html = map(url, read_html))

stock_htmls <- 
  stock_htmls %>% 
    mutate(market_cap = map_dbl(html, extract_market_cap),
           url = if_else(is.na(market_cap), 
                         str_c("http://www.moneycontrol.com/stocks/cptmarket/compsearchnew.php?topsearch_type=1&search_str=", symbol), 
                         url),
           html = if_else(is.na(market_cap), 
                          map(url, read_html),
                          html),
           market_cap = map_dbl(html, extract_market_cap)
           ) 
stock_ratio_html <- 
  stock_htmls %>% ungroup() %>% 
  slice(1:3) %>% 
  mutate(ratios_url = map_chr(html, ~ html_node(., "td:nth-child(4) li:nth-child(9) a") %>% html_attr("href") ),
         ratios_url = str_c("http://www.moneycontrol.com", ratios_url),
         ratios_html = map(ratios_url, read_html))

# stock_ratio_cons_standalone <- 
stock_ratio_html %>% 
  mutate(ratios_url = map2_chr(ratios_html, ratios_url, function(x, y){
             ratio_href <- html_nodes(x, ".tabnsdn li+ li a") %>% html_attr("href")
             if(length(ratio_href) == 0){
               y
             } else {
               paste0("http://www.moneycontrol.com", ratio_href)
             }
            }),
         ratios_html = map(ratios_url, read_html),
         return_on_net_worth = map(ratios_html, ~ html_nodes(., "tr:nth-child(21) .det+ .det") %>% html_text())
    ) %>% View
         
#return on net worth
#yearly resuts > consolidated > Net Sales/Income from operations
#yearly resuts> consolidated > Net P/L After M.I & Associates	