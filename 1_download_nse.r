# if pacman not already installed
# install.packages("pacman")
pacman::p_load(httr, tidyverse, janitor, lubridate)

download_stock_data <- function(startDate, endDate, exchange = c("nse", "bse")){
  #work with date, month, year for which data has to be extracted
  myDate = startDate
  zippedFile <- tempfile() 
  
  while (myDate <= endDate){
    filenameDate = paste(as.character(myDate, "%y%m%d"), ".csv", sep = "")
    monthfilename=paste(as.character(myDate, "%y%m"),".csv", sep = "")
    downloadfilename=paste("cm", toupper(as.character(myDate, "%d%b%Y")), "bhav.csv", sep = "")
    temp =""
    
    #Generate URL
    
    if(exchange == "nse"){
      url <- paste("https://www.nseindia.com/content/historical/EQUITIES/", as.character(myDate, "%Y"), "/", toupper(as.character(myDate, "%b")), "/", downloadfilename, ".zip", sep = "")
    } else{
      url <- paste0("www.bseindia.com/download/BhavCopy/Equity/EQ",format(myDate, "%d%m%y"),"_CSV.ZIP")
    }
    
    #retrieve Zipped file
    tryCatch({
      #Download Zipped File
      
      #28-10-2014: Fix for '403 Forbidden'
      #download.file(myURL,zippedFile, quiet=TRUE, mode="wb",cacheOK=TRUE)
      GET(url, user_agent("Mozilla/5.0"), write_disk(paste(downloadfilename,".zip",sep="")))
      
      
      #Unzip file and save it in temp 
      #28-10-2014: Fix for '403 Forbidden'
      temp <- read.csv(unzip(paste(downloadfilename,".zip",sep="")), sep = ",") 
      temp$exchange <- exchange 
      if(exchange == "nse"){
        temp$X <- NULL
      } else{
        temp$timestamp <- myDate
      }
      
      
      #Write the BHAVCOPY csv - datewise
      write_csv(temp,path=paste0("data/", exchange, "/", filenameDate))
      message(myDate)
  
    }, error=function(err){
      #print(paste(myDate, "-No Record"))
    }
    )
    myDate <- myDate+1
    print(paste(myDate, "Next Record"))
  }
  
  junk <- dir(pattern = "csv")
  file.remove(junk)
  junk <- dir(pattern = "CSV")
  file.remove(junk)
  
  all_data_files <- list.files(path = paste0("data/", exchange),pattern = "[0-9]+.csv") %>% 
    paste0("data/", exchange, "/", .)
  all_data <- map_df(all_data_files, read_csv)
  
  if(exchange == "bse"){
    all_data <- 
      all_data %>% 
      rename(symbol = SC_NAME,
             isin = SC_CODE,
             series = SC_TYPE,
             tottrdqty = NO_OF_SHRS) %>% 
      mutate(series = if_else(series == "Q", "EQ", series),
             timestamp = format(timestamp, "%d-%m-%Y"))
  }
  
  all_data <- 
    all_data %>%
    clean_names() %>%
    rename(volume = tottrdqty,
           date = timestamp) %>% 
    mutate(date = dmy(date), 
           year = year(date), 
           month = month(date),
           day = day(date),
           ym = str_c(year, str_pad(month, width = 2, pad = "0"))) %>% 
    filter(series == "EQ") %>% 
    select(symbol, isin, exchange, date, open, high, low, close, volume, year, month, day, ym)
  
  write_csv(all_data, paste0("data/all_data_",exchange, ".csv"))
  
  all_data
}

update_stocks <- function(exchange = c("nse", "bse")){
  
  if(!paste0("all_data_", exchange, ".csv") %in% list.files(path = "data")){
    # Define start and end dates, and convert them into date format
    
    endDate <- today()
    startDate <- endDate - years(2)
    
    download_stock_data(startDate, endDate, exchange)
    
  } else {
    
    #EOD updation
    message("updating daily stock data")
    all_data_exchange <- read_csv(paste0("data/all_data_", exchange, ".csv"))
    
    if(max(all_data_exchange$date) < today()){
      
      startDate = max(all_data_exchange$date) + 1
      endDate = today()
      
      download_stock_data(startDate, endDate, exchange)
      
    } else{
      message("Already up to date")
    }
    
  } 
}

bse <- update_stocks("bse")
nse <- update_stocks("nse")

 
bind_rows(nse, 
          bse %>% mutate(isin = isin %>% as.character())) %>% 
write_csv("all_data.csv")

rm(list = ls())
message("-----------------------Updation complete---------------------------")