#28-10-2014: Fix for '403 Forbidden'
## Credit http://stackoverflow.com/questions/26086868/error-downloading-a-csv-in-zip-from-website-with-get-in-r

# if pacman not already installed
# install.packages("pacman")
pacman::p_load(httr, tidyverse, janitor)


if(!"all_data.csv" %in% list.files()){
  # Define start and end dates, and convert them into date format
  startDate = as.Date("2017-11-01", order="ymd")
  endDate = as.Date(today(), order="ymd")
  
  #work with date, month, year for which data has to be extracted
  myDate = startDate
  zippedFile <- tempfile() 
  
  while (myDate <= endDate){
    filenameDate = paste(as.character(myDate, "%y%m%d"), ".csv", sep = "")
    monthfilename=paste(as.character(myDate, "%y%m"),".csv", sep = "")
    downloadfilename=paste("cm", toupper(as.character(myDate, "%d%b%Y")), "bhav.csv", sep = "")
    temp =""
    
    #Generate URL
    
    myURL = paste("https://www.nseindia.com/content/historical/EQUITIES/", as.character(myDate, "%Y"), "/", toupper(as.character(myDate, "%b")), "/", downloadfilename, ".zip", sep = "")
    
    #retrieve Zipped file
    tryCatch({
      #Download Zipped File
      
      #28-10-2014: Fix for '403 Forbidden'
      #download.file(myURL,zippedFile, quiet=TRUE, mode="wb",cacheOK=TRUE)
      GET(myURL, user_agent("Mozilla/5.0"), write_disk(paste(downloadfilename,".zip",sep="")))
      
      
      #Unzip file and save it in temp 
      #28-10-2014: Fix for '403 Forbidden'
      temp <- read.csv(unzip(paste(downloadfilename,".zip",sep="")), sep = ",") 
      
      temp$X <- NULL
      
      
      #Write the BHAVCOPY csv - datewise
      write_csv(temp,path=paste0("data/",filenameDate))
      
      # #Write the csv in Monthly file
      # if (file.exists(monthfilename))
      # {
      # 	write.table(temp,file=monthfilename,sep=",", eol="\n", row.names = FALSE, col.names = FALSE, append=TRUE)
      # }else
      # {
      # 	write.table(temp,file=monthfilename,sep=",", eol="\n", row.names = FALSE, col.names = TRUE, append=FALSE)
      # }
      
      
      #Print Progress
      #print(paste (myDate, "-Done!", endDate-myDate, "left"))
    }, error=function(err){
      #print(paste(myDate, "-No Record"))
    }
    )
    myDate <- myDate+1
    print(paste(myDate, "Next Record"))
  }
  
  junk <- dir(pattern = "csv")
  file.remove(junk)
  
  all_data_files <- list.files(path = "data", pattern = "[0-9]+.csv") %>% paste0("data/", .)
  all_data <- map_df(all_data_files, read_csv)
  
  all_data <- 
    all_data %>%
    clean_names() %>%
    rename(volume = tottrdqty,
           date = timestamp) %>% 
    mutate(date = dmy(date), 
           year = year(date), 
           month = month(date),
           day = day(date),
           ym = str_c(year, str_pad(month, width = 2, pad = "0")))
  
  write_csv(all_data, "all_data.csv")
  
} else {
  
  #EOD updation
  all_data <- read_csv("all_data.csv")
  
  if(max(all_data$date) < ymd(today())){
    print("yes")
  } else{
    print("no")
  }
  
}