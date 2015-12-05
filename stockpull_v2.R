## Stock returns reader
## Eamonn Gamble
## December 4, 2015

## Version Notes 1.2
## Split log function away from trade function
## Pulls info from Yahoo finance, creates csv with price history since purchase, logs trade details in another sheet
## Still issues with creating extra columns when logging a second transaction


rm( list=ls() )

setwd('C:/Users/Eamonn/Documents/GitHub/MereMarkets')

trade <- function(tckr, price, action, amt, day, month, year, currency = 'CAD') {
  
  ## Need something that will take ticker and pull correct Yahoo finance sheet
  
  sheet <- paste("http://real-chart.finance.yahoo.com/table.csv?s=", tckr,"&a=", month - 1, "&b=", day,"&c=", year,"&d=11&e=3&f=2015&g=d&ignore=.csv", sep = "")
  table <- read.csv(url(sheet))
  ## next line to be replaced with previous one
  #table <- read.csv(file = "table.csv", header = TRUE)
  
  ## Make date fields useable and comparable
  
  dates <- as.Date(as.character(table[, 1]),"%Y-%m-%d")
  trade_date <- as.Date(as.character(paste(year, month, day, sep = "-"), "%Y-%m-%d"))
  dates <- dates[dates > trade_date]
  
  ## Record additional market data, cut off unecessary data
  close <- table[1:length(dates),5]
  volume <- table[1:length(dates),6]
  adj_close <- table[1:length(dates),7]
  
  ## Enter purchase price on trade date
  #hold_return <- matrix[0,]
  
  ## Calculate returns
  r   <- -log(table[2:nrow(table), 7] / table[1:(nrow(table) - 1), 7])*100
  ret <- r[1:length(dates)]
  
  ## Construct table with date and returns and save to csv
  
  x <- cbind(dates, close, volume, adj_close, ret)
  write.csv(x, file = paste(tckr, ".csv", sep=""))
  
  ## Rung log_trade function
  log_trade(tckr, price, action, amt, trade_date, currency = 'CAD')

   
}


log_trade <- function(tckr, price, action, amt, trade_date, currency) {
  
  ## Search to see if log exists, if not, creates one
  
  a <- file.exists("log.csv")
  if (a == FALSE) {
    
    ## Create data frame with column headers and first entry    
    setup <- data.frame("Txn #"=1,"Ticker"=tckr, "Price"=price, "Action"=action, "Units"=amt, "Date"=trade_date, "Currency"=currency)
    
    ## Write file
    write.csv(setup, "log.csv")
    
  } else {
    
    ## Pull existing data from log
    existing <- read.csv(file = "log.csv", header = TRUE)
    ## Count how many transactions are already in log
    T <- nrow(existing)
    ## Build vector of new transaction details
    rowTBA <- c(T+1, tckr, price, action, amt, trade_date, currency)
    ## Add to existing data frame
    newdf <- rbind(existing, rowTBA)
    ## Name rows
    T <- nrow(newdf)
    ## Create file
    write.csv(newdf, file = "log.csv")
    
  }
  
}


#a <- "AAPL"
#b <- 100
#c <- "Buy"
#d <- 50
#e <- 1
#f <- 1
#g <- 2014

#trade(a,b,c,d,e,f,g)
#trade("KO", 32, "Buy", 100, 4, 10, 2013)



## Operational up to here -----------------------------------------------------

