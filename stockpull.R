## Stock returns reader
## Eamonn Gamble
## December 12, 2015

## Version Notes 1.1
## Built out trade function
## Pulls info from Yahoo finance, creates csv with price history since purchase, logs trade details in another sheet

rm( list=ls() )

setwd('C:/Users/Eamonn/Documents/GitHub/MereMarkets')

library(foreign)
library(gdata)

## Define two functions
## Primary function, trade(), will take user inputs, pull appropriate data from internet, 
## and add relevant data to a csv file, also creates second csv that houses txn logs
## Secondary function will use txn logs to calculate holding period return for any time period


trade <- function(tckr, price, action, amt, day, month, year, currency = 'CAD') {
  
  ## Need something that will take ticker and pull correct Yahoo finance sheet
  
  sheet <- paste("http://real-chart.finance.yahoo.com/table.csv?s=", tckr,"&a=", month - 1, "&b=", day,"&c=", year,"&d=11&e=3&f=2015&g=d&ignore=.csv", sep = "")
  #table <- read.csv(url(sheet))
  ## next line to be replaced with previous one
  table <- read.csv(file = "table.csv", header = TRUE)
  
  ## Make date fields useable and comparable
  
  dates <- as.Date(as.character(table[, 1]),"%Y-%m-%d")
  trade_date <- as.Date(as.character(paste(year, month, day, sep = "-"), "%Y-%m-%d"))
  dates <- dates[dates > trade_date]
  
  ## Calculate returns
  
  r   <- -log(table[2:nrow(table), 7] / table[1:(nrow(table) - 1), 7])*100
  ret <- r[1:length(dates)]
  
  ## Construct table with date and returns and save to csv
  
  x <- data.frame(dates, ret)
  write.csv(x, file = paste(tckr, ".csv", sep=""))

  ## Search to see if log exists, if not, creates one
  
  a <- file.exists("log.csv")
  if (a == FALSE) {

    ## Create empty vector with right dimensions    
    setup <- array(0, dim = c(1,6))
    ## Assign column names
    colnames(setup) <- c("Ticker", "Price", "Action", "Units", "Date", "Currency")
    ## Build vector of new transaction details
    rowTBA <- c(tckr, price, action, amt, trade_date, currency)
    ## Add to new data frame
    newdf <- rbind(setup, rowTBA)
    ## Name rows
    rownames(newdf) <- c(0,1)
    ## Create file
    write.csv(newdf, "log.csv")
  
  } else {
      
    ## Pull existing data from log
    existing <- read.csv(file = "log.csv", header = TRUE)
    ## Build vector of new transaction details
    rowTBA <- c(tckr, price, action, amt, trade_date, currency)
    ## Add to existing data frame
    newdf <- rbind(existing, rowTBA)
    ## Name rows
    T <- nrow(newdf)
    rownames(newdf) <- array(1:T)
    ## Create file
    write.csv(newdf, file = "log.csv")
    
  }
   
}

a <- "AAPL"
b <- 100
c <- "Buy"
d <- 50
e <- 1
f <- 1
g <- 2014

trade(a,b,c,d,e,f,g)



## Operational up to here -----------------------------------------------------

