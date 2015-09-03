
#read stock data function--pass ticker symbol as parameter
read.stock.data <- function(symbol){
URL <- paste0("http://ichart.finance.yahoo.com/table.csv?s=", symbol)
dat <- tryCatch(read.csv(URL, header=T, stringsAsFactors=F), warning = function(w) {print(paste0("ticker symbol ", symbol, " failed to scrape."))})
dat
}

#example
dat<-read.stock.data("AAPL")

#pass multiple symbols and create a list object
import.stocks.data<-function(symbols) {
  dat.ls<-list()
  for (i in 1:length(symbols)){
    dat<-read.stock.data(symbols[i])
    dat.ls[[i]] <- dat
    }
  dat.ls
}

#example

#just a few symbols you care about...
symbols.few <- c("GOOG", "MSFT", "CVX", "AAPL", "F")

#build list of historic data for each symbol
dat.ls<-import.stocks.data(symbols.few)

#look at one of the data frames
head(dat.ls[[2]])
tail(dat.ls[[2]])

#another example with more data
setwd("~/R/Stocks")

#Get all symbols
#NASDAQ makes this information available via FTP and they update it every night. Log into ftp.nasdaqtrader.com anonymously. Look in the directory SymbolDirectory. You'll notice two files: nasdaqlisted.txt and otherlisted.txt. These two files will give you the entire list of tradeable symbols, where they are listed, their name/description, and an indicator as to whether they are an ETF.
symbol.dat <- read.table("nasdaqlisted.txt", header=T, stringsAsFactors=F, sep="|")
symbols <- unique(symbol.dat$Symbol)

#get ALL stock data
dat.ls<-import.stocks.data(symbols)


#post-processing (Hooper's code)

death.cross<-function(stocks.ls){

  for (i in 1:length(stocks.ls)) {
    
    stock<-stocks.ls[[i]]
    
    if (!is.null(dim(stock)[1])){
      
      #format data
      stock$Date <- as.Date(stock$Date, "%Y-%m-%d")
      names(stock) <- c("Date", "Open", "High", "Low", "Close", "Vol", "Adj")
      stock$symbol <- symbols[i]
      
      #50 Day Stock Moving Average
      sma.50 <- numeric()
      for (j in 1:nrow(stock)) {
        sma.50[j] <- mean(stock$Adj[j:(j+49)])
        sma.50
      }
      
      #200 Day Stock Moving Average
      sma.200 <- numeric()
      for (j in 1:nrow(stock)) {
        sma.200[j] <- mean(stock$Adj[j:(j+199)])
        sma.200
      }
      
      stock <- data.frame(stock, sma.50, sma.200)
      stock$death.cross <- stock$sma.50 - stock$sma.200
      
    }
    
    stocks.ls[[i]]<-stock
    
  }
  
  stocks.ls
  
}

#example

symbol.dat <- read.table("nasdaqlisted.txt", header=T, stringsAsFactors=F, sep="|")
symbols <- unique(symbol.dat$Symbol)
dat.ls<-import.stocks.data(symbols[1:50])
stocks.ls <- death.cross(dat.ls)