library(httr)
library(jsonlite)

# Helper functions
get_stock_data <- function (stock) {
  rawStats <- GET(paste('https://api.iextrading.com/1.0/stock/', stock, '/stats', sep=''))
  rawPrice <- GET(paste('https://api.iextrading.com/1.0/stock/', stock, '/price', sep=''))
  rawFinancials <- GET(paste('https://api.iextrading.com/1.0/stock/', stock, '/financials', sep=''))
  contentStats <- NULL
  contentPrice <- NULL
  contentFinancials <- NULL
  result = tryCatch({
    contentStats <- fromJSON(rawToChar(rawStats$content))
    contentPrice <- fromJSON(rawToChar(rawPrice$content))
    contentFinancials <- fromJSON(rawToChar(rawFinancials$content))
  }, warning = function(w) {
  }, error = function(e) {
    return(NULL)
    stop()
  }, finally = {
  })
  temp <- c(contentStats, contentFinancials)
  temp[['price']] = contentPrice
  return(temp)
  Sys.sleep(1)
}

# Program
sortedStocks <- readRDS('./sortedstocks.RData')

apply(sortedStocks, 1, function (value) {
  stockData <- get_stock_data(value[1])
  percentOffHigh <- (stockData$price - stockData$week52high) / stockData$price
  movingSlope <- stockData$price - stockData$day200MovingAverage
  cat(percentOffHigh)

  #cat(str(stockData))
})
