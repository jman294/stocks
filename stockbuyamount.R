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
sortedStocks <- readRDS('./output/sortedstocks.RData')

stockScoresList <- data.frame("Symbol" = "NULL", "Percent" = 0, "Price" = 0, stringsAsFactors=FALSE)

sum <- 0
toConsider <- 10

pot <- 60000

for (i in 1:toConsider) {
  value <- sortedStocks[i,]
  stockData <- get_stock_data(value[1])

  score <- value[2]
  percentOffHigh <- (stockData$week52high - stockData$price) / stockData$price
  if (percentOffHigh <= 0) {
    percentOffHigh <- sqrt(abs(percentOffHigh))
  }

  priceDifference <- stockData$price - stockData$day200MovingAvg
  if (priceDifference <= 0) {
    priceDifference <- sqrt(abs(priceDifference))
  }

  averageDifference <- stockData$day50MovingAvg - stockData$day200MovingAvg
  if (averageDifference <= 0) {
    averageDifference <- sign(averageDifference) * abs(averageDifference)^(1/3)
  }

  cat(percentOffHigh, priceDifference, averageDifference, '\n')

  resultScore <- .15 * as.numeric(score) + .4 * percentOffHigh + .25 * priceDifference + .2 * averageDifference

  sum <- sum + resultScore
  if (!is.na(resultScore)) {
    listing <- list(stockData$symbol, as.numeric(resultScore), as.numeric(stockData$price))
    stockScoresList <- rbind(stockScoresList, listing, stringsAsFactors=FALSE)
  }
}

newStockScoresList <- data.frame("Symbol" = "NULL", "Percent" = 0, "PTotal" = 0, "NumShares" = 0, stringsAsFactors=FALSE)
for (i in 1:nrow(stockScoresList)) {
  percent <- stockScoresList[i, "Percent"]/sum
  total <- percent * pot
  listing <- list(stockScoresList[i, "Symbol"], as.numeric(percent), total, total / stockScoresList[i, "Price"])
  newStockScoresList <- rbind(newStockScoresList, listing, stringsAsFactors=FALSE)
}
