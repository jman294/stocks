library(httr)
library(jsonlite)

setClass("myDate")
setAs("character", "myDate", function(from) as.Date(from, format="%Y%m%d") )

# Helper functions
get_stock_data <- function (stock) {
  raw <- GET(paste('https://api.iextrading.com/1.0/stock/', stock, '/stats', sep=''))
  content <- NULL
  result = tryCatch({
    content <- fromJSON(rawToChar(raw$content))
  }, warning = function(w) {
  }, error = function(e) {
    return(NULL)
    stop()
  }, finally = {
  })
  return(content)
  Sys.sleep(1)
}

get_all_stock_data <- function (csv_file) {
  stocks <- c()
  data <- read.csv(csv_file, head=FALSE, stringsAsFactors=FALSE)
  for (i in data$V1) {
    stock_data <- get_stock_data(i)
    stocks[[i]] <- stock_data
  }
  return(stocks)
}

save_all_stock_data <- function (csv_file, R_filename) {
  stocks <- get_all_stock_data(csv_file)
  saveRDS(stocks, file=R_filename)
}

write_all_stock_data <- function () {
  setwd('./sectors/')
  for (i in list.files()) {
    if (i != 'rstockdata') {
      save_all_stock_data(i, paste('rstockdata/', i, '.rds', sep=''))
    }
  }
  setwd('..')
}

read_stock_data <- function (R_filename) {
  return(readRDS(file=R_filename))
}

# Market Share
# Percent of combined market cap company holds over past three years
#                                                   |-Change in market share

get_total_market_share <- function (stock_data) {
  total <- 0
  for (i in data) {
    total = total + i$marketcap
  }
  return(total)
}

#stocks <- readRDS('./sectors/energy.rds')
#cat(str(get_total_market_share('./sectors/energy.csv')))

# Financial Strength
# Debt to equity ratio maybe? Some other basic financial ratio

# Innovation
# Percent headlines mentioning company and "new"

# Business model
# Rated by user for now
