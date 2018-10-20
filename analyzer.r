library(httr)
library(jsonlite)

setClass("myDate")
setAs("character", "myDate", function(from) as.Date(from, format="%Y%m%d") )

# Helper functions
get_stock_data <- function (stock) {
  rawStats <- GET(paste('https://api.iextrading.com/1.0/stock/', stock, '/stats', sep=''))
  rawFinancials <- GET(paste('https://api.iextrading.com/1.0/stock/', stock, '/financials', sep=''))
  contentStats <- NULL
  contentFinancials <- NULL
  result = tryCatch({
    contentStats <- fromJSON(rawToChar(rawStats$content))
    contentFinancials <- fromJSON(rawToChar(rawFinancials$content))
  }, warning = function(w) {
  }, error = function(e) {
    return(NULL)
    stop()
  }, finally = {
  })
  return(c(contentStats, contentFinancials))
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

get_total_market_cap <- function (stock_data) {
  total <- 0
  for (i in stock_data) {
    total = total + i$marketcap
  }
  return(total)
}

# Financial Strength
# Debt to equity ratio maybe? Some other basic financial ratio
get_average_roa <- function (stock_data) {
  sum <- 0
  count <- 0
  for (i in stock_data) {
    if (class(i$returnOnAssets) == 'numeric') {
      sum = sum + i$returnOnAssets
      count = count + 1
    }
  }
  return(sum/count)
}

get_average_debt_ratio <- function (stock_data) {
  sum <- 0
  count <- 0
  for (i in stock_data) {
    if (!(is.na(i$financials[1, 'totalDebt']) ||
          is.null(i$financials[1, 'totalDebt']) ||
          is.null(i$financials[1, 'totalAssets']) ||
          is.na(i$financials[1, 'totalAssets']))) {
      sum = sum + i$financials[1, 'totalDebt'] / i$financials[1, 'totalAssets']
      count = count + 1
    }
  }
  return(sum/count)
}


# Innovation
# Percent headlines mentioning company and "new"

get_average_rnd <- function (stock_data) {
  sum <- 0
  count <- 0
  for (i in stock_data) {
    if (!(is.na(i$financials[1, 'researchAndDevelopment']) ||
          is.null(i$financials[1, 'researchAndDevelopment']))) {
      sum = sum + i$financials[1, 'researchAndDevelopment']
      count = count + 1
    }
  }
  return(sum/count)
}

# Business model
# Rated by user for now

#### Prog

if (length(list.files('sectors/rstockdata')) == 0) {
  cat('Gathering stock data...')
  write_all_stock_data()
  cat('done')
}

for (i in list.files('./sectors/rstockdata')) {
  stocks <- read_stock_data(paste('./sectors/rstockdata/', i, sep=''))
  totalMarketCap <- get_total_market_cap(stocks)
  averageROA <- get_average_roa(stocks)
  averageDebtRatio <- get_average_debt_ratio(stocks)
  averageRnd <- get_average_rnd(stocks)
  cat('\n\n\n')
  cat(i)
  cat('\nAverage ROA: ')
  cat(averageROA)
  cat('\nAverage Debt Ratio: ')
  cat(averageDebtRatio)
  cat('\nAverage RND: ')
  cat(averageRnd)
  cat('\n\n')
  for (stock in stocks) {
    mktShare <- stock$marketcap/totalMarketCap

    debtRatio <- stock$financials[1, 'totalDebt']/stock$financials[1, 'totalAssets']
    useDebt <- !(is.na(stock$financials[1, 'totalDebt']) || is.na(stock$financials[1, 'totalAssets']) || is.null(stock$financials[1, 'totalDebt']) || is.null(stock$financials[1, 'totalAssets']))

    useRnd <- !(is.na(stock$financials[1, 'researchAndDevelopment']) || is.null(stock$financials[1, 'researchAndDevelopment']))

    if (useRnd && useDebt) {
      score <- .4 * mktShare + .4 * (stock$returnOnAssets - averageROA) + .1 * debtRatio + .1 * rnd
    } else if (useRnd) {
      rnd <- sqrt(stock$financials[1, 'researchAndDevelopment']) - sqrt(averageRnd)
      score <- .4 * mktShare + .4 * (stock$returnOnAssets - averageROA) + .2 * rnd
    } else if (useDebt) {
      score <- .4 * mktShare + .4 * (stock$returnOnAssets - averageROA) + .2 * debtRatio
    } else {
      score <- .55 * mktShare + .45 * (stock$returnOnAssets - averageROA)
    }
    cat(stock$symbol)
    cat(' Score: \n')
    cat(str(debtRatio), str(rnd), str(stock$financials[1, 'researchAndDevelopment']), str(mktShare), "\n");
    cat(score*100)
    cat('\n\n')
  }
}
