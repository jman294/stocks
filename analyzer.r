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

get_average_profit_margin <- function (stock_data) {
  sum <- 0
  count <- 0
  for (i in stock_data) {
    if (!(is.na(i$financials[1, 'netIncome']) ||
          is.null(i$financials[1, 'netIncome']) ||
          is.null(i$financials[1, 'totalRevenue']) ||
          is.na(i$financials[1, 'totalRevenue']))) {
      sum = sum + i$financials[1, 'netIncome'] / i$financials[1, 'totalRevenue']
      count = count + 1
    }
  }
  return(sum/count)
}

# Past Growth
get_average_cash_change <- function (stock) {
  mean(as.vector(diff(stock$financials[, 'currentCash'])/c(0, stock$financials[, 'currentCash'], 0))[2:4])
}

get_average_cash_change_sector <- function (stocks) {
  sum <- 0
  count <- 0
  for (stock in stocks) {
    averageCashChangeStock <- get_average_cash_change(stock)
    if (!(is.na(averageCashChangeStock) || is.null(averageCashChangeStock))) {
      sum <- sum + averageCashChangeStock
      count <- count + 1
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

stockScoresList <- data.frame("Symbol" = "NULL", "Score" = 1, "Industry" = "NULL", stringsAsFactors=FALSE)

for (i in list.files('./sectors/rstockdata')) {
  stocks <- read_stock_data(paste('./sectors/rstockdata/', i, sep=''))
  totalMarketCap <- get_total_market_cap(stocks)
  averageROA <- get_average_roa(stocks)
  averageDebtRatio <- get_average_debt_ratio(stocks)
  averageRnd <- get_average_rnd(stocks)
  averageCashChange <- get_average_cash_change_sector(stocks)
  averageProfitMargin <- get_average_profit_margin(stocks)

  for (stock in stocks) {
    mktShare <- stock$marketcap/totalMarketCap

    cashChange <- (get_average_cash_change(stock) - averageCashChange)/get_average_cash_change(stock)

    debtRatio <- stock$financials[1, 'totalDebt']/stock$financials[1, 'totalAssets']
    useDebt <- !(is.na(stock$financials[1, 'totalDebt']) || is.na(stock$financials[1, 'totalAssets']) || is.null(stock$financials[1, 'totalDebt']) || is.null(stock$financials[1, 'totalAssets']))

    useRnd <- !(is.na(stock$financials[1, 'researchAndDevelopment']) || is.null(stock$financials[1, 'researchAndDevelopment']))

    profitMarginDiff <- stock$financials[1, 'netIncome']/stock$financials[1, 'totalRevenue'] - averageProfitMargin

    cat(mktShare, roa, cashChange, debtRatio, rnd, profitMarginDiff, stock$symbol, '\n')
    if (useRnd && useDebt) {
      rnd <- (stock$financials[1, 'researchAndDevelopment'] - averageRnd)/stock$financials[1, 'researchAndDevelopment']
      roa <- (stock$returnOnAssets - averageROA)/stock$returnOnAssets
      score <- .2 * mktShare + .3 * roa + .1 * cashChange + .1 * debtRatio + .1 * rnd * .2 * profitMarginDiff
    } else if (useRnd && !is.na(profitMarginDiff) && !is.na(cashChange)) {
      rnd <- (stock$financials[1, 'researchAndDevelopment'] - averageRnd)/stock$financials[1, 'researchAndDevelopment']
      roa <- (stock$returnOnAssets - averageROA)/stock$returnOnAssets
      score <- .3 * mktShare + .35 * roa + .1 * cashChange + .1 * rnd + .15 * profitMarginDiff
    } else if (useDebt && !is.na(profitMarginDiff) && !is.na(cashChange)) {
      roa <- (stock$returnOnAssets - averageROA)/stock$returnOnAssets
      score <- .25 * mktShare + .35 * roa + .1 * cashChange + .15 * debtRatio + .15 * profitMarginDiff
    } else {
      score <- NA
      stock$symbol <- NULL
    }

    if (!is.na(score)) {
      listing <- list(stock$symbol, as.numeric(score), i)
      stockScoresList <- rbind(stockScoresList, listing, stringsAsFactors=FALSE)
    }
  }
}

stockScoresList <- stockScoresList[order(stockScoresList$Score, decreasing=TRUE),]
saveRDS(stockScoresList, file='./output/sortedstocks.RData')
