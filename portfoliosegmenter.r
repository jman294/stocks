files <- list.files(path="./histdaily", pattern="table_x.*.csv$")
stuff <- lapply(files, function(x) {
  data <- read.csv(file=paste('./histdaily/', x, sep=''), head=TRUE, sep=",", colClasses=c("NULL", "NULL", NA, "NULL", "NULL", "NULL", "NULL"))
  index <- sample(1:nrow(data),round(0.75*nrow(data)))
  train <- data[index,]
  test <- data[-index,]
  maxs <- apply(data, 2, max)
  mins <- apply(data, 2, min)

  scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

  train_ <- scaled[index,]
  test_ <- scaled[-index,]
  #str(train_)
  #names(train_) <- "price"
  #str(names(train_))
  library(neuralnet)
  #f <- as.formula(paste(" ~", paste(n[!n %in% "NULL"], collapse = " + ")))
  f <- as.formula("~ price")
  nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)
})
