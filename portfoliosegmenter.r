files <- list.files(path="./histdaily", pattern="table_x.*.csv$")
stuff <- lapply(files, function(x) {
  data <- read.csv(file=paste('./histdaily/', x, sep=''), head=FALSE, sep=",") # load file
  index <- sample(1:nrow(data),round(0.75*nrow(data)))
  train <- data[index,]
  test <- data[-index,]
  maxs <- apply(data, 2, max)
  mins <- apply(data, 2, min)

  scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

  train_ <- scaled[index,]
  test_ <- scaled[-index,]
  apply(train_, 2, cat)
})
