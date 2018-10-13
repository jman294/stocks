files <- list.files(path="./histdaily", pattern="table_x.*.csv$")
stuff <- lapply(files, function(x) {
  data <- read.csv(file=paste('./histdaily/', x, sep=''), head=FALSE, sep=",") # load file
  index <- sample(1:nrow(data),round(0.75*nrow(data)))
  train <- data[index,]
  test <- data[-index,]
})
