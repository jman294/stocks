library(rnn)

files <- list.files(path="./histdaily", pattern="table_x.csv$")

lag_transform <- function(x, k= 1){
  lagged =  c(rep(NA, k), x[1:(length(x)-k)])
  DF = as.data.frame(cbind(lagged, x))
  colnames(DF) <- c( paste0('x-', k), 'x')
  DF[is.na(DF)] <- 0
  return(DF)
}

scale_data = function(train, test, feature_range = c(0, 1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = ((x - min(x) ) / (max(x) - min(x)  ))
  std_test  = ((test - min(x) ) / (max(x) - min(x)  ))

  scaled_train = std_train *(fr_max -fr_min) + fr_min
  scaled_test = std_test *(fr_max -fr_min) + fr_min

  return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
}

## inverse-transform
invert_scaling = function(scaled, scaler, feature_range = c(0, 1)){
  min = scaler[1]
  max = scaler[2]
  t = length(scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(t)

  for( i in 1:t){
    X = (scaled[i]- mins)/(maxs - mins)
    rawValues = X *(max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}

model <- NULL
stuff <- lapply(files, function(x) {
  setClass("myDate")
  setAs("character", "myDate", function(from) as.Date(from, format="%Y%m%d") )
  data <- read.csv(file=paste('./histdaily/', x, sep=''), head=FALSE, sep=",", colClasses=c("myDate", "NULL", NA, "NULL", "NULL", "NULL", "NULL"))
  diff <- diff(data$V3)
  supervised = lag_transform(diff)
  N = nrow(supervised)
  n = round(N *0.7, digits = 0)
  train = supervised[1:n, ]
  test  = supervised[(n+1):N,  ]
  Scaled = scale_data(train, test, c(-1, 1))

  y_train = Scaled$scaled_train[, 2]
  x_train = Scaled$scaled_train[, 1]
  y_test = Scaled$scaled_test[, 2]
  x_test = Scaled$scaled_test[, 1]
  # Reshape the input to 3-dim
  dim(x_train) <- c(length(x_train), 1, 1)
  dim(y_train) <- c(length(y_train), 1, 1)

  # specify required arguments
  X_shape2 = dim(x_train)[2]
  X_shape3 = dim(x_train)[3]
  batch_size = 1                # must be a common factor of both the train and test samples
  units = 1                     # can adjust this, in model tuninig phase

  model <- trainr(y_train, x_train, model=model, learningrate=1, hidden_dim=16, network_type="lstm")

  L = length(x_test)
  scaler = Scaled$scaler
  predictions = numeric(L)

  for(i in 1:L){
       X = x_test[i]
       dim(X) = c(1,1,1)
       yhat = predictr(model, X)
       # invert scaling
       yhat = invert_scaling(yhat, scaler,  c(-1, 1))
       # invert differencing
       yhat  = yhat + data$V3[(n+i)]
       # store
       predictions[i] <- yhat
  }
  # Plot predicted vs actual. Training set + testing set
  plot(as.vector(data$V3), col = 'red', type = 'l', main = "Actual vs predicted", ylab = "Y,Yp")
  lines(as.vector(predictions), type = 'l', col = 'blue')
  legend("topright", c("Predicted", "Real"), col = c("blue","red"), lty = c(1,1), lwd = c(1,1))

  #=========================================================================================
  #library(keras)
  #library(tensorflow)

  #model <- keras_model_sequential()
  #model%>%
    #layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= TRUE)%>%
    #layer_dense(units = 1)
})
