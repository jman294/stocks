data <- read.csv(file=paste('./histdaily/', 'table_ebay.csv', sep=''), head=FALSE, sep=",", colClasses=c("myDate", "NULL", NA, "NULL", "NULL", "NULL", "NULL"))
diff <- diff(data$V3)
supervised = lag_transform(diff)
N = nrow(supervised)
train = supervised[1:N, ]
test = supervised[1:N, ]
Scaled = scale_data(train, test, c(-1, 1))

y_train = Scaled$scaled_train[, 2]
x_train = Scaled$scaled_train[, 1]
# Reshape the input to 3-dim
dim(x_train) <- c(length(x_train), 1, 1)

# specify required arguments
X_shape2 = dim(x_train)[2]
X_shape3 = dim(x_train)[3]
batch_size = 1                # must be a common factor of both the train and test samples
units = 1                     # can adjust this, in model tuninig phase

L = N
scaler = Scaled$scaler
predictions = numeric(L)

for(i in 1:L){
     X = x_train[i]
     str(X)
     dim(X) = c(1,1,1)
     yhat = model %>% predict(X, batch_size=batch_size)
     # invert scaling
     yhat = invert_scaling(yhat, scaler,  c(-1, 1))
     # invert differencing
     yhat  = yhat + data$V3[(n+i)]
     # store
     predictions[i] <- yhat
}

# Plot predicted vs actual. Training set + testing set
plot(data, col = 'red', type = 'l', main = "Actual vs predicted", ylab = "Price")
lines(as.vector(data$V1), y=as.vector(predictions), type = 'l', col = 'blue')
legend("topright", c("Predicted", "Real"), col = c("blue","red"), lty = c(1,1), lwd = c(1,1))
