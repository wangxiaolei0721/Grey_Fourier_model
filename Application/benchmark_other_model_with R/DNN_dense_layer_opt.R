# clear work space and screen
rm(list=ls()) # clear data and value
shell("cls") # clear screen
gc() # clear memory


# load packages
library(keras)
library(readr)
library(ggplot2)


# set script path
current_path<-getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# read data from .txt document
CSJ_PM25 <- read_csv("./CSJ_PM25.txt",col_names = TRUE,show_col_types = FALSE)
# View(CSJ_PM25) view data


# data grouping
l <- nrow(CSJ_PM25)
test <- 24
validation <- 12
train <- l-test-validation


# model parameter
lookback <- 12  # backtracking time step = 12 24
delay <- 1 # prediction step = 1
batch_size <- 6 # batch size = 6 12


# data generator with batch
generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 6) {
  if (is.null(max_index)) max_index <- length(data)-delay
  i <- min_index # begin from min_index - 1
  function() {
    if (shuffle) {
      rows <- sample(c(min_index:max_index), size = batch_size)
    } else {
      if (i + batch_size - 1 > max_index)
        i <<- min_index
      rows <- c(i:min(i+batch_size-1, max_index))
      print(rows)
      i <<- i + length(rows)
      print(i)
    }
    samples <- array(0, dim = c(length(rows),
                                lookback))
    targets <- array(0, dim = c(length(rows)))
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback+1, rows[[j]],
                     length.out = dim(samples)[[2]])
      print(indices)
      samples[j,] <- data[indices]
      targets[[j]] <- data[rows[[j]] + delay]
    }
    data_list <- list(samples, targets)
    return(data_list)
  }
}


# shanghai time series
SH<-CSJ_PM25$SH
plot(SH)



SH_train_gen <- generator(
  SH,
  lookback = lookback,
  delay = delay,
  min_index = lookback,
  max_index = train-delay,
  shuffle = TRUE,
  batch_size = batch_size
)



SH_val_gen = generator(
  SH,
  lookback = lookback,
  delay = delay,
  min_index = train,
  max_index = train+validation-delay,
  batch_size = batch_size
)
val_steps <- validation / batch_size



# fully connected neural network
set.seed(1)
SH_model <- keras_model_sequential() %>%
  layer_dense(units = 24, activation = "relu",input_shape = c(lookback)) %>% # units = 12 24 36
  layer_dense(units = 1)
SH_model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mape"
)


history <- SH_model %>% fit(
  SH_train_gen,
  steps_per_epoch = 20,
  epochs = 20,
  validation_data = SH_val_gen,
  validation_steps = val_steps
)

plot(history)



# test value by iterative prediction
SH_result<- rep(NA, l)
SH_test <- SH[1:(train+validation)]

for (i in ((lookback+1):(train+validation))){
  test_one<-t(as.matrix(SH_test[(i-lookback):(i-1)]))
  test_one_predict <- SH_model %>% predict(test_one)
  SH_result[i] <- test_one_predict
}

for (i in ((train+validation+1):l)){
  test_one<-t(as.matrix(SH_result[(i-lookback):(i-1)]))
  test_one_predict <- SH_model %>% predict(test_one)
  SH_result[i] <- test_one_predict
}
SH_test <- SH_result[(l-test+1):l]
# absolute percentage error
SH_APET<-abs(SH_test-SH[(l-test+1):l]) / SH[(l-test+1):l] *100
SH_MAPET<-mean(SH_APET,na.rm = TRUE)
plot(SH_result)



# nanjing time series
NJ<-CSJ_PM25$NJ
plot(NJ)


NJ_train_gen <- generator(
  NJ,
  lookback = lookback,
  delay = delay,
  min_index = lookback,
  max_index = train-delay,
  shuffle = TRUE,
  batch_size = batch_size
)


NJ_val_gen = generator(
  NJ,
  lookback = lookback,
  delay = delay,
  min_index = train,
  max_index = train+validation-delay,
  batch_size = batch_size
)
val_steps <- validation / batch_size



# fully connected neural network
set.seed(1)
NJ_model <- keras_model_sequential() %>%
  layer_dense(units = 24, activation = "relu",input_shape = c(lookback)) %>% # units = 12 24 36
  layer_dense(units = 1)
NJ_model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mape"
)


history <- NJ_model %>% fit(
  NJ_train_gen,
  steps_per_epoch = 20,
  epochs = 20,
  validation_data = NJ_val_gen,
  validation_steps = val_steps
)

plot(history)



# test value by iterative prediction
NJ_result<- rep(NA, l)
NJ_test <- NJ[1:(train+validation)]

for (i in ((lookback+1):(train+validation))){
  test_one<-t(as.matrix(NJ_test[(i-lookback):(i-1)]))
  test_one_predict <- NJ_model %>% predict(test_one)
  NJ_result[i] <- test_one_predict
}

for (i in ((train+validation+1):l)){
  test_one<-t(as.matrix(NJ_result[(i-lookback):(i-1)]))
  test_one_predict <- NJ_model %>% predict(test_one)
  NJ_result[i] <- test_one_predict
}
NJ_test <- NJ_result[(l-test+1):l]
# absolute percentage error
NJ_APET<-abs(NJ_test-NJ[(l-test+1):l]) / NJ[(l-test+1):l] *100
NJ_MAPET<-mean(NJ_APET,na.rm = TRUE)
plot(NJ_result)



# hangzhou time series
HZ<-CSJ_PM25$HZ
plot(HZ)


HZ_train_gen <- generator(
  HZ,
  lookback = lookback,
  delay = delay,
  min_index = lookback,
  max_index = train-delay,
  shuffle = TRUE,
  batch_size = batch_size
)


HZ_val_gen = generator(
  HZ,
  lookback = lookback,
  delay = delay,
  min_index = train,
  max_index = train+validation-delay,
  batch_size = batch_size
)
val_steps <- validation / batch_size



# fully connected neural network
set.seed(1)
HZ_model <- keras_model_sequential() %>%
  layer_dense(units = 24, activation = "relu",input_shape = c(lookback)) %>% # units = 12 24 36
  layer_dense(units = 1)
HZ_model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mape"
)


history <- HZ_model %>% fit(
  HZ_train_gen,
  steps_per_epoch = 20,
  epochs = 20,
  validation_data = HZ_val_gen,
  validation_steps = val_steps
)

plot(history)



# test value by iterative prediction
HZ_result<- rep(NA, l)
HZ_test <- HZ[1:(train+validation)]

for (i in ((lookback+1):(train+validation))){
  test_one<-t(as.matrix(HZ_test[(i-lookback):(i-1)]))
  test_one_predict <- HZ_model %>% predict(test_one)
  HZ_result[i] <- test_one_predict
}

for (i in ((train+validation+1):l)){
  test_one<-t(as.matrix(HZ_result[(i-lookback):(i-1)]))
  test_one_predict <- HZ_model %>% predict(test_one)
  HZ_result[i] <- test_one_predict
}
HZ_test <- HZ_result[(l-test+1):l]
# absolute percentage error
HZ_APET<-abs(HZ_test-HZ[(l-test+1):l]) / HZ[(l-test+1):l] *100
HZ_MAPET<-mean(HZ_APET,na.rm = TRUE)
plot(HZ_result)



# hefei time series
HF<-CSJ_PM25$HF
plot(HF)


HF_train_gen <- generator(
  HF,
  lookback = lookback,
  delay = delay,
  min_index = lookback,
  max_index = train-delay,
  shuffle = TRUE,
  batch_size = batch_size
)


HF_val_gen = generator(
  HF,
  lookback = lookback,
  delay = delay,
  min_index = train,
  max_index = train+validation-delay,
  batch_size = batch_size
)
val_steps <- validation / batch_size



# fully connected neural network
set.seed(1)
HF_model <- keras_model_sequential() %>%
  layer_dense(units = 24, activation = "relu",input_shape = c(lookback)) %>% # units = 12 24 36
  layer_dense(units = 1)
HF_model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mape"
)


history <- HF_model %>% fit(
  HF_train_gen,
  steps_per_epoch = 20,
  epochs = 20,
  validation_data = HF_val_gen,
  validation_steps = val_steps
)

plot(history)



# test value by iterative prediction

HF_result<- rep(NA, l)
HF_test <- HF[1:(train+validation)]

for (i in ((lookback+1):(train+validation))){
  test_one<-t(as.matrix(HF_test[(i-lookback):(i-1)]))
  test_one_predict <- HF_model %>% predict(test_one)
  HF_result[i] <- test_one_predict
}


for (i in ((train+validation+1):l)){
  test_one<-t(as.matrix(HF_result[(i-lookback):(i-1)]))
  test_one_predict <- HF_model %>% predict(test_one)
  HF_result[i] <- test_one_predict
}
HF_test <- HF_result[(l-test+1):l]
# absolute percentage error
HF_APET<-abs(HF_test-HF[(l-test+1):l]) / HF[(l-test+1):l] *100
HF_MAPET<-mean(HF_APET,na.rm = TRUE)
plot(HF_result)




# data export
CSJ_densenet<-data.frame(SH_result,NJ_result,HZ_result,HF_result)
CSJ_densenet
write.csv(CSJ_densenet,"../benchmark_other_model_data/PM25_densenet.csv")


# return to current_path
setwd(current_path) 

