# clear work space and screen
rm(list=ls()) # clear data and value
shell("cls") # clear screen
gc() # clear memory


# load package
library(forecast) # packages for ARIMA
library(readr)
library(ggplot2)


# set script path
current_path<-getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# read data from .txt document
CSJ_PM25 <- read_csv("./CSJ_PM25.txt",col_names = TRUE,show_col_types = FALSE)
# View(CSJ_PM25) view data


# train and test
l<-nrow(CSJ_PM25)
test<-24
validation<-12
train<-l-test-validation



# shanghai time series
SH<-CSJ_PM25$SH
SH_ts<-ts(SH, frequency = 12, start = c(2014, 12))
SH_fit<-ts(SH[1:train], frequency = 12, start = c(2014, 12)) # 2014
SH_val<-ts(SH[(train+1):(train+validation)], frequency = 12, start = c(2018, 12)) # 2018


# diff
SH_fit_diff12<-diff(SH_fit,lag=12)
plot(SH_fit_diff12)
SH_fit_diff12_diff1<-diff(SH_fit_diff12,1)
plot(SH_fit_diff12_diff1)


# First step: order selection

# auto order
SH_arima <- auto.arima(SH_fit)
SH_arima
SH_APEF<-abs(SH_arima$residuals) / SH_fit *100
SH_MAPEF<-mean(SH_APEF)
SH_arima_fore <- forecast(SH_arima, h = validation)
SH_APEV<-abs(SH_arima_fore$mean-SH_val) / SH_val *100
SH_MAPEV<-mean(SH_APEV)
plot(SH_arima_fore)
SH_auto<-c(SH_arima$fitted,SH_arima_fore$mean)


# optimize order
order<-cbind(
c(0, 1, 0, 0, 1, 1),
c(0, 1, 0, 1, 1, 0),
c(0, 1, 0, 1, 1, 1),
c(0, 1, 1, 0, 1, 0),
c(0, 1, 1, 0, 1, 1),
c(0, 1, 1, 1, 1, 0),
c(0, 1, 1, 1, 1, 1),
c(1, 1, 0, 0, 1, 0),
c(1, 1, 0, 0, 1, 1),
c(1, 1, 0, 1, 1, 0),
c(1, 1, 0, 1, 1, 1),
c(1, 1, 1, 0, 1, 0),
c(1, 1, 1, 0, 1, 1),
c(1, 1, 1, 1, 1, 0),
c(1, 1, 1, 1, 1, 1)
)


factor<-NULL
for (i in 1:ncol(order))
{
  factor<-c(factor,paste(order[,i],collapse = ""))
}

MAPEF<-NULL
MAPEV<-NULL



for (i in 1:ncol(order))
{
  SH_order<-Arima(SH_fit,order = order[1:3,i],seasonal = order[4:6,i])
  SH_order
  SH_order_APEF<-abs(SH_order$residuals) / SH_fit *100
  SH_order_MAPEF<-mean(SH_order_APEF)
  
  
  SH_order_val<-forecast(SH_order,h=validation)
  SH_order_APEV<-abs(SH_order_val$mean-SH_val) / SH_val *100
  SH_order_MAPEV<-mean(SH_order_APEV)
  
  MAPEF<-c(MAPEF,SH_order_MAPEF)
  MAPEV<-c(MAPEV,SH_order_MAPEV)
  }

# plot MAPEF and MAPEV
Accuracy<-data.frame(factor,MAPEF,MAPEV)
ggplot(data=Accuracy)+theme_bw()+geom_point(mapping=aes(factor,MAPEF), color = "blue", size = 3)+geom_point(mapping=aes(factor,MAPEV), color = "red", size = 3)




# Second step: establish model
SH_fit<-ts(SH[1:(train+validation)], frequency = 12, start = c(2014, 12)) # 2014
SH_test<-ts(SH[(train+validation+1):l], frequency = 12, start = c(2019, 12)) # 2019

SH_model<-Arima(SH_fit,order = c(0,1,0),seasonal = c(1,1,0))
SH_model
SH_model_APEF<-abs(SH_model$residuals) / SH_fit *100
SH_model_MAPEF<-mean(SH_model_APEF)


SH_model_test<-forecast(SH_model,h=test)
SH_model_APET<-abs(SH_model_test$mean-SH_test) / SH_test *100
SH_model_MAPET<-mean(SH_model_APET)
plot(SH_model_test)
SH_result<-c(SH_model$fitted,SH_model_test$mean)





# nanjing time series

NJ<-CSJ_PM25$NJ
NJ_ts<-ts(NJ, frequency = 12, start = c(2014, 12))
NJ_fit<-ts(NJ[1:train], frequency = 12, start = c(2014, 12)) # 2014
NJ_val<-ts(NJ[(train+1):(train+validation)], frequency = 12, start = c(2018, 12)) # 2018


# diff
NJ_fit_diff12<-diff(NJ_fit,lag=12)
plot(NJ_fit_diff12)
NJ_fit_diff12_diff1<-diff(NJ_fit_diff12,1)
plot(NJ_fit_diff12_diff1)


# First step: order selection

# auto order
NJ_arima <- auto.arima(NJ_fit)
NJ_arima
NJ_APEF<-abs(NJ_arima$residuals) / NJ_fit *100
NJ_MAPEF<-mean(NJ_APEF)
NJ_arima_fore <- forecast(NJ_arima, h = validation)
NJ_APEV<-abs(NJ_arima_fore$mean-NJ_val) / NJ_val *100
NJ_MAPEV<-mean(NJ_APEV)
plot(NJ_arima_fore)
NJ_auto<-c(NJ_arima$fitted,NJ_arima_fore$mean)


# optimize order
MAPEF<-NULL
MAPEV<-NULL
for (i in 1:ncol(order))
{
  NJ_order<-Arima(NJ_fit,order = order[1:3,i],seasonal = order[4:6,i])
  NJ_order
  NJ_order_APEF<-abs(NJ_order$residuals) / NJ_fit *100
  NJ_order_MAPEF<-mean(NJ_order_APEF)
  
  
  NJ_order_val<-forecast(NJ_order,h=validation)
  NJ_order_APEV<-abs(NJ_order_val$mean-NJ_val) / NJ_val *100
  NJ_order_MAPEV<-mean(NJ_order_APEV)
  
  MAPEF<-c(MAPEF,NJ_order_MAPEF)
  MAPEV<-c(MAPEV,NJ_order_MAPEV)
}

# plot MAPEF and MAPEV
Accuracy<-data.frame(factor,MAPEF,MAPEV)
ggplot(data=Accuracy)+theme_bw()+geom_point(mapping=aes(factor,MAPEF), color = "blue", size = 3)+geom_point(mapping=aes(factor,MAPEV), color = "red", size = 3)




# Second step: establish model
NJ_fit<-ts(NJ[1:(train+validation)], frequency = 12, start = c(2014, 12)) # 2014
NJ_test<-ts(NJ[(train+validation+1):l], frequency = 12, start = c(2019, 12)) # 2019

NJ_model<-Arima(NJ_fit,order = c(0,1,1),seasonal = c(0,1,1))
NJ_model
NJ_model_APEF<-abs(NJ_model$residuals) / NJ_fit *100
NJ_model_MAPEF<-mean(NJ_model_APEF)


NJ_model_test<-forecast(NJ_model,h=test)
NJ_model_APET<-abs(NJ_model_test$mean-NJ_test) / NJ_test *100
NJ_model_MAPET<-mean(NJ_model_APET)
plot(NJ_model_test)
NJ_result<-c(NJ_model$fitted,NJ_model_test$mean)





# hangzhou time series

HZ<-CSJ_PM25$HZ
HZ_ts<-ts(HZ, frequency = 12, start = c(2014, 12))
HZ_fit<-ts(HZ[1:train], frequency = 12, start = c(2014, 12)) # 2014
HZ_val<-ts(HZ[(train+1):(train+validation)], frequency = 12, start = c(2018, 12)) # 2018


# diff
HZ_fit_diff12<-diff(HZ_fit,lag=12)
plot(HZ_fit_diff12)
HZ_fit_diff12_diff1<-diff(HZ_fit_diff12,1)
plot(HZ_fit_diff12_diff1)


# First step: order selection

# auto order
HZ_arima <- auto.arima(HZ_fit)
HZ_arima
HZ_APEF<-abs(HZ_arima$residuals) / HZ_fit *100
HZ_MAPEF<-mean(HZ_APEF)
HZ_arima_fore <- forecast(HZ_arima, h = validation)
HZ_APEV<-abs(HZ_arima_fore$mean-HZ_val) / HZ_val *100
HZ_MAPEV<-mean(HZ_APEV)
plot(HZ_arima_fore)
HZ_auto<-c(HZ_arima$fitted,HZ_arima_fore$mean)


# optimize order
MAPEF<-NULL
MAPEV<-NULL
for (i in 1:ncol(order))
{
  HZ_order<-Arima(HZ_fit,order = order[1:3,i],seasonal = order[4:6,i])
  HZ_order
  HZ_order_APEF<-abs(HZ_order$residuals) / HZ_fit *100
  HZ_order_MAPEF<-mean(HZ_order_APEF)
  
  
  HZ_order_val<-forecast(HZ_order,h=validation)
  HZ_order_APEV<-abs(HZ_order_val$mean-HZ_val) / HZ_val *100
  HZ_order_MAPEV<-mean(HZ_order_APEV)
  
  MAPEF<-c(MAPEF,HZ_order_MAPEF)
  MAPEV<-c(MAPEV,HZ_order_MAPEV)
}

# plot MAPEF and MAPEV
Accuracy<-data.frame(factor,MAPEF,MAPEV)
ggplot(data=Accuracy)+theme_bw()+geom_point(mapping=aes(factor,MAPEF), color = "blue", size = 3)+geom_point(mapping=aes(factor,MAPEV), color = "red", size = 3)




# Second step: establish model
HZ_fit<-ts(HZ[1:(train+validation)], frequency = 12, start = c(2014, 12)) # 2014
HZ_test<-ts(HZ[(train+validation+1):l], frequency = 12, start = c(2019, 12)) # 2019

HZ_model<-Arima(HZ_fit,order = c(0,1,0),seasonal = c(0,1,1))
HZ_model
HZ_model_APEF<-abs(HZ_model$residuals) / HZ_fit *100
HZ_model_MAPEF<-mean(HZ_model_APEF)


HZ_model_test<-forecast(HZ_model,h=test)
HZ_model_APET<-abs(HZ_model_test$mean-HZ_test) / HZ_test *100
HZ_model_MAPET<-mean(HZ_model_APET)
plot(HZ_model_test)
HZ_result<-c(HZ_model$fitted,HZ_model_test$mean)






# hefei time series

HF<-CSJ_PM25$HF
HF_ts<-ts(HF, frequency = 12, start = c(2014, 12))
HF_fit<-ts(HF[1:train], frequency = 12, start = c(2014, 12)) # 2014
HF_val<-ts(HF[(train+1):(train+validation)], frequency = 12, start = c(2018, 12)) # 2018


# diff
HF_fit_diff12<-diff(HF_fit,lag=12)
plot(HF_fit_diff12)
HF_fit_diff12_diff1<-diff(HF_fit_diff12,1)
plot(HF_fit_diff12_diff1)


# First step: order selection

# auto order
HF_arima <- auto.arima(HF_fit)
HF_arima
HF_APEF<-abs(HF_arima$residuals) / HF_fit *100
HF_MAPEF<-mean(HF_APEF)
HF_arima_fore <- forecast(HF_arima, h = validation)
HF_APEV<-abs(HF_arima_fore$mean-HF_val) / HF_val *100
HF_MAPEV<-mean(HF_APEV)
plot(HF_arima_fore)
HF_auto<-c(HF_arima$fitted,HF_arima_fore$mean)

# optimize order
MAPEF<-NULL
MAPEV<-NULL
for (i in 1:ncol(order))
{
  HF_order<-Arima(HF_fit,order = order[1:3,i],seasonal = order[4:6,i])
  HF_order
  HF_order_APEF<-abs(HF_order$residuals) / HF_fit *100
  HF_order_MAPEF<-mean(HF_order_APEF)
  
  
  HF_order_val<-forecast(HF_order,h=validation)
  HF_order_APEV<-abs(HF_order_val$mean-HF_val) / HF_val *100
  HF_order_MAPEV<-mean(HF_order_APEV)
  
  MAPEF<-c(MAPEF,HF_order_MAPEF)
  MAPEV<-c(MAPEV,HF_order_MAPEV)
}

# plot MAPEF and MAPEV
Accuracy<-data.frame(factor,MAPEF,MAPEV)
ggplot(data=Accuracy)+theme_bw()+geom_point(mapping=aes(factor,MAPEF), color = "blue", size = 3)+geom_point(mapping=aes(factor,MAPEV), color = "red", size = 3)




# Second step: establish model
HF_fit<-ts(HF[1:(train+validation)], frequency = 12, start = c(2014, 12)) # 2014
HF_test<-ts(HF[(train+validation+1):l], frequency = 12, start = c(2019, 12)) # 2019

HF_model<-Arima(HF_fit,order = c(0,1,1),seasonal = c(0,1,1))
HF_model
HF_model_APEF<-abs(HF_model$residuals) / HF_fit *100
HF_model_MAPEF<-mean(HF_model_APEF)


HF_model_test<-forecast(HF_model,h=test)
HF_model_APET<-abs(HF_model_test$mean-HF_test) / HF_test *100
HF_model_MAPET<-mean(HF_model_APET)
plot(HF_model_test)
HF_result<-c(HF_model$fitted,HF_model_test$mean)




# data export
CSJ_ARIMA<-data.frame(SH_result,NJ_result,HZ_result,HF_result)
CSJ_ARIMA
# write.csv(CSJ_ARIMA,"../benchmark_other_model_data/PM25_ARIMA.csv")


# return to current_path
setwd(current_path) 
