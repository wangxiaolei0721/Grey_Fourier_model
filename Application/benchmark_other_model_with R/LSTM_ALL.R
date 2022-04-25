# clear work space and screen
rm(list=ls()) # clear data and value
shell("cls") # clear screen
gc() # clear memory


# packages for Long short term memory
library(keras)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(readr)
library(ggplot2)


# set script path
current_path<-getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# read data from .txt document
CSJ_PM25 <- read_csv("./CSJ_PM25.txt",col_names = TRUE,show_col_types = FALSE)
# View(CSJ_PM25) view data



# load function
source("./LSTM.R")



# shanghai time series
SH_lag<-5
SH_seasonal_lag<-1
test_size<-24
set.seed(1)
SH<-CSJ_PM25$SH
SH_fit<-LSTM(SH,SH_lag,SH_seasonal_lag,test_size)
SH.fit<-c(rep(NA,11+SH_seasonal_lag),SH_fit)
plot(SH.fit)


# nanjing time series
NJ_lag<-3
NJ_seasonal_lag<-1
set.seed(1)
NJ<-CSJ_PM25$NJ
NJ_fit<-LSTM(NJ,NJ_lag,NJ_seasonal_lag,test_size)
NJ.fit<-c(rep(NA,11+NJ_seasonal_lag),NJ_fit)
plot(NJ.fit)



# hangzhou time series
HZ_lag<-11
HZ_seasonal_lag<-1
set.seed(1)
HZ<-CSJ_PM25$HZ
HZ_fit<-LSTM(HZ,HZ_lag,HZ_seasonal_lag,test_size)
HZ.fit<-c(rep(NA,11+HZ_seasonal_lag),HZ_fit)
plot(SH.fit)

# hefei time series
HF_lag<-11
HF_seasonal_lag<-1
set.seed(1)
HF<-CSJ_PM25$HF
HF_fit<-LSTM(HF,HF_lag,HF_seasonal_lag,test_size)
HF.fit<-c(rep(NA,11+HF_seasonal_lag),HF_fit)
plot(HF.fit)


# data export
CSJ_ARIMA<-data.frame(SH.fit,NJ.fit,HZ.fit,HF.fit)
CSJ_ARIMA
write.csv(CSJ_ARIMA,"../benchmark_other_model_data/PM25_LSTM.csv")

# return to current_path
setwd(current_path) 

