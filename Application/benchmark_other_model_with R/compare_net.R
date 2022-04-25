# clear work space and screen
rm(list=ls()) # clear data and value
shell("cls") # clear screen
gc() # clear memory


# load package
library(forecast) # packages for neural net
library(readr)
library(ggplot2)



# set script path
current_path<-getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# read data from .txt document
CSJ_PM25 <- read_csv("./CSJ_PM25.txt",col_names = TRUE,show_col_types = FALSE)
# View(CSJ_PM25) view data



# train and test
l<-nrow(CSJ_PM25);
test<-24
train<-l-test



# shanghai time series
SH<-CSJ_PM25$SH
SH_ts<-ts(SH, frequency = 12, start = c(2014, 12))
SH_fit<-ts(SH[1:train], frequency = 12, start = c(2014, 12)) # 2014
SH_test<-ts(SH[train+1:l], frequency = 12, start = c(2019, 12)) # 2019



# establish neural net
set.seed(20)
# training 
# auto order
SH_net <- nnetar(SH_fit,size = 3,lambda="auto")
SH_net
SH_net_fit <- forecast(SH_net)
SH_APEF<-abs(SH_net$residuals) / SH_fit *100
SH_MAPEF<-mean(SH_APEF,na.rm = TRUE)
# testing 
SH_net_fore <- forecast(SH_net_fit, h = test)
# absolute percentage error
SH_APET<-abs(SH_net_fore$mean-SH_test) / SH_test *100
SH_MAPET<-mean(SH_APET,na.rm = TRUE)
plot(SH_net_fore)
SH.fit<-c(SH_net_fit$fitted,SH_net_fore$mean)
SH.fit




# nanjing time series
NJ<-CSJ_PM25$NJ
NJ_ts<-ts(NJ, frequency = 12, start = c(2014, 12))
NJ_fit<-ts(NJ[1:train], frequency = 12, start = c(2014, 12)) # 2014
NJ_test<-ts(NJ[train+1:l], frequency = 12, start = c(2019, 12)) # 2019


# establish neural net
set.seed(20)
# establish neural net
# auto order
NJ_net <- nnetar(NJ_fit,size = 3,lambda="auto") 
NJ_net
NJ_net_fit <- forecast(NJ_net )
NJ_APEF<-abs(NJ_net_fit$fitted-NJ_fit) / NJ_fit *100
NJ_MAPEF<-mean(NJ_APEF,na.rm = TRUE)
# testing 
NJ_net_fore <- forecast(NJ_net_fit, h = test)
# absolute percentage error
NJ_APET<-abs(NJ_net_fore$mean-NJ_test) / NJ_test *100
NJ_MAPET<-mean(NJ_APET,na.rm = TRUE)
plot(NJ_net_fore)
NJ.fit<-c(NJ_net_fit$fitted,NJ_net_fore$mean)
NJ.fit



# hangzhou time series
HZ<-CSJ_PM25$HZ
HZ_ts<-ts(HZ, frequency = 12, start = c(2014, 12))
HZ_fit<-ts(HZ[1:train], frequency = 12, start = c(2014, 12)) # 2014
HZ_test<-ts(HZ[train+1:l], frequency = 12, start = c(2019, 12)) # 2019


# establish neural net
set.seed(20)
# training 
# auto order
HZ_net <- nnetar(HZ_fit,size = 3,lambda="auto")
HZ_net
HZ_net_fit <- forecast(HZ_net )
HZ_APEF<-abs(HZ_net_fit$fitted-HZ_fit) / HZ_fit *100
HZ_MAPEF<-mean(HZ_APEF,na.rm = TRUE)
# testing 
HZ_net_fore <- forecast(HZ_net_fit, h = test)
# absolute percentage error
HZ_APET<-abs(HZ_net_fore$mean-HZ_test) / HZ_test *100
HZ_MAPET<-mean(HZ_APET,na.rm = TRUE)
plot(HZ_net_fore)
HZ.fit<-c(HZ_net_fit$fitted,HZ_net_fore$mean)
HZ.fit




# hefei time series
HF<-CSJ_PM25$HF
HF_ts<-ts(HF, frequency = 12, start = c(2014, 12))
HF_fit<-ts(HF[1:train], frequency = 12, start = c(2014, 12)) # Jan of 2015
HF_test<-ts(HF[train+1:l], frequency = 12, start = c(2019, 12)) # Jan of 2020


# establish neural net
set.seed(20)
# training 
# auto order
HF_net <- nnetar(HF_fit,size = 3,lambda="auto")
HF_net
HF_net_fit <- forecast(HF_net)
HF_APEF<-abs(HF_net_fit$fitted-HF_fit) / HF_fit *100
HF_MAPEF<-mean(HF_APEF,na.rm = TRUE)
# testing 
HF_net_fore <- forecast(HF_net_fit, h = test)
# absolute percentage error
HF_APET<-abs(HF_net_fore$mean-HF_test) / HF_test *100
HF_MAPET<-mean(HF_APET,na.rm = TRUE)
plot(HF_net_fore)
HF.fit<-c(HF_net_fit$fitted,HF_net_fore$mean)
HF.fit



# data export
CSJ_ARIMA<-data.frame(SH.fit,NJ.fit,HZ.fit,HF.fit)
CSJ_ARIMA
write.csv(CSJ_ARIMA,"../benchmark_other_model_data/PM25_Net.csv")


# return to current_path
setwd(current_path) 

