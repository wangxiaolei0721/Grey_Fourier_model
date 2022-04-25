# clear work space and screen
rm(list=ls()) # clear data and value
shell("cls") # clear screen
gc() # clear memory

# load package
library(forecast) # packages for Holt-Winter
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



# shanghai holt-winter model
SH_hw<-ets(SH_fit)
SH_hw
SH_APEF<-abs(SH_hw$residuals) / SH_fit *100
SH_MAPEF<-mean(SH_APEF)
SH_hw_fore<-forecast(SH_hw,test)
SH_APET<-abs(SH_hw_fore$mean-SH_test) / SH_test *100
SH_MAPET<-mean(SH_APET)
plot(SH_hw_fore)
SH.fit<-c(SH_hw$fitted,SH_hw_fore$mean)



# nanjing time series
NJ<-CSJ_PM25$NJ
NJ_ts<-ts(NJ, frequency = 12, start = c(2014, 12))
NJ_fit<-ts(NJ[1:train], frequency = 12, start = c(2014, 12)) # 2014
NJ_test<-ts(NJ[train+1:l], frequency = 12, start = c(2019, 12)) # 2019


# nanjing holt-winter model
NJ_hw<-ets(NJ_fit)
NJ_hw
NJ_APEF<-abs(NJ_hw$residuals) / NJ_fit *100
NJ_MAPEF<-mean(NJ_APEF)
NJ_hw_fore<-forecast(NJ_hw,test)
NJ_APET<-abs(NJ_hw_fore$mean-NJ_test) / NJ_test *100
NJ_MAPET<-mean(NJ_APET)
plot(NJ_hw_fore)
NJ.fit<-c(NJ_hw$fitted,NJ_hw_fore$mean)



# hangzhou time series
HZ<-CSJ_PM25$HZ
HZ_ts<-ts(HZ, frequency = 12, start = c(2014, 12))
HZ_fit<-ts(HZ[1:train], frequency = 12, start = c(2014, 12)) # 2014
HZ_test<-ts(HZ[train+1:l], frequency = 12, start = c(2019, 12)) # 2019



# hangzhou holt-winter model
HZ_hw<-ets(HZ_fit)
HZ_hw
HZ_APEF<-abs(HZ_hw$residuals) / HZ_fit *100
HZ_MAPEF<-mean(HZ_APEF)
HZ_hw_fore<-forecast(HZ_hw,test)
HZ_APET<-abs(HZ_hw_fore$mean-HZ_test) / HZ_test *100
HZ_MAPET<-mean(HZ_APET)
plot(HZ_hw_fore)
HZ.fit<-c(HZ_hw$fitted,HZ_hw_fore$mean)



# hefei time series
HF<-CSJ_PM25$HF
HF_ts<-ts(HF, frequency = 12, start = c(2014, 12))
HF_fit<-ts(HF[1:train], frequency = 12, start = c(2014, 12)) # Jan of 2015
HF_test<-ts(HF[train+1:l], frequency = 12, start = c(2019, 12)) # Jan of 2020


# hefei holt-winter model
HF_hw<-ets(HF_fit)
HF_APEF<-abs(HF_hw$residuals) / HF_fit *100
HF_MAPEF<-mean(HF_APEF)
HF_hw_fore<-forecast(HF_hw,test)
HF_APET<-abs(HF_hw_fore$mean-HF_test) / HF_test *100
HF_MAPET<-mean(HF_APET)
plot(HF_hw_fore)
HF.fit<-c(HF_hw$fitted,HF_hw_fore$mean)



# data export
CSJ_ARIMA<-data.frame(SH.fit,NJ.fit,HZ.fit,HF.fit)
CSJ_ARIMA
write.csv(CSJ_ARIMA,"../benchmark_other_model_data/PM25_HW.csv")

# return to current_path
setwd(current_path) 
