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
l<-nrow(CSJ_PM25)
test<-24
validation<-12
train<-l-test-validation



# shanghai time series
SH<-CSJ_PM25$SH
SH_ts<-ts(SH, frequency = 12, start = c(2014, 12))
SH_fit<-ts(SH[1:train], frequency = 12, start = c(2014, 12)) # 2014
SH_val<-ts(SH[(train+1):(train+validation)], frequency = 12, start = c(2018, 12)) # 2018


# shanghai holt-winter model

# First step: structure selection

# model = "AAA"
SH_AAA<-ets(SH_fit,model = "AAA")
SH_AAA
SH_AAA_APEF<-abs(SH_AAA$residuals) / SH_fit *100
SH_AAA_MAPEF<-mean(SH_AAA_APEF)


SH_AAA_val<-forecast(SH_AAA,validation)
SH_AAA_APEV<-abs(SH_AAA_val$mean-SH_val) / SH_val *100
SH_AAA_MAPEV<-mean(SH_AAA_APEV)


# model = "AAA" & damped trend = TRUE
SH_AAdA<-ets(SH_fit,model = "AAA",damped = TRUE)
SH_AAdA
SH_AAdA_APEF<-abs(SH_AAdA$residuals) / SH_fit *100
SH_AAdA_MAPEF<-mean(SH_AAdA_APEF)


SH_AAdA_val<-forecast(SH_AAdA,validation)
SH_AAdA_APEV<-abs(SH_AAdA_val$mean-SH_val) / SH_val *100
SH_AAdA_MAPEV<-mean(SH_AAdA_APEV)


# model = "MAM"
SH_MAM<-ets(SH_fit,model = "MAM")
SH_MAM
SH_MAM_APEF<-abs(SH_MAM$residuals) / SH_fit *100
SH_MAM_MAPEF<-mean(SH_MAM_APEF)


SH_MAM_val<-forecast(SH_MAM,validation)
SH_MAM_APEV<-abs(SH_MAM_val$mean-SH_val) / SH_val *100
SH_MAM_MAPEV<-mean(SH_MAM_APEV)


# model = "MAM" & damped trend = TRUE
SH_MAdM<-ets(SH_fit,model = "MAM",damped = TRUE)
SH_MAdM
SH_MAdM_APEF<-abs(SH_MAdM$residuals) / SH_fit *100
SH_MAdM_MAPEF<-mean(SH_MAdM_APEF)


SH_MAdM_val<-forecast(SH_MAdM,validation)
SH_MAdM_APEV<-abs(SH_MAdM_val$mean-SH_val) / SH_val *100
SH_MAdM_MAPEV<-mean(SH_MAdM_APEV)


# plot MAPEF and MAPEV
factor<-c("AAA","AAdA","MAM","MAdM")
MAPEF<-c(SH_AAA_MAPEF,SH_AAdA_MAPEF,SH_MAM_MAPEF,SH_MAdM_MAPEF)
MAPEV<-c(SH_AAA_MAPEV,SH_AAdA_MAPEV,SH_MAM_MAPEV,SH_MAdM_MAPEV)
Accuracy<-data.frame(factor,MAPEF,MAPEV)
ggplot(data=Accuracy)+theme_bw()+geom_point(mapping=aes(factor,MAPEF), color = "blue", size = 3)+geom_point(mapping=aes(factor,MAPEV), color = "red", size = 3)




# Second step: establish model
SH_fit<-ts(SH[1:(train+validation)], frequency = 12, start = c(2014, 12)) # 2014
SH_test<-ts(SH[((train+validation)+1):l], frequency = 12, start = c(2019, 12)) # 2019

SH_model<-ets(SH_fit,model = "AAA",damped = TRUE)
SH_model
SH_APEF<-abs(SH_model$residuals) / SH_fit *100
SH_MAPEF<-mean(SH_APEF)


SH_model_test<-forecast(SH_model,test)
SH_APET<-abs(SH_model_test$mean-SH_test) / SH_test *100
SH_MAPET<-mean(SH_APET)
plot(SH_model_test)
SH_result<-c(SH_model$fitted,SH_model_test$mean)



# nanjing time series
NJ<-CSJ_PM25$NJ
NJ_ts<-ts(NJ, frequency = 12, start = c(2014, 12))
NJ_fit<-ts(NJ[1:train], frequency = 12, start = c(2014, 12)) # 2014
NJ_val<-ts(NJ[(train+1):(train+validation)], frequency = 12, start = c(2018, 12)) # 2018


# nanjing holt-winter model

# First step: structure selection

# model = "AAA"
NJ_AAA<-ets(NJ_fit,model = "AAA")
NJ_AAA
NJ_AAA_APEF<-abs(NJ_AAA$residuals) / NJ_fit *100
NJ_AAA_MAPEF<-mean(NJ_AAA_APEF)


NJ_AAA_val<-forecast(NJ_AAA,validation)
NJ_AAA_APEV<-abs(NJ_AAA_val$mean-NJ_val) / NJ_val *100
NJ_AAA_MAPEV<-mean(NJ_AAA_APEV)


# model = "AAA" & damped trend = TRUE
NJ_AAdA<-ets(NJ_fit,model = "AAA",damped = TRUE)
NJ_AAdA
NJ_AAdA_APEF<-abs(NJ_AAdA$residuals) / NJ_fit *100
NJ_AAdA_MAPEF<-mean(NJ_AAdA_APEF)


NJ_AAdA_val<-forecast(NJ_AAdA,validation)
NJ_AAdA_APEV<-abs(NJ_AAdA_val$mean-NJ_val) / NJ_val *100
NJ_AAdA_MAPEV<-mean(NJ_AAdA_APEV)


# model = "MAM"
NJ_MAM<-ets(NJ_fit,model = "MAM")
NJ_MAM
NJ_MAM_APEF<-abs(NJ_MAM$residuals) / NJ_fit *100
NJ_MAM_MAPEF<-mean(NJ_MAM_APEF)


NJ_MAM_val<-forecast(NJ_MAM,validation)
NJ_MAM_APEV<-abs(NJ_MAM_val$mean-NJ_val) / NJ_val *100
NJ_MAM_MAPEV<-mean(NJ_MAM_APEV)


# model = "MAM" & damped trend = TRUE
NJ_MAdM<-ets(NJ_fit,model = "MAM",damped = TRUE)
NJ_MAdM
NJ_MAdM_APEF<-abs(NJ_MAdM$residuals) / NJ_fit *100
NJ_MAdM_MAPEF<-mean(NJ_MAdM_APEF)


NJ_MAdM_val<-forecast(NJ_MAdM,validation)
NJ_MAdM_APEV<-abs(NJ_MAdM_val$mean-NJ_val) / NJ_val *100
NJ_MAdM_MAPEV<-mean(NJ_MAdM_APEV)


# plot MAPEF and MAPEV
factor<-c("AAA","AAdA","MAM","MAdM")
MAPEF<-c(NJ_AAA_MAPEF,NJ_AAdA_MAPEF,NJ_MAM_MAPEF,NJ_MAdM_MAPEF)
MAPEV<-c(NJ_AAA_MAPEV,NJ_AAdA_MAPEV,NJ_MAM_MAPEV,NJ_MAdM_MAPEV)
Accuracy<-data.frame(factor,MAPEF,MAPEV)
ggplot(data=Accuracy)+theme_bw()+geom_point(mapping=aes(factor,MAPEF), color = "blue", size = 3)+geom_point(mapping=aes(factor,MAPEV), color = "red", size = 3)




# Second step: establish model
NJ_fit<-ts(SH[1:(train+validation)], frequency = 12, start = c(2014, 12)) # 2014
NJ_test<-ts(SH[((train+validation)+1):l], frequency = 12, start = c(2019, 12)) # 2019

NJ_model<-ets(NJ_fit,model = "AAA")
NJ_model
NJ_APEF<-abs(NJ_model$residuals) / NJ_fit *100
NJ_MAPEF<-mean(NJ_APEF)


NJ_model_test<-forecast(NJ_model,test)
NJ_APET<-abs(NJ_model_test$mean-NJ_test) / NJ_test *100
NJ_MAPET<-mean(NJ_APET)
plot(NJ_model_test)
NJ_result<-c(NJ_model$fitted,NJ_model_test$mean)



# hangzhou time series
HZ<-CSJ_PM25$HZ
HZ_ts<-ts(HZ, frequency = 12, start = c(2014, 12))
HZ_fit<-ts(HZ[1:train], frequency = 12, start = c(2014, 12)) # 2014
HZ_val<-ts(HZ[(train+1):(train+validation)], frequency = 12, start = c(2018, 12)) # 2018


# hangzhou holt-winter model

# First step: structure selection

# model = "AAA"
HZ_AAA<-ets(HZ_fit,model = "AAA")
HZ_AAA
HZ_AAA_APEF<-abs(HZ_AAA$residuals) / HZ_fit *100
HZ_AAA_MAPEF<-mean(HZ_AAA_APEF)


HZ_AAA_val<-forecast(HZ_AAA,validation)
HZ_AAA_APEV<-abs(HZ_AAA_val$mean-HZ_val) / HZ_val *100
HZ_AAA_MAPEV<-mean(HZ_AAA_APEV)


# model = "AAA" & damped trend = TRUE
HZ_AAdA<-ets(HZ_fit,model = "AAA",damped = TRUE)
HZ_AAdA
HZ_AAdA_APEF<-abs(HZ_AAdA$residuals) / HZ_fit *100
HZ_AAdA_MAPEF<-mean(HZ_AAdA_APEF)


HZ_AAdA_val<-forecast(HZ_AAdA,validation)
HZ_AAdA_APEV<-abs(HZ_AAdA_val$mean-HZ_val) / HZ_val *100
HZ_AAdA_MAPEV<-mean(HZ_AAdA_APEV)


# model = "MAM"
HZ_MAM<-ets(HZ_fit,model = "MAM")
HZ_MAM
HZ_MAM_APEF<-abs(HZ_MAM$residuals) / HZ_fit *100
HZ_MAM_MAPEF<-mean(HZ_MAM_APEF)


HZ_MAM_val<-forecast(HZ_MAM,validation)
HZ_MAM_APEV<-abs(HZ_MAM_val$mean-HZ_val) / HZ_val *100
HZ_MAM_MAPEV<-mean(HZ_MAM_APEV)


# model = "MAM" & damped trend = TRUE
HZ_MAdM<-ets(HZ_fit,model = "MAM",damped = TRUE)
HZ_MAdM
HZ_MAdM_APEF<-abs(HZ_MAdM$residuals) / HZ_fit *100
HZ_MAdM_MAPEF<-mean(HZ_MAdM_APEF)


HZ_MAdM_val<-forecast(HZ_MAdM,validation)
HZ_MAdM_APEV<-abs(HZ_MAdM_val$mean-HZ_val) / HZ_val *100
HZ_MAdM_MAPEV<-mean(HZ_MAdM_APEV)


# plot MAPEF and MAPEV
factor<-c("AAA","AAdA","MAM","MAdM")
MAPEF<-c(HZ_AAA_MAPEF,HZ_AAdA_MAPEF,HZ_MAM_MAPEF,HZ_MAdM_MAPEF)
MAPEV<-c(HZ_AAA_MAPEV,HZ_AAdA_MAPEV,HZ_MAM_MAPEV,HZ_MAdM_MAPEV)
Accuracy<-data.frame(factor,MAPEF,MAPEV)
ggplot(data=Accuracy)+theme_bw()+geom_point(mapping=aes(factor,MAPEF), color = "blue", size = 3)+geom_point(mapping=aes(factor,MAPEV), color = "red", size = 3)




# Second step: establish model
HZ_fit<-ts(SH[1:(train+validation)], frequency = 12, start = c(2014, 12)) # 2014
HZ_test<-ts(SH[((train+validation)+1):l], frequency = 12, start = c(2019, 12)) # 2019

HZ_model<-ets(HZ_fit,model = "AAA",damped = TRUE)
HZ_model
HZ_APEF<-abs(HZ_model$residuals) / HZ_fit *100
HZ_MAPEF<-mean(HZ_APEF)


HZ_model_test<-forecast(HZ_model,test)
HZ_APET<-abs(HZ_model_test$mean-HZ_test) / HZ_test *100
HZ_MAPET<-mean(HZ_APET)
plot(HZ_model_test)
HZ_result<-c(HZ_model$fitted,HZ_model_test$mean)




# hefei time series
HF<-CSJ_PM25$HF
HF_ts<-ts(HF, frequency = 12, start = c(2014, 12))
HF_fit<-ts(HF[1:train], frequency = 12, start = c(2014, 12)) # 2014
HF_val<-ts(HF[(train+1):(train+validation)], frequency = 12, start = c(2018, 12)) # 2018


# hefei holt-winter model

# First step: structure selection

# model = "AAA"
HF_AAA<-ets(HF_fit,model = "AAA")
HF_AAA
HF_AAA_APEF<-abs(HF_AAA$residuals) / HF_fit *100
HF_AAA_MAPEF<-mean(HF_AAA_APEF)


HF_AAA_val<-forecast(HF_AAA,validation)
HF_AAA_APEV<-abs(HF_AAA_val$mean-HF_val) / HF_val *100
HF_AAA_MAPEV<-mean(HF_AAA_APEV)


# model = "AAA" & damped trend = TRUE
HF_AAdA<-ets(HF_fit,model = "AAA",damped = TRUE)
HF_AAdA
HF_AAdA_APEF<-abs(HF_AAdA$residuals) / HF_fit *100
HF_AAdA_MAPEF<-mean(HF_AAdA_APEF)


HF_AAdA_val<-forecast(HF_AAdA,validation)
HF_AAdA_APEV<-abs(HF_AAdA_val$mean-HF_val) / HF_val *100
HF_AAdA_MAPEV<-mean(HF_AAdA_APEV)


# model = "MAM"
HF_MAM<-ets(HF_fit,model = "MAM")
HF_MAM
HF_MAM_APEF<-abs(HF_MAM$residuals) / HF_fit *100
HF_MAM_MAPEF<-mean(HF_MAM_APEF)


HF_MAM_val<-forecast(HF_MAM,validation)
HF_MAM_APEV<-abs(HF_MAM_val$mean-HF_val) / HF_val *100
HF_MAM_MAPEV<-mean(HF_MAM_APEV)


# model = "MAM" & damped trend = TRUE
HF_MAdM<-ets(HF_fit,model = "MAM",damped = TRUE)
HF_MAdM
HF_MAdM_APEF<-abs(HF_MAdM$residuals) / HF_fit *100
HF_MAdM_MAPEF<-mean(HF_MAdM_APEF)


HF_MAdM_val<-forecast(HF_MAdM,validation)
HF_MAdM_APEV<-abs(HF_MAdM_val$mean-HF_val) / HF_val *100
HF_MAdM_MAPEV<-mean(HF_MAdM_APEV)


# plot MAPEF and MAPEV
factor<-c("AAA","AAdA","MAM","MAdM")
MAPEF<-c(HF_AAA_MAPEF,HF_AAdA_MAPEF,HF_MAM_MAPEF,HF_MAdM_MAPEF)
MAPEV<-c(HF_AAA_MAPEV,HF_AAdA_MAPEV,HF_MAM_MAPEV,HF_MAdM_MAPEV)
Accuracy<-data.frame(factor,MAPEF,MAPEV)
ggplot(data=Accuracy)+theme_bw()+geom_point(mapping=aes(factor,MAPEF), color = "blue", size = 3)+geom_point(mapping=aes(factor,MAPEV), color = "red", size = 3)




# Second step: establish model
HF_fit<-ts(SH[1:(train+validation)], frequency = 12, start = c(2014, 12)) # 2014
HF_test<-ts(SH[((train+validation)+1):l], frequency = 12, start = c(2019, 12)) # 2019

HF_model<-ets(HF_fit,model = "AAA",damped = TRUE)
HF_model
HF_APEF<-abs(HF_model$residuals) / HF_fit *100
HF_MAPEF<-mean(HF_APEF)


HF_model_test<-forecast(HF_model,test)
HF_APET<-abs(HF_model_test$mean-HF_test) / HF_test *100
HF_MAPET<-mean(HF_APET)
plot(HF_model_test)
HF_result<-c(HF_model$fitted,HF_model_test$mean)




# data export
CSJ_es<-data.frame(SH_result,NJ_result,HZ_result,HF_result)
CSJ_es
# write.csv(CSJ_es,"../benchmark_other_model_data/PM25_es.csv")


# return to current_path
setwd(current_path) 
