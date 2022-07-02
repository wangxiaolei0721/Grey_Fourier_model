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
l<-nrow(CSJ_PM25)
test<-24
validation<-12
train<-l-test-validation



# shanghai time series
SH<-CSJ_PM25$SH
SH_ts<-ts(SH, frequency = 12, start = c(2014, 12))
SH_fit<-ts(SH[1:train], frequency = 12, start = c(2014, 12)) # 2014
SH_val<-ts(SH[(train+1):(train+validation)], frequency = 12, start = c(2018, 12)) # 2018


# establish neural net
set.seed(1)



# auto order
SH_net <- nnetar(SH_fit,size = 3)
SH_net
SH_APEF<-abs(SH_net$residuals) / SH_fit *100
SH_MAPEF<-mean(SH_APEF,na.rm = TRUE)
SH_net_fore <- forecast(SH_net, h = validation)
# absolute percentage error
SH_APET<-abs(SH_net_fore$mean-SH_val) / SH_val *100
SH_MAPET<-mean(SH_APET,na.rm = TRUE)
plot(SH_net_fore)
SH_auto<-c(SH_net$fitted,SH_net_fore$mean)
SH_auto


# optimize order
p<-c(1:6)  # autocorrelation order
k<-c(3:6)  # hidden nodes

factor<-NULL
MAPEF<-NULL
MAPEV<-NULL


for (i in 1:length(p))
{
  for (j in 1:length(k))
  {
    factor<-c(factor,paste(p[i],k[j],sep=""))
    SH_order <- nnetar(SH_fit,p=p[i],P=1,size = k[j]) # seasonal lag = 1
    SH_order_APEF<-abs(SH_order$residuals) / SH_fit *100
    SH_order_MAPEF<-mean(SH_order_APEF,na.rm = TRUE)
    # validation
    SH_order_val <- forecast(SH_order, h = validation)
    # absolute percentage error
    SH_order_APEV<-abs(SH_order_val$mean-SH_val) / SH_val *100
    SH_order_MAPEV<-mean(SH_order_APEV,na.rm = TRUE)
    # store data
    MAPEF<-c(MAPEF,SH_order_MAPEF)
    MAPEV<-c(MAPEV,SH_order_MAPEV)
  }
}



# plot MAPEF and MAPEV
Accuracy<-data.frame(factor,MAPEF,MAPEV)
ggplot(data=Accuracy)+theme_bw()+geom_point(mapping=aes(factor,MAPEF), color = "blue", size = 3)+geom_point(mapping=aes(factor,MAPEV), color = "red", size = 3)



# Second step: establish model
SH_fit<-ts(SH[1:(train+validation)], frequency = 12, start = c(2014, 12)) # 2014
SH_test<-ts(SH[((train+validation)+1):l], frequency = 12, start = c(2019, 12)) # 2019

SH_model <- nnetar(SH_fit,p=1,P=1,size = 4)
SH_model_APEF<-abs(SH_model$residuals) / SH_fit *100
SH_model_MAPEF<-mean(SH_model_APEF,na.rm = TRUE)


# test
SH_model_test <- forecast(SH_model, h = test)
# absolute percentage error
SH_model_APET<-abs(SH_model_test$mean-SH_test) / SH_test *100
SH_model_MAPET<-mean(SH_model_APET,na.rm = TRUE)
plot(SH_model_test)
SH_result<-c(SH_model$fitted,SH_model_test$mean)





# nanjing time series
NJ<-CSJ_PM25$NJ
NJ_ts<-ts(NJ, frequency = 12, start = c(2014, 12))
NJ_fit<-ts(NJ[1:train], frequency = 12, start = c(2014, 12)) # 2014
NJ_val<-ts(NJ[(train+1):(train+validation)], frequency = 12, start = c(2018, 12)) # 2018


# establish neural net
set.seed(1)


# auto order
NJ_net <- nnetar(NJ_fit,size = 3)
NJ_net
NJ_APEF<-abs(NJ_net$residuals) / NJ_fit *100
NJ_MAPEF<-mean(NJ_APEF,na.rm = TRUE)
NJ_net_fore <- forecast(NJ_net, h = validation)
# absolute percentage error
NJ_APET<-abs(NJ_net_fore$mean-NJ_val) / NJ_val *100
NJ_MAPET<-mean(NJ_APET,na.rm = TRUE)
plot(NJ_net_fore)
NJ_auto<-c(NJ_net$fitted,NJ_net_fore$mean)
NJ_auto


# optimize order
p<-c(1:6)
k<-c(3:6)

factor<-NULL
MAPEF<-NULL
MAPEV<-NULL


for (i in 1:length(p))
{
  for (j in 1:length(k))
  {
    factor<-c(factor,paste(p[i],k[j],sep=""))
    NJ_order <- nnetar(NJ_fit,p=p[i],P=1,size = k[j]) # seasonal lag = 1
    NJ_order_APEF<-abs(NJ_order$residuals) / NJ_fit *100
    NJ_order_MAPEF<-mean(NJ_order_APEF,na.rm = TRUE)
    # validation
    NJ_order_val <- forecast(NJ_order, h = validation)
    # absolute percentage error
    NJ_order_APEV<-abs(NJ_order_val$mean-NJ_val) / NJ_val *100
    NJ_order_MAPEV<-mean(NJ_order_APEV,na.rm = TRUE)
    # store data
    MAPEF<-c(MAPEF,NJ_order_MAPEF)
    MAPEV<-c(MAPEV,NJ_order_MAPEV)
  }
}



# plot MAPEF and MAPEV
Accuracy<-data.frame(factor,MAPEF,MAPEV)
ggplot(data=Accuracy)+theme_bw()+geom_point(mapping=aes(factor,MAPEF), color = "blue", size = 3)+geom_point(mapping=aes(factor,MAPEV), color = "red", size = 3)



# Second step: establish model
NJ_fit<-ts(NJ[1:(train+validation)], frequency = 12, start = c(2014, 12)) # 2014
NJ_test<-ts(NJ[((train+validation)+1):l], frequency = 12, start = c(2019, 12)) # 2019

NJ_model <- nnetar(NJ_fit,p=1,P=1,size = 3)
NJ_model_APEF<-abs(NJ_model$residuals) / NJ_fit *100
NJ_model_MAPEF<-mean(NJ_model_APEF,na.rm = TRUE)


# test
NJ_model_test <- forecast(NJ_model, h = test)
# absolute percentage error
NJ_model_APET<-abs(NJ_model_test$mean-NJ_test) / NJ_test *100
NJ_model_MAPET<-mean(NJ_model_APET,na.rm = TRUE)
plot(NJ_model_test)
NJ_result<-c(NJ_model$fitted,NJ_model_test$mean)




# hangzhou time series
HZ<-CSJ_PM25$HZ
HZ_ts<-ts(HZ, frequency = 12, start = c(2014, 12))
HZ_fit<-ts(HZ[1:train], frequency = 12, start = c(2014, 12)) # 2014
HZ_val<-ts(HZ[(train+1):(train+validation)], frequency = 12, start = c(2018, 12)) # 2018


# establish neural net
set.seed(1)



# auto order
HZ_net <- nnetar(HZ_fit,size = 3)
HZ_net
HZ_APEF<-abs(HZ_net$residuals) / HZ_fit *100
HZ_MAPEF<-mean(HZ_APEF,na.rm = TRUE)
HZ_net_fore <- forecast(HZ_net, h = validation)
# absolute percentage error
HZ_APET<-abs(HZ_net_fore$mean-HZ_val) / HZ_val *100
HZ_MAPET<-mean(HZ_APET,na.rm = TRUE)
plot(HZ_net_fore)
HZ_auto<-c(HZ_net$fitted,HZ_net_fore$mean)
HZ_auto


# optimize order
p<-c(1:6)
k<-c(3:6)

factor<-NULL
MAPEF<-NULL
MAPEV<-NULL


for (i in 1:length(p))
{
  for (j in 1:length(k))
  {
    factor<-c(factor,paste(p[i],k[j],sep=""))
    HZ_order <- nnetar(HZ_fit,p=p[i],P=1,size = k[j]) # seasonal lag = 1
    HZ_order_APEF<-abs(HZ_order$residuals) / HZ_fit *100
    HZ_order_MAPEF<-mean(HZ_order_APEF,na.rm = TRUE)
    # validation
    HZ_order_val <- forecast(HZ_order, h = validation)
    # absolute percentage error
    HZ_order_APEV<-abs(HZ_order_val$mean-HZ_val) / HZ_val *100
    HZ_order_MAPEV<-mean(HZ_order_APEV,na.rm = TRUE)
    # store data
    MAPEF<-c(MAPEF,HZ_order_MAPEF)
    MAPEV<-c(MAPEV,HZ_order_MAPEV)
  }
}



# plot MAPEF and MAPEV
Accuracy<-data.frame(factor,MAPEF,MAPEV)
ggplot(data=Accuracy)+theme_bw()+geom_point(mapping=aes(factor,MAPEF), color = "blue", size = 3)+geom_point(mapping=aes(factor,MAPEV), color = "red", size = 3)



# Second step: establish model
HZ_fit<-ts(HZ[1:(train+validation)], frequency = 12, start = c(2014, 12)) # 2014
HZ_test<-ts(HZ[((train+validation)+1):l], frequency = 12, start = c(2019, 12)) # 2019

HZ_model <- nnetar(HZ_fit,p=1,P=1,size = 3)
HZ_model_APEF<-abs(HZ_model$residuals) / HZ_fit *100
HZ_model_MAPEF<-mean(HZ_model_APEF,na.rm = TRUE)


# test
HZ_model_test <- forecast(HZ_model, h = test)
# absolute percentage error
HZ_model_APET<-abs(HZ_model_test$mean-HZ_test) / HZ_test *100
HZ_model_MAPET<-mean(HZ_model_APET,na.rm = TRUE)
plot(HZ_model_test)
HZ_result<-c(HZ_model$fitted,HZ_model_test$mean)





# hefei time series
HF<-CSJ_PM25$HF
HF_ts<-ts(HF, frequency = 12, start = c(2014, 12))
HF_fit<-ts(HF[1:train], frequency = 12, start = c(2014, 12)) # 2014
HF_val<-ts(HF[(train+1):(train+validation)], frequency = 12, start = c(2018, 12)) # 2018


# establish neural net
set.seed(1)



# auto order
HF_net <- nnetar(HF_fit,size = 3)
HF_net
HF_APEF<-abs(HF_net$residuals) / HF_fit *100
HF_MAPEF<-mean(HF_APEF,na.rm = TRUE)
HF_net_fore <- forecast(HF_net, h = validation)
# absolute percentage error
HF_APET<-abs(HF_net_fore$mean-HF_val) / HF_val *100
HF_MAPET<-mean(HF_APET,na.rm = TRUE)
plot(HF_net_fore)
HF_auto<-c(HF_net$fitted,HF_net_fore$mean)
HF_auto


# optimize order
p<-c(1:6)
k<-c(3:6)

factor<-NULL
MAPEF<-NULL
MAPEV<-NULL


for (i in 1:length(p))
{
  for (j in 1:length(k))
  {
    factor<-c(factor,paste(p[i],k[j],sep=""))  # seasonal lag = 1
    HF_order <- nnetar(HF_fit,p=p[i],P=1,size = k[j])
    HF_order_APEF<-abs(HF_order$residuals) / HF_fit *100
    HF_order_MAPEF<-mean(HF_order_APEF,na.rm = TRUE)
    # validation
    HF_order_val <- forecast(HF_order, h = validation)
    # absolute percentage error
    HF_order_APEV<-abs(HF_order_val$mean-HF_val) / HF_val *100
    HF_order_MAPEV<-mean(HF_order_APEV,na.rm = TRUE)
    # store data
    MAPEF<-c(MAPEF,HF_order_MAPEF)
    MAPEV<-c(MAPEV,HF_order_MAPEV)
  }
}



# plot MAPEF and MAPEV
Accuracy<-data.frame(factor,MAPEF,MAPEV)
ggplot(data=Accuracy)+theme_bw()+geom_point(mapping=aes(factor,MAPEF), color = "blue", size = 3)+geom_point(mapping=aes(factor,MAPEV), color = "red", size = 3)



# Second step: establish model
HF_fit<-ts(HF[1:(train+validation)], frequency = 12, start = c(2014, 12)) # 2014
HF_test<-ts(HF[((train+validation)+1):l], frequency = 12, start = c(2019, 12)) # 2019

HF_model <- nnetar(HF_fit,p=2,P=1,size = 3)
HF_model_APEF<-abs(HF_model$residuals) / HF_fit *100
HF_model_MAPEF<-mean(HF_model_APEF,na.rm = TRUE)


# test
HF_model_test <- forecast(HF_model, h = test)
# absolute percentage error
HF_model_APET<-abs(HF_model_test$mean-HF_test) / HF_test *100
HF_model_MAPET<-mean(HF_model_APET,na.rm = TRUE)
plot(HF_model_test)
HF_result<-c(HF_model$fitted,HF_model_test$mean)



# data export
CSJ_net<-data.frame(SH_result,NJ_result,HZ_result,HF_result)
CSJ_net
write.csv(CSJ_net,"../benchmark_other_model_data/PM25_net.csv")


# return to current_path
setwd(current_path) 

