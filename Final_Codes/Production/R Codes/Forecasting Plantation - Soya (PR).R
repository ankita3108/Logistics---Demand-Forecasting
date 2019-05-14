## To clear the screen
rm(list=ls())

library(forecast)
library(tseries)
library(imputeTS)

setwd("..")
setwd("..")
setwd("..")
path<-paste0(getwd())
print(path)
path1<-paste0(path,"/Production/Data Directory/")
path2<-paste0(path,"/Production/Data Directory/Soya Plantation Input Files/")
path3<-paste0(path,"/Production/Data Directory/Soya Plantation Output/")
path4<-paste0(path,"/Production/Data Directory/Plantation Plots/")

data_dir=function()
{
  setwd(path1)
}

set_input_dir=function()
{
  setwd(path2)
}

set_output_dir=function()
{
  setwd(path3)
}

set_plot_dir=function()
{
  setwd(path4)
}

data_dir()
year_variables=read.csv("year_variables.csv",header = FALSE)

max_year=year_variables$V2[1]
min_year=year_variables$V2[2]
max_month=year_variables$V2[3]
min_month=year_variables$V2[4]
pred_year=year_variables$V2[5]

data_points=(max_year-min_year)*12-(min_month-1)+max_month

max_len=((pred_year-max_year)*12)+(12-max_month)

total_data_points=data_points+max_len

## Reading the data from csv files
set_input_dir()
data <- read.csv("Soya_PR.csv", header = T, stringsAsFactors = FALSE)
str(data)

data$Plantation<-data$Plantation+1

log_data<-log(data[,c(2:4)])

sum(is.na(log_data))

## Function to scale data
scale_mm<-function(x)
{
  a<-(x-min(x))/(max(x)-min(x))
  return(a)
}

## Function to unscale data
unscale<-function(x,a)
{
  p<-(x*(max(a)-min(a)))+min(a)
  return(p)
}

scaled_data<-apply(log_data,2,scale_mm)

## Prediction for Future
train_pred<-scaled_data[c(1:data_points),]
test_pred<-scaled_data[c((data_points+1):total_data_points),]

## Creating the time series
ts_pred=ts(train_pred[,c(1)],start=c(min_year,1),frequency=12)

## Manual Arima Fit

#ARIMAfit <- arima(ts_pred,order=c(0,0,0),seasonal=list(order=c(2,1,0),period=12),xreg = train_pred[,c(2:3)])
ARIMAfit <- auto.arima(ts_pred,xreg = train_pred[,c(2:3)])

summary(ARIMAfit)

pred_test <- forecast(ARIMAfit,h=max_len,xreg = test_pred[,c(2:3)])
df_test=data.frame(pred_test)

## Unscaling the data
unscaled_test<-unscale(df_test$Point.Forecast,log_data)

## Unlogging the unscaled data
unlog_test<-exp(unscaled_test)
final_test=unlog_test-1

## Getting the fitted values
train_fit<-fitted(ARIMAfit)
unscaled_train<-unscale(train_fit,log_data)
unlog_train<-exp(unscaled_train)
unlog_train=unlog_train-1

## combination of training fitted and test values
final_predicted<-c(unlog_train,final_test)

## Creating time series for fitted data
pred=ts(final_predicted,start=c(min_year,1),frequency=12)

## Plotting the original and validated dataset
org_ts <- ts(data[,c(2)],start=c(min_year,1),frequency=12)
set_plot_dir()
jpeg('Soya_PR.jpg')
plot(org_ts,ylab="Plantation ('000 ha)", main="Plantation Forecast - Soya (PR)")
lines(pred,col="red")
dev.off()

dataframe=as.data.frame(final_predicted)
colnames(dataframe)<-c("Predicted Plantation")
Date<-data$Date
dataframe<-cbind(Date,dataframe)
dataframe$`Predicted Plantation`<-ifelse(dataframe$`Predicted Plantation`<0,0,dataframe$`Predicted Plantation`)

set_output_dir()
write.csv(dataframe,file = "Predicted-Plantation_PR.csv",row.names=FALSE)