## To clear the screen
rm(list=ls())

## Importing required libraries
library(forecast)
library(tseries)
library(imputeTS)
library(Amelia)
library(dplyr)
library(tibble)

## library imported to standardize the dataset
library(vegan)

## Setting the paths
setwd("..")
setwd("..")
setwd("..")
setwd("..")
path<-paste0(getwd())
path1<-paste0(path,"/Export/Data Directory/Soya_Bran")
path2<-paste0(path,"/Export/Data Directory/Soya_Bran/Plots - Export/")
path3<-paste0(path,"/Export/Data Directory/Soya_Bran/covariates_results/")
path4<-paste0(path,"/Production/Data Directory/")
path5<-paste0(path,"/Export/Data Directory/Soya_Bran/Predicted-Export/")
path6<-paste0(path,"/Export/Data Directory/Soya_Bran/Confidence-Results/")

set_production_dir=function()
{
  setwd(path4)
}

set_input_dir=function()
{
  setwd(path1)
}

set_covariate_dir=function()
{
  setwd(path3)
}

set_plot_dir=function()
{
  setwd(path2)
}

set_predicted_dir=function()
{
  setwd(path5)
}

set_confidence_dir=function()
{
  setwd(path6)
}

## Setting Directory for Retrieving Year Variable from Production directory
set_production_dir()
year_variables=read.csv(paste0(path4,"year_variables.csv"),header = FALSE)

## Setting Directory for Argentina Production in Export Directory
set_covariate_dir()
argentina_prod=read.csv(paste0(path3,"Argentina_Prod.csv"),header = TRUE)

max_year=year_variables$V2[1]
min_year=argentina_prod$Ano[1]
max_month=year_variables$V2[3]
min_month=year_variables$V2[4]
pred_year=year_variables$V2[5]

data_points=(max_year-min_year)*12-(min_month-1)+max_month

max_len=((pred_year-max_year)*12)+(12-max_month)

total_data_points=data_points+max_len

## Setting the path for Input Files
set_input_dir()
## Reading the data from csv files
export_data <- read.csv("final_input_data.csv", header = T, stringsAsFactors = FALSE,sep = ',')

## Filtering State from File
data_BA<-subset(export_data, UF=='BA'&Year>=min_year)
data_GO<-subset(export_data, UF=='GO'&Year>=min_year)
data_MG<-subset(export_data, UF=='MG'&Year>=min_year)
data_MS<-subset(export_data, UF=='MS'&Year>=min_year)
data_MT<-subset(export_data, UF=='MT'&Year>=min_year)
data_PR<-subset(export_data, UF=='PR'&Year>=min_year)
data_RS<-subset(export_data, UF=='RS'&Year>=min_year)
data_SC<-subset(export_data, UF=='SC'&Year>=min_year)
data_SP<-subset(export_data, UF=='SP'&Year>=min_year)

##-------------------------------- Prediction - BA -------------------------------------------##

## Add a row at specific index
rownames(data_BA) <- seq(length=nrow(data_BA))

scale=function(x)
{
  z=(x-mean(x))/sd(x)
  return(z)
}

unscale=function(z,x)
{
  p=(sd(x)*z)+mean(x)
  return(p)
}

num_numeric = apply(data_BA[,c(5:13)], 2, as.numeric)
num_numeric = apply(num_numeric, 2, scale)
num_numeric<-as.data.frame(num_numeric)

## Removing all the irrelevant predictors
num_numeric$Exchange_Rate<-NULL
num_numeric$CBOT_Soya<-NULL

## Prediction for Future
train_pred<-num_numeric[c(1:data_points),]
test_pred<-num_numeric[c((data_points+1):total_data_points),]

test_pred = apply(test_pred, 2, as.numeric)

## Creating the time series
ts_pred=ts(train_pred[,c(1)],start=c(min_year,1),frequency=12)

## Manual Arima Fit
ARIMAfit <- arima(ts_pred,order=c(0,1,2),seasonal=list(order=c(1,1,2),period=12),xreg = train_pred[,c(2:7)])

pred_test <- forecast(ARIMAfit,h=max_len,xreg = test_pred[,c(2:7)],level = c(70,75,80,85,90,95))
df_test=data.frame(pred_test)

## Unscaling the data
unscaled_data<-unscale(df_test$Point.Forecast,data_BA[,c(5)])

## Getting the fitted values
train_fit<-fitted(ARIMAfit)
unscaled_train<-unscale(train_fit,data_BA[,c(5)])

## combination of training fitted and test values
final_predicted<-c(unscaled_train,unscaled_data)

## Creating time series for fitted data
pred=ts(final_predicted,start=c(min_year,1),frequency=12)

## Plotting the original and validated dataset
count_ts <- ts(data_BA[,c(5)],start=c(min_year,1),frequency=12)
set_plot_dir()
jpeg('Soya_Bran_BA.jpg')
plot(count_ts,ylab="Export ('000 tonnes)", main="Export Forecast - Bahia (BA)")
lines(pred,col="red")
dev.off()

dataframe=as.data.frame(final_predicted)
colnames(dataframe)<-c("Predicted Export")
Year<-data_BA$Year
Month<-data_BA$Month
Date<-paste(data_BA$Month, data_BA$Year, sep="/1/")
dataframe=dataframe %>% add_column(Date, .before = "Predicted Export")
dataframe$`Predicted Export`<-ifelse(dataframe$`Predicted Export`<0,0,dataframe$`Predicted Export`)

## Saving the Predicted Exports
set_predicted_dir()
write.csv(dataframe,"Predicted-Export_BA.csv",row.names = FALSE)

## unscaling the predictions and confidence intervals also
unscale<-unscale(df_test,data_BA[,c(5)])

confidence<-unscale
confidence <- cbind(Year = rownames(confidence), confidence)
Year<-as.character(confidence$Year)
confidence_values<-apply(confidence[2:14], 2, FUN = function(x){ifelse(x<0,0,x)})
confidence<-cbind(Year,confidence_values)
rownames(confidence) <- NULL

set_confidence_dir()
write.csv(confidence,"Confidence_BA.csv",row.names = FALSE)

##----------------------------------- Prediction - GO ---------------------------------------------##

scale=function(x)
{
  z=(x-mean(x))/sd(x)
  return(z)
}

unscale=function(z,x)
{
  p=(sd(x)*z)+mean(x)
  return(p)
}

rownames(data_GO) <- seq(length=nrow(data_GO)) 

num_numeric = apply(data_GO[,c(5:13)], 2, as.numeric)
num_numeric = apply(num_numeric,2,scale)

num_numeric<-as.data.frame(num_numeric)
## Removing all the irrelevant predictors
num_numeric$Exchange_Rate<-NULL
num_numeric$CBOT_Soyameal<-NULL

## Prediction for Future
train_pred<-num_numeric[c(1:data_points),]
test_pred<-num_numeric[c((data_points+1):total_data_points),]

test_pred = apply(test_pred, 2, as.numeric)

## Creating the time series
ts_pred=ts(train_pred[,c(1)],start=c(min_year,1),frequency=12)

## Manual Arima Fit
ARIMAfit <- arima(ts_pred,order=c(0,1,0),seasonal=list(order=c(1,1,1),period=12),xreg = train_pred[,c(2:7)])

pred_test <- forecast(ARIMAfit,h=max_len,xreg = test_pred[,c(2:7)],level = c(70,75,80,85,90,95))
df_test=data.frame(pred_test)

## Unscaling the data
unscaled_data<-unscale(df_test$Point.Forecast,data_GO[,c(5)])

## Getting the fitted values
train_fit<-fitted(ARIMAfit)
unscaled_train<-unscale(train_fit,data_GO[,c(5)])

## combination of training fitted and test values
final_predicted<-c(unscaled_train,unscaled_data)

## Creating time series for fitted data
pred=ts(final_predicted,start=c(min_year,1),frequency=12)

## Plotting the original and validated dataset
count_ts <- ts(data_GO[,c(5)],start=c(min_year,1),frequency=12)
set_plot_dir()
jpeg('Soya_Bran_GO.jpg')
plot(count_ts,ylab="Export ('000 tonnes)", main="Export Forecast - Goias (GO)")
lines(pred,col="red")
dev.off()

dataframe=as.data.frame(final_predicted)
colnames(dataframe)<-c("Predicted Export")
Year<-data_GO$Year
Month<-data_GO$Month
Date<-paste(data_GO$Month, data_GO$Year, sep="/1/")
dataframe=dataframe %>% add_column(Date, .before = "Predicted Export")
dataframe$`Predicted Export`<-ifelse(dataframe$`Predicted Export`<0,0,dataframe$`Predicted Export`)

## Saving the Predicted Exports
set_predicted_dir()
write.csv(dataframe,"Predicted-Export_GO.csv",row.names = FALSE)

## unscaling the predictions and confidence intervals also
unscale<-unscale(df_test,data_GO[,c(5)])

confidence<-unscale
confidence <- cbind(Year = rownames(confidence), confidence)
Year<-as.character(confidence$Year)
confidence_values<-apply(confidence[2:14], 2, FUN = function(x){ifelse(x<0,0,x)})
confidence<-cbind(Year,confidence_values)
rownames(confidence) <- NULL

set_confidence_dir()
write.csv(confidence,"Confidence_GO.csv",row.names = FALSE)

##--------------------------------- Prediction - MG -----------------------------------------------##

scale=function(x)
{
  z=(x-mean(x))/sd(x)
  return(z)
}

unscale=function(z,x)
{
  p=(sd(x)*z)+mean(x)
  return(p)
}

rownames(data_MG) <- seq(length=nrow(data_MG)) 

num_numeric = apply(data_MG[,c(5:13)], 2,as.numeric)
num_numeric = apply(num_numeric, 2,scale)

num_numeric<-as.data.frame(num_numeric)
## Removing all the irrelevant predictors
num_numeric$Exchange_Rate<-NULL
num_numeric$CBOT_Soyameal<-NULL

## Prediction for Future
train_pred<-num_numeric[c(1:data_points),]
test_pred<-num_numeric[c((data_points+1):total_data_points),]

test_pred = apply(test_pred, 2, as.numeric)

## Creating the time series
ts_pred=ts(train_pred[,c(1)],start=c(min_year,1),frequency=12)

## Manual Arima Fit
ARIMAfit <- arima(ts_pred,order=c(0,1,2),seasonal=list(order=c(1,1,2),period=12),xreg = train_pred[,c(2:7)])

pred_test <- forecast(ARIMAfit,h=max_len,xreg = test_pred[,c(2:7)],level = c(70,75,80,85,90,95))
df_test=data.frame(pred_test)

## Unscaling the data
unscaled_data<-unscale(df_test$Point.Forecast,data_MG[,c(5)])

## Getting the fitted values
train_fit<-fitted(ARIMAfit)
unscaled_train<-unscale(train_fit,data_MG[,c(5)])

## combination of training fitted and test values
final_predicted<-c(unscaled_train,unscaled_data)

## Creating time series for fitted data
pred=ts(final_predicted,start=c(min_year,1),frequency=12)

## Plotting the original and validated dataset
count_ts <- ts(data_MG[,c(5)],start=c(min_year,1),frequency=12)
set_plot_dir()
jpeg('Soya_Bran_MG.jpg')
plot(count_ts,ylab="Export ('000 tonnes)", main="Export Forecast - Minas Gerais (MG)")
lines(pred,col="red")
dev.off()

dataframe=as.data.frame(final_predicted)
colnames(dataframe)<-c("Predicted Export")
Year<-data_MG$Year
Month<-data_MG$Month
Date<-paste(data_MG$Month, data_MG$Year, sep="/1/")
dataframe=dataframe %>% add_column(Date, .before = "Predicted Export")
dataframe$`Predicted Export`<-ifelse(dataframe$`Predicted Export`<0,0,dataframe$`Predicted Export`)

## Saving the Predicted Exports
set_predicted_dir()
write.csv(dataframe,"Predicted-Export_MG.csv",row.names = FALSE)

## unscaling the predictions and confidence intervals also
unscale<-unscale(df_test,data_MG[,c(5)])

confidence<-unscale
confidence <- cbind(Year = rownames(confidence), confidence)
Year<-as.character(confidence$Year)
confidence_values<-apply(confidence[2:14], 2, FUN = function(x){ifelse(x<0,0,x)})
confidence<-cbind(Year,confidence_values)
rownames(confidence) <- NULL

set_confidence_dir()
write.csv(confidence,"Confidence_MG.csv",row.names = FALSE)

##--------------------------------- Prediction - MS ------------------------------------------------##

scale=function(x)
{
  z=(x-mean(x))/sd(x)
  return(z)
}

unscale=function(z,x)
{
  p=(sd(x)*z)+mean(x)
  return(p)
}

rownames(data_MS) <- seq(length=nrow(data_MS)) 
data_MS[is.na(data_MS)] <- 0

num_numeric = apply(data_MS[,c(5:13)], 2, as.numeric)
num_numeric = apply(num_numeric,2,scale)

num_numeric<-as.data.frame(num_numeric)
## Removing all the irrelevant predictors
num_numeric$Exchange_Rate<-NULL
num_numeric$CBOT_Soya<-NULL

## Prediction for Future
train_pred<-num_numeric[c(1:data_points),]
test_pred<-num_numeric[c((data_points+1):total_data_points),]

test_pred = apply(test_pred, 2, as.numeric)

## Creating the time series
ts_pred=ts(train_pred[,c(1)],start=c(min_year,1),frequency=12)

## Manual Arima Fit
ARIMAfit <- arima(ts_pred,order=c(0,1,1),seasonal=list(order=c(1,1,2),period=12),xreg = train_pred[,c(2:7)])

pred_test <- forecast(ARIMAfit,h=max_len,xreg = test_pred[,c(2:7)],level = c(70,75,80,85,90,95))
df_test=data.frame(pred_test)

## Unscaling the data
unscaled_data<-unscale(df_test$Point.Forecast,data_MS[,c(5)])

## Getting the fitted values
train_fit<-fitted(ARIMAfit)
unscaled_train<-unscale(train_fit,data_MS[,c(5)])

## combination of training fitted and test values
final_predicted<-c(unscaled_train,unscaled_data)

## Creating time series for fitted data
pred=ts(final_predicted,start=c(min_year,1),frequency=12)

## Plotting the original and validated dataset
count_ts <- ts(data_MS[,c(5)],start=c(min_year,1),frequency=12)
set_plot_dir()
jpeg('Soya_Bran_MS.jpg')
plot(count_ts,ylab="Export ('000 tonnes)", main="Export Forecast - Matogrosso Do Sul (MS)")
lines(pred,col="red")
dev.off()

dataframe=as.data.frame(final_predicted)
colnames(dataframe)<-c("Predicted Export")
Year<-data_MS$Year
Month<-data_MS$Month
Date<-paste(data_MS$Month, data_MS$Year, sep="/1/")
dataframe=dataframe %>% add_column(Date, .before = "Predicted Export")
dataframe$`Predicted Export`<-ifelse(dataframe$`Predicted Export`<0,0,dataframe$`Predicted Export`)

## Saving the Predicted Exports
set_predicted_dir()
write.csv(dataframe,"Predicted-Export_MS.csv",row.names = FALSE)

## unscaling the predictions and confidence intervals also
unscale<-unscale(df_test,data_MS[,c(5)])

confidence<-unscale
confidence <- cbind(Year = rownames(confidence), confidence)
Year<-as.character(confidence$Year)
confidence_values<-apply(confidence[2:14], 2, FUN = function(x){ifelse(x<0,0,x)})
confidence<-cbind(Year,confidence_values)
rownames(confidence) <- NULL

set_confidence_dir()
write.csv(confidence,"Confidence_MS.csv",row.names = FALSE)

##-------------------------------- Prediction - MT ----------------------------------------------------##

scale=function(x)
{
  z=(x-mean(x))/sd(x)
  return(z)
}

unscale=function(z,x)
{
  p=(sd(x)*z)+mean(x)
  return(p)
}

rownames(data_MT) <- seq(length=nrow(data_MT)) 

num_numeric = apply(data_MT[,c(5:13)], 2, as.numeric)
num_numeric = apply(num_numeric, 2, scale)

num_numeric<-as.data.frame(num_numeric)
## Removing all the irrelevant predictors
num_numeric$Exchange_Rate<-NULL
num_numeric$CBOT_Soya<-NULL

## Prediction for Future
train_pred<-num_numeric[c(1:data_points),]
test_pred<-num_numeric[c((data_points+1):total_data_points),]

test_pred = apply(test_pred, 2, as.numeric)

## Creating the time series
ts_pred=ts(train_pred[,c(1)],start=c(min_year,1),frequency=12)

## Manual Arima Fit
ARIMAfit <- arima(ts_pred,order=c(0,1,2),seasonal=list(order=c(0,1,2),period=12),xreg = train_pred[,c(2:7)])

pred_test <- forecast(ARIMAfit,h=max_len,xreg = test_pred[,c(2:7)],level = c(70,75,80,85,90,95))
df_test=data.frame(pred_test)

## Unscaling the data
unscaled_data<-unscale(df_test$Point.Forecast,data_MT[,c(5)])

## Getting the fitted values
train_fit<-fitted(ARIMAfit)
unscaled_train<-unscale(train_fit,data_MT[,c(5)])

## combination of training fitted and test values
final_predicted<-c(unscaled_train,unscaled_data)

## Creating time series for fitted data
pred=ts(final_predicted,start=c(min_year,1),frequency=12)

## Plotting the original and validated dataset
count_ts <- ts(data_MT[,c(5)],start=c(min_year,1),frequency=12)
set_plot_dir()
jpeg('Soya_Bran_MT.jpg')
plot(count_ts,ylab="Export ('000 tonnes)", main="Export Forecast - Mato Grosso (MT)")
lines(pred,col="red")
dev.off()

dataframe=as.data.frame(final_predicted)
colnames(dataframe)<-c("Predicted Export")
Year<-data_MT$Year
Month<-data_MT$Month
Date<-paste(data_MT$Month, data_MT$Year, sep="/1/")
dataframe=dataframe %>% add_column(Date, .before = "Predicted Export")
dataframe$`Predicted Export`<-ifelse(dataframe$`Predicted Export`<0,0,dataframe$`Predicted Export`)

## Saving the Predicted Exports
set_predicted_dir()
write.csv(dataframe,"Predicted-Export_MT.csv",row.names = FALSE)

## unscaling the predictions and confidence intervals also
unscale<-unscale(df_test,data_MT[,c(5)])

confidence<-unscale
confidence <- cbind(Year = rownames(confidence), confidence)
Year<-as.character(confidence$Year)
confidence_values<-apply(confidence[2:14], 2, FUN = function(x){ifelse(x<0,0,x)})
confidence<-cbind(Year,confidence_values)
rownames(confidence) <- NULL

set_confidence_dir()
write.csv(confidence,"Confidence_MT.csv",row.names = FALSE)

##---------------------------------- Prediction - PR ------------------------------------------------##

scale=function(x)
{
  z=(x-mean(x))/sd(x)
  return(z)
}

unscale=function(z,x)
{
  p=(sd(x)*z)+mean(x)
  return(p)
}

rownames(data_PR) <- seq(length=nrow(data_PR)) 

num_numeric = apply(data_PR[,c(5:13)], 2, as.numeric)
num_numeric = apply(num_numeric, 2, scale)

num_numeric<-as.data.frame(num_numeric)
## Removing all the irrelevant predictors
num_numeric$Exchange_Rate<-NULL
num_numeric$CBOT_Soyameal<-NULL

## Prediction for Future
train_pred<-num_numeric[c(1:data_points),]
test_pred<-num_numeric[c((data_points+1):total_data_points),]

test_pred = apply(test_pred, 2, as.numeric)

## Creating the time series
ts_pred=ts(train_pred[,c(1)],start=c(min_year,1),frequency=12)

## Manual Arima Fit
ARIMAfit <- arima(ts_pred,order=c(0,1,2),seasonal=list(order=c(0,1,2),period=12),xreg = train_pred[,c(2:7)])

pred_test <- forecast(ARIMAfit,h=max_len,xreg = test_pred[,c(2:7)],level = c(70,75,80,85,90,95))
df_test=data.frame(pred_test)

## Unscaling the data
unscaled_data<-unscale(df_test$Point.Forecast,data_PR[,c(5)])

## Getting the fitted values
train_fit<-fitted(ARIMAfit)
unscaled_train<-unscale(train_fit,data_PR[,c(5)])

## combination of training fitted and test values
final_predicted<-c(unscaled_train,unscaled_data)

## Creating time series for fitted data
pred=ts(final_predicted,start=c(min_year,1),frequency=12)

## Plotting the original and validated dataset
count_ts <- ts(data_PR[,c(5)],start=c(min_year,1),frequency=12)
set_plot_dir()
jpeg('Soya_Bran_PR.jpg')
plot(count_ts,ylab="Export ('000 tonnes)", main="Export Forecast - Parana (PR)")
lines(pred,col="red")
dev.off()

dataframe=as.data.frame(final_predicted)
colnames(dataframe)<-c("Predicted Export")
Year<-data_PR$Year
Month<-data_PR$Month
Date<-paste(data_PR$Month, data_PR$Year, sep="/1/")
dataframe=dataframe %>% add_column(Date, .before = "Predicted Export")
dataframe$`Predicted Export`<-ifelse(dataframe$`Predicted Export`<0,0,dataframe$`Predicted Export`)

## Saving the Predicted Exports
set_predicted_dir()
write.csv(dataframe,"Predicted-Export_PR.csv",row.names = FALSE)

## unscaling the predictions and confidence intervals also
unscale<-unscale(df_test,data_PR[,c(5)])

confidence<-unscale
confidence <- cbind(Year = rownames(confidence), confidence)
Year<-as.character(confidence$Year)
confidence_values<-apply(confidence[2:14], 2, FUN = function(x){ifelse(x<0,0,x)})
confidence<-cbind(Year,confidence_values)
rownames(confidence) <- NULL

set_confidence_dir()
write.csv(confidence,"Confidence_PR.csv",row.names = FALSE)

##------------------------------ Prediction - RS ----------------------------------------------##

scale=function(x)
{
  z=(x-mean(x))/sd(x)
  return(z)
}

unscale=function(z,x)
{
  p=(sd(x)*z)+mean(x)
  return(p)
}

rownames(data_RS) <- seq(length=nrow(data_RS)) 

num_numeric = apply(data_RS[,c(5:13)], 2, as.numeric)
num_numeric = apply(num_numeric,2,scale)

num_numeric<-as.data.frame(num_numeric)
## Removing all the irrelevant predictors
num_numeric$Exchange_Rate<-NULL

## Prediction for Future
train_pred<-num_numeric[c(1:data_points),]
test_pred<-num_numeric[c((data_points+1):total_data_points),]

test_pred = apply(test_pred, 2, as.numeric)

## Creating the time series
ts_pred=ts(train_pred[,c(1)],start=c(min_year,1),frequency=12)

## Manual Arima Fit
ARIMAfit <- arima(ts_pred,order=c(0,1,2),seasonal=list(order=c(0,1,2),period=12),xreg = train_pred[,c(2:8)])

pred_test <- forecast(ARIMAfit,h=max_len,xreg = test_pred[,c(2:8)],level = c(70,75,80,85,90,95))
df_test=data.frame(pred_test)

## Unscaling the data
unscaled_data<-unscale(df_test$Point.Forecast,data_RS[,c(5)])

## Getting the fitted values
train_fit<-fitted(ARIMAfit)
unscaled_train<-unscale(train_fit,data_RS[,c(5)])

## combination of training fitted and test values
final_predicted<-c(unscaled_train,unscaled_data)

## Creating time series for fitted data
pred=ts(final_predicted,start=c(min_year,1),frequency=12)

## Plotting the original and validated dataset
count_ts <- ts(data_RS[,c(5)],start=c(min_year,1),frequency=12)
set_plot_dir()
jpeg('Soya_Bran_RS.jpg')
plot(count_ts,ylab="Export ('000 tonnes)", main="Export Forecast - Rio Grande Do Sul (RS)")
lines(pred,col="red")
dev.off()

dataframe=as.data.frame(final_predicted)
colnames(dataframe)<-c("Predicted Export")
Year<-data_RS$Year
Month<-data_RS$Month
Date<-paste(data_RS$Month, data_RS$Year, sep="/1/")
dataframe=dataframe %>% add_column(Date, .before = "Predicted Export")
dataframe$`Predicted Export`<-ifelse(dataframe$`Predicted Export`<0,0,dataframe$`Predicted Export`)

## Saving the Predicted Exports
set_predicted_dir()
write.csv(dataframe,"Predicted-Export_RS.csv",row.names = FALSE)

## unscaling the predictions and confidence intervals also
unscale<-unscale(df_test,data_RS[,c(5)])

confidence<-unscale
confidence <- cbind(Year = rownames(confidence), confidence)
Year<-as.character(confidence$Year)
confidence_values<-apply(confidence[2:14], 2, FUN = function(x){ifelse(x<0,0,x)})
confidence<-cbind(Year,confidence_values)
rownames(confidence) <- NULL

set_confidence_dir()
write.csv(confidence,"Confidence_RS.csv",row.names = FALSE)

##--------------------------------- Prediction - SC ---------------------------------------------##

scale=function(x)
{
  z=(x-mean(x))/sd(x)
  return(z)
}

unscale=function(z,x)
{
  p=(sd(x)*z)+mean(x)
  return(p)
}

rownames(data_SC) <- seq(length=nrow(data_SC)) 

num_numeric = apply(data_SC[,c(5:13)], 2, as.numeric)
num_numeric = apply(num_numeric,2,scale)

num_numeric<-as.data.frame(num_numeric)
## Removing all the irrelevant predictors
num_numeric$Exchange_Rate<-NULL
num_numeric$CBOT_Soyameal<-NULL

## Prediction for Future
train_pred<-num_numeric[c(1:data_points),]
test_pred<-num_numeric[c((data_points+1):total_data_points),]

test_pred = apply(test_pred, 2, as.numeric)

## Creating the time series
ts_pred=ts(train_pred[,c(1)],start=c(min_year,1),frequency=12)

## Manual Arima Fit
ARIMAfit <- arima(ts_pred,order=c(0,1,0),seasonal=list(order=c(0,1,1),period=12),xreg = train_pred[,c(2:7)])

pred_test <- forecast(ARIMAfit,h=max_len,xreg = test_pred[,c(2:7)],level = c(70,75,80,85,90,95))
df_test=data.frame(pred_test)

## Unscaling the data
unscaled_data<-unscale(df_test$Point.Forecast,data_SC[,c(5)])

## Getting the fitted values
train_fit<-fitted(ARIMAfit)
unscaled_train<-unscale(train_fit,data_SC[,c(5)])

## combination of training fitted and test values
final_predicted<-c(unscaled_train,unscaled_data)

## Creating time series for fitted data
pred=ts(final_predicted,start=c(min_year,1),frequency=12)

## Plotting the original and validated dataset
count_ts <- ts(data_SC[,c(5)],start=c(min_year,1),frequency=12)
set_plot_dir()
jpeg('Soya_Bran_SC.jpg')
plot(count_ts,ylab="Export ('000 tonnes)", main="Export Forecast - Santa Catarina (SC)")
lines(pred,col="red")
dev.off()

dataframe=as.data.frame(final_predicted)
colnames(dataframe)<-c("Predicted Export")
Year<-data_SC$Year
Month<-data_SC$Month
Date<-paste(data_SC$Month, data_SC$Year, sep="/1/")
dataframe=dataframe %>% add_column(Date, .before = "Predicted Export")
dataframe$`Predicted Export`<-ifelse(dataframe$`Predicted Export`<0,0,dataframe$`Predicted Export`)

## Saving the Predicted Exports
set_predicted_dir()
write.csv(dataframe,"Predicted-Export_SC.csv",row.names = FALSE)

## unscaling the predictions and confidence intervals also
unscale<-unscale(df_test,data_SC[,c(5)])

confidence<-unscale
confidence <- cbind(Year = rownames(confidence), confidence)
Year<-as.character(confidence$Year)
confidence_values<-apply(confidence[2:14], 2, FUN = function(x){ifelse(x<0,0,x)})
confidence<-cbind(Year,confidence_values)
rownames(confidence) <- NULL

set_confidence_dir()
write.csv(confidence,"Confidence_SC.csv",row.names = FALSE)

##------------------------------------ Prediction - SP ------------------------------------------------##

scale=function(x)
{
  z=(x-mean(x))/sd(x)
  return(z)
}

unscale=function(z,x)
{
  p=(sd(x)*z)+mean(x)
  return(p)
}

rownames(data_SP) <- seq(length=nrow(data_SP)) 

num_numeric = apply(data_SP[,c(5:13)], 2, as.numeric)
num_numeric = apply(num_numeric,2,scale)

## Prediction for Future
train_pred<-num_numeric[c(1:data_points),]
test_pred<-num_numeric[c((data_points+1):total_data_points),]

test_pred = apply(test_pred, 2, as.numeric)

## Creating the time series
ts_pred=ts(train_pred[,c(1)],start=c(min_year,1),frequency=12)

## Manual Arima Fit
ARIMAfit <- arima(ts_pred,order=c(0,1,1),seasonal=list(order=c(0,1,2),period=12),xreg = train_pred[,c(2:7)])

pred_test <- forecast(ARIMAfit,h=max_len,xreg = test_pred[,c(2:7)],level = c(70,75,80,85,90,95))
df_test=data.frame(pred_test)

## Unscaling the data
unscaled_data<-unscale(df_test$Point.Forecast,data_SP[,c(5)])

## Getting the fitted values
train_fit<-fitted(ARIMAfit)
unscaled_train<-unscale(train_fit,data_SP[,c(5)])

## combination of training fitted and test values
final_predicted<-c(unscaled_train,unscaled_data)

## Creating time series for fitted data
pred=ts(final_predicted,start=c(min_year,1),frequency=12)

## Plotting the original and validated dataset
count_ts <- ts(data_SP[,c(5)],start=c(min_year,1),frequency=12)
set_plot_dir()
jpeg('Soya_Bran_SP.jpg')
plot(count_ts,ylab="Export ('000 tonnes)", main="Export Forecast - Sao Paulo (SP)")
lines(pred,col="red")
dev.off()

dataframe=as.data.frame(final_predicted)
colnames(dataframe)<-c("Predicted Export")
Year<-data_SP$Year
Month<-data_SP$Month
Date<-paste(data_SP$Month, data_SP$Year, sep="/1/")
dataframe=dataframe %>% add_column(Date, .before = "Predicted Export")
dataframe$`Predicted Export`<-ifelse(dataframe$`Predicted Export`<0,0,dataframe$`Predicted Export`)

## Saving the Predicted Exports
set_predicted_dir()
write.csv(dataframe,"Predicted-Export_SP.csv",row.names = FALSE)

## unscaling the predictions and confidence intervals also
unscale<-unscale(df_test,data_SP[,c(5)])

confidence<-unscale
confidence <- cbind(Year = rownames(confidence), confidence)
Year<-as.character(confidence$Year)
confidence_values<-apply(confidence[2:14], 2, FUN = function(x){ifelse(x<0,0,x)})
confidence<-cbind(Year,confidence_values)
rownames(confidence) <- NULL

set_confidence_dir()
write.csv(confidence,"Confidence_SP.csv",row.names = FALSE)
