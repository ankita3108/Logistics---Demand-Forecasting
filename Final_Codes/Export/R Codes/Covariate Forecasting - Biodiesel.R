## To clear the screen
rm(list=ls())

library(forecast)
library(imputeTS)

## Setting the paths
setwd("..")
setwd("..")
setwd("..")
setwd("..")
path<-paste0(getwd())
path1<-paste0(path,"/Export/Data Directory/")
path2<-paste0(path,"/Export/Data Directory/Soya_Bran/Plots - Covariates/")
path3<-paste0(path,"/Export/Data Directory/Soya_Bran/Prediction - Covariates/")
path4<-paste0(path,"/Production/Data Directory/")

set_input_dir=function()
{
  setwd(path1)
}

set_plot_dir=function()
{
  setwd(path2)
}

set_output_dir=function()
{
  setwd(path3)
}

set_production_dir=function()
{
  setwd(path4)
}

## Setting Directory for Retrieving Year Variable from Production directory
set_production_dir()
year_variables=read.csv(paste0(path4,"year_variables.csv"),header = FALSE)

max_year=year_variables$V2[1]
min_year=year_variables$V2[2]
max_month=year_variables$V2[3]
min_month=year_variables$V2[4]
pred_year=year_variables$V2[5]

data_points=(max_year-min_year)*12-(min_month-1)+max_month

max_len=((pred_year-max_year)*12)+(12-max_month)

total_data_points=data_points+max_len

set_input_dir()
data3=read.csv("../Input Files/Producao_Biodiesel.csv",header = T, stringsAsFactors = FALSE)

## Filtering State BA from File
data_BA<-subset(data3, Estado_Production=='BA')
rownames(data_BA) <- seq(length=nrow(data_BA))
BA_ts <- ts(data_BA[,c(4)],start=c(min_year,1),frequency=12)
ARIMAfit <- arima(BA_ts,order=c(1,1,1),seasonal=list(order=c(1,2,1),period=12))
forecast_points=total_data_points-nrow(data_BA)
pred <- forecast(ARIMAfit,h=forecast_points)
set_plot_dir()
jpeg('Biodiesel_Prod (BA).jpg')
plot(pred, ylab="Biodiesel Prod (m3)", main="Forecasting (BA) - Biodiesel Production (m3)")
dev.off()
set_output_dir()
write.csv(pred,"Prediction - Biodiesel_Production (BA).csv")

##-------------------------------------------------------------------------------------------##

set_input_dir()
data4=read.csv("../Input Files/Consumo_Biodiesel.csv",header = T, stringsAsFactors = FALSE)

## Filtering State BA from File
data_BA<-subset(data4, Estado_Consumption=='BA')
rownames(data_BA) <- seq(length=nrow(data_BA))
BA_ts <- ts(data_BA[,c(4)],start=c(min_year,1),frequency=12)
ARIMAfit <- arima(BA_ts,order=c(1,0,1),seasonal=list(order=c(1,2,1),period=12))
pred <- forecast(ARIMAfit,h=forecast_points)
set_plot_dir()
jpeg('Biodiesel_Consump (BA).jpg')
plot(pred, ylab="Biodiesel Consump (m3)", main="Forecasting (BA) - Biodiesel Consumption (m3)")
dev.off()
set_output_dir()
write.csv(pred,"Prediction - Biodiesel_Consumption (BA).csv")

##-----------------------------------------------------------------------------------------------------##

## Filtering State GO from File
data_GO<-subset(data3, Estado_Production=='GO')
rownames(data_GO) <- seq(length=nrow(data_GO))
GO_ts <- ts(data_GO[,c(4)],start=c(min_year,1),frequency=12)
ARIMAfit <- arima(GO_ts,order=c(1,1,1),seasonal=list(order=c(1,2,1),period=12))
pred <- forecast(ARIMAfit,h=forecast_points)
set_plot_dir()
jpeg('Biodiesel_Prod (GO).jpg')
plot(pred,ylab="Biodiesel Prod (m3)", main="Forecasting (GO) - Biodiesel Production (m3)")
dev.off()
set_output_dir()
write.csv(pred,"Prediction - Biodiesel_Production (GO).csv")

##-------------------------------------------------------------------------------------------##

## Filtering State GO from File
data_GO<-subset(data4, Estado_Consumption=='GO')
rownames(data_GO) <- seq(length=nrow(data_GO))
GO_ts <- ts(data_GO[,c(4)],start=c(min_year,1),frequency=12)
ARIMAfit <- arima(GO_ts,order=c(1,0,1),seasonal=list(order=c(1,2,1),period=12))
pred <- forecast(ARIMAfit,h=forecast_points)
set_plot_dir()
jpeg('Biodiesel_Consump (GO).jpg')
plot(pred,ylab="Biodiesel Consump (m3)", main="Forecasting (GO) - Biodiesel Consumption (m3)")
dev.off()
set_output_dir()
write.csv(pred,"Prediction - Biodiesel_Consumption (GO).csv")

##-------------------------------------------------------------------------------------------##

## Filtering State MG from File
data_MG<-subset(data3, Estado_Production=='MG')
rownames(data_MG) <- seq(length=nrow(data_MG))
MG_ts <- ts(data_MG[,c(4)],start=c(min_year,1),frequency=12)
ARIMAfit <- arima(MG_ts,order=c(1,1,1),seasonal=list(order=c(1,2,1),period=12))
pred <- forecast(ARIMAfit,h=forecast_points)
set_plot_dir()
jpeg('Biodiesel_Prod (MG).jpg')
plot(pred,ylab="Biodiesel Prod (m3)", main="Forecasting (MG) - Biodiesel Production (m3)")
dev.off()
set_output_dir()
write.csv(pred,"Prediction - Biodiesel_Production (MG).csv")

##-------------------------------------------------------------------------------------------##

## Filtering State MG from File
data_MG<-subset(data4, Estado_Consumption=='MG')
rownames(data_MG) <- seq(length=nrow(data_MG))
MG_ts <- ts(data_MG[,c(4)],start=c(min_year,1),frequency=12)
ARIMAfit <- arima(MG_ts,order=c(1,0,1),seasonal=list(order=c(1,2,1),period=12))
pred <- forecast(ARIMAfit,h=forecast_points)
set_plot_dir()
jpeg('Biodiesel_Consump (MG).jpg')
plot(pred,ylab="Biodiesel Consump (m3)", main="Forecasting (MG) - Biodiesel Consumption (m3)")
dev.off()
set_output_dir()
write.csv(pred,"Prediction - Biodiesel_Consumption (MG).csv")

##-------------------------------------------------------------------------------------------##

## Filtering State MS from File
data_MS<-subset(data3, Estado_Production=='MS')
rownames(data_MS) <- seq(length=nrow(data_MS))
MS_ts <- ts(data_MS[,c(4)],start=c(min_year,1),frequency=12)
ARIMAfit <- arima(MS_ts,order=c(1,1,1),seasonal=list(order=c(1,1,1),period=12))
pred <- forecast(ARIMAfit,h=forecast_points)
set_plot_dir()
jpeg('Biodiesel_Prod (MS).jpg')
plot(pred,ylab="Biodiesel Prod (m3)", main="Forecasting (MS) - Biodiesel Production (m3)")
dev.off()
set_output_dir()
write.csv(pred,"Prediction - Biodiesel_Production (MS).csv")

##-------------------------------------------------------------------------------------------##

## Filtering State MS from File
data_MS<-subset(data4, Estado_Consumption=='MS')
rownames(data_MS) <- seq(length=nrow(data_MS))
MS_ts <- ts(data_MS[,c(4)],start=c(min_year,1),frequency=12)
ARIMAfit <- arima(MS_ts,order=c(1,0,1),seasonal=list(order=c(1,2,1),period=12))
pred <- forecast(ARIMAfit,h=forecast_points)
set_plot_dir()
jpeg('Biodiesel_Consump (MS).jpg')
plot(pred,ylab="Biodiesel Consump (m3)", main="Forecasting (MS) - Biodiesel Consumption (m3)")
dev.off()
DF=as.data.frame(pred)

mean_less<-sapply(DF$`Point Forecast`,function(x){if(x<0){x<-0}else{x<-x}})

DF$`Point Forecast`<-mean_less

mean_less<-sapply(DF$`Lo 80`,function(x){if(x<0){x<-0}else{x<-x}})

DF$`Lo 80`<-mean_less

mean_less<-sapply(DF$`Hi 80`,function(x){if(x<0){x<-0}else{x<-x}})

DF$`Hi 80`<-mean_less

mean_less<-sapply(DF$`Lo 95`,function(x){if(x<0){x<-0}else{x<-x}})

DF$`Lo 95`<-mean_less

mean_less<-sapply(DF$`Hi 95`,function(x){if(x<0){x<-0}else{x<-x}})

DF$`Hi 95`<-mean_less

pred2<-DF

set_output_dir()
write.csv(DF,"Prediction - Biodiesel_Consumption (MS).csv")

##-------------------------------------------------------------------------------------------##

## Filtering State MT from File
data_MT<-subset(data3, Estado_Production=='MT')
rownames(data_MT) <- seq(length=nrow(data_MT))
MT_ts <- ts(data_MT[,c(4)],start=c(min_year,1),frequency=12)
ARIMAfit <- arima(MT_ts,order=c(1,1,1),seasonal=list(order=c(1,2,1),period=12))
pred <- forecast(ARIMAfit,h=forecast_points)
set_plot_dir()
jpeg('Biodiesel_Prod (MT).jpg')
plot(pred,ylab="Biodiesel Prod (m3)", main="Forecasting (MT) - Biodiesel Production (m3)")
dev.off()

DF=as.data.frame(pred)

mean_less<-sapply(DF$`Point Forecast`,function(x){if(x<0){x<-0}else{x<-x}})

DF$`Point Forecast`<-mean_less

mean_less<-sapply(DF$`Lo 80`,function(x){if(x<0){x<-0}else{x<-x}})

DF$`Lo 80`<-mean_less

mean_less<-sapply(DF$`Hi 80`,function(x){if(x<0){x<-0}else{x<-x}})

DF$`Hi 80`<-mean_less

mean_less<-sapply(DF$`Lo 95`,function(x){if(x<0){x<-0}else{x<-x}})

DF$`Lo 95`<-mean_less

mean_less<-sapply(DF$`Hi 95`,function(x){if(x<0){x<-0}else{x<-x}})

DF$`Hi 95`<-mean_less

pred2<-DF
set_output_dir()
write.csv(DF,"Prediction - Biodiesel_Production (MT).csv")

##-------------------------------------------------------------------------------------------##

## Filtering State MT from File
data_MT<-subset(data4, Estado_Consumption=='MT')
rownames(data_MT) <- seq(length=nrow(data_MT))
MT_ts <- ts(data_MT[,c(4)],start=c(min_year,1),frequency=12)
ARIMAfit <- arima(MT_ts,order=c(1,0,1),seasonal=list(order=c(1,2,1),period=12))
pred <- forecast(ARIMAfit,h=forecast_points)
set_plot_dir()
jpeg('Biodiesel_Consump (MT).jpg')
plot(pred,ylab="Biodiesel Consump (m3)", main="Forecasting (MT) - Biodiesel Consumption (m3)")
dev.off()
DF=as.data.frame(pred)

mean_less<-sapply(DF$`Point Forecast`,function(x){if(x<0){x<-0}else{x<-x}})

DF$`Point Forecast`<-mean_less

mean_less<-sapply(DF$`Lo 80`,function(x){if(x<0){x<-0}else{x<-x}})

DF$`Lo 80`<-mean_less

mean_less<-sapply(DF$`Hi 80`,function(x){if(x<0){x<-0}else{x<-x}})

DF$`Hi 80`<-mean_less

mean_less<-sapply(DF$`Lo 95`,function(x){if(x<0){x<-0}else{x<-x}})

DF$`Lo 95`<-mean_less

mean_less<-sapply(DF$`Hi 95`,function(x){if(x<0){x<-0}else{x<-x}})

DF$`Hi 95`<-mean_less

pred2<-DF
set_output_dir()
write.csv(DF,"Prediction - Biodiesel_Consumption (MT).csv")

##-------------------------------------------------------------------------------------------##

## Filtering State PR from File
data_PR<-subset(data3, Estado_Production=='PR')
rownames(data_PR) <- seq(length=nrow(data_PR))
PR_ts <- ts(data_PR[,c(4)],start=c(min_year,1),frequency=12)
ARIMAfit <- arima(PR_ts,order=c(1,1,2),seasonal=list(order=c(1,1,1),period=12))
pred <- forecast(ARIMAfit,h=forecast_points)
set_plot_dir()
jpeg('Biodiesel_Prod (PR).jpg')
plot(pred,ylab="Biodiesel Prod (m3)", main="Forecasting (PR) - Biodiesel Production (m3)")
dev.off()
set_output_dir()
write.csv(pred,"Prediction - Biodiesel_Production (PR).csv")

##-------------------------------------------------------------------------------------------##

## Filtering State PR from File
data_PR<-subset(data4, Estado_Consumption=='PR')
rownames(data_PR) <- seq(length=nrow(data_PR))
PR_ts <- ts(data_PR[,c(4)],start=c(min_year,1),frequency=12)
ARIMAfit <- arima(PR_ts,order=c(1,1,1),seasonal=list(order=c(1,1,2),period=12))
pred <- forecast(ARIMAfit,h=forecast_points)
set_plot_dir()
jpeg('Biodiesel_Consump (PR).jpg')
plot(pred,ylab="Biodiesel Consump (m3)", main="Forecasting (PR) - Biodiesel Consumption (m3)")
dev.off()
set_output_dir()
write.csv(pred,"Prediction - Biodiesel_Consumption (PR).csv")

##-------------------------------------------------------------------------------------------##

## Filtering State RS from File
data_RS<-subset(data3, Estado_Production=='RS')
rownames(data_RS) <- seq(length=nrow(data_RS))
RS_ts <- ts(data_RS[,c(4)],start=c(min_year,1),frequency=12)
ARIMAfit <- arima(RS_ts,order=c(1,1,1),seasonal=list(order=c(1,2,1),period=12))
pred <- forecast(ARIMAfit,h=forecast_points)
set_plot_dir()
jpeg('Biodiesel_Prod (RS).jpg')
plot(pred,ylab="Biodiesel Prod (m3)", main="Forecasting (RS) - Biodiesel Production (m3)")
dev.off()
set_output_dir()
write.csv(pred,"Prediction - Biodiesel_Production (RS).csv")

##-------------------------------------------------------------------------------------------##

## Filtering State RS from File
data_RS<-subset(data4, Estado_Consumption=='RS')
rownames(data_RS) <- seq(length=nrow(data_RS))
RS_ts <- ts(data_RS[,c(4)],start=c(min_year,1),frequency=12)
ARIMAfit <- arima(RS_ts,order=c(1,1,1),seasonal=list(order=c(1,2,1),period=12))
pred <- forecast(ARIMAfit,h=forecast_points)
set_plot_dir()
jpeg('Biodiesel_Consump (RS).jpg')
plot(pred,ylab="Biodiesel Consump (m3)", main="Forecasting (RS) - Biodiesel Consumption (m3)")
dev.off()
set_output_dir()
write.csv(pred,"Prediction - Biodiesel_Consumption (RS).csv")

##-------------------------------------------------------------------------------------------##

## Filtering State SC from File
data_SC<-subset(data3, Estado_Production=='SC')
rownames(data_SC) <- seq(length=nrow(data_SC))
SC_ts <- ts(data_SC[,c(4)],start=c(min_year,1),frequency=12)
ARIMAfit <- arima(SC_ts,order=c(1,1,2),seasonal=list(order=c(2,1,1),period=12))
pred <- forecast(ARIMAfit,h=forecast_points)
set_plot_dir()
jpeg('Biodiesel_Prod (SC).jpg')
plot(pred,ylab="Biodiesel Prod (m3)", main="Forecasting (SC) - Biodiesel Production (m3)")
dev.off()
set_output_dir()
write.csv(pred,"Prediction - Biodiesel_Production (SC).csv")

##-------------------------------------------------------------------------------------------##

## Filtering State SC from File
data_SC<-subset(data4, Estado_Consumption=='SC')
rownames(data_SC) <- seq(length=nrow(data_SC))
SC_ts <- ts(data_SC[,c(4)],start=c(min_year,1),frequency=12)
ARIMAfit <- arima(SC_ts,order=c(1,1,1),seasonal=list(order=c(1,2,1),period=12))
pred <- forecast(ARIMAfit,h=forecast_points)
set_plot_dir()
jpeg('Biodiesel_Consump (SC).jpg')
plot(pred,ylab="Biodiesel Consump (m3)", main="Forecasting (SC) - Biodiesel Consumption (m3)")
dev.off()
set_output_dir()
write.csv(pred,"Prediction - Biodiesel_Consumption (SC).csv")

##-------------------------------------------------------------------------------------------##

## Filtering State SP from File
data_SP<-subset(data3, Estado_Production=='SP')
rownames(data_SP) <- seq(length=nrow(data_SP))
SP_ts <- ts(data_SP[,c(4)],start=c(min_year,1),frequency=12)
ARIMAfit <- arima(SP_ts,order=c(1,1,2),seasonal=list(order=c(2,1,1),period=12))
pred <- forecast(ARIMAfit,h=forecast_points)
set_plot_dir()
jpeg('Biodiesel_Prod (SP).jpg')
plot(pred,ylab="Biodiesel Prod (m3)", main="Forecasting (SP) - Biodiesel Production (m3)")
dev.off()
set_output_dir()
write.csv(pred,"Prediction - Biodiesel_Production (SP).csv")

##-------------------------------------------------------------------------------------------##

## Filtering State SP from File
data_SP<-subset(data4, Estado_Consumption=='SP')
rownames(data_SP) <- seq(length=nrow(data_SP))
SP_ts <- ts(data_SP[,c(4)],start=c(min_year,1),frequency=12)
ARIMAfit <- arima(SP_ts,order=c(1,1,1),seasonal=list(order=c(1,2,1),period=12))
pred <- forecast(ARIMAfit,h=forecast_points)
set_plot_dir()
jpeg('Biodiesel_Consump (SP).jpg')
plot(pred,ylab="Biodiesel Consump (m3)", main="Forecasting (SP) - Biodiesel Consumption (m3)")
dev.off()
set_output_dir()
write.csv(pred,"Prediction - Biodiesel_Consumption (SP).csv")

##-------------------------------------------------------------------------------------------##
