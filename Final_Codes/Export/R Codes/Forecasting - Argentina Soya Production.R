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
path1<-paste0(path,"/Export/Data Directory/Soya_Bran")
path2<-paste0(path,"/Export/Data Directory/Soya_Bran/Plots - Covariates/")
path3<-paste0(path,"/Export/Data Directory/Soya_Bran/Prediction - Covariates/")
path4<-paste0(path,"/Production/Data Directory/")
path5<-paste0(path,"/Export/Data Directory/Soya_Bran/covariates_results/")

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

set_results_dir=function()
{
  setwd(path5)
}

## Setting Directory for Retrieving Year Variable from Production directory
set_production_dir()
year_variables=read.csv(paste0(path4,"year_variables.csv"),header = FALSE)

## Setting Directory for Argentina Production in Export Directory
set_input_dir()
argentina_prod=read.csv("argentina_prod_value.csv",header = TRUE)

max_year=year_variables$V2[1]
min_year=argentina_prod$Ano[1]
max_month=year_variables$V2[3]
min_month=year_variables$V2[4]

data_points=48
max_len=12
last_year=nrow(argentina_prod)

total_data_points=data_points+max_len

## Function to trim the string from last n characters
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

##--------------------------------------Argentina Production-------------------------------------------##
set_input_dir()
argentina_prod<-read.csv("Argentina_Prod_Input.csv",header = T, stringsAsFactors = FALSE)
argentina_prod<-subset(argentina_prod,Ano>=min_year)
argentina_prod<-subset(argentina_prod,Ano<=argentina_prod$Ano[last_year])

## Data type Conversion
argentina_prod<-as.data.frame(apply(argentina_prod, 2, as.numeric))

## Adding 1 to Argentina Production before logging to prevent NA's
argentina_prod$Argentina_Production<-argentina_prod$Argentina_Production+1
prod_ts <- ts(argentina_prod[,c(3)],start=c(min_year,1),frequency=12)

log_data<-log(argentina_prod[,c(3:4)])

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
ARIMAfit <- arima(ts_pred,order=c(0,0,0),seasonal=list(order=c(0,1,1),period=12),xreg = train_pred[,c(2)])

pred_test <- forecast(ARIMAfit,h=max_len,xreg = test_pred[,c(2)],level = c(70,75,80,85,90,95))
df_test=data.frame(pred_test)

## Unscaling the data
unscaled_test<-unscale(df_test$Point.Forecast,log_data$Argentina_Production)

## Unlogging the unscaled data
unlog_test<-exp(unscaled_test)
final_test=unlog_test-1

## Getting the fitted values
train_fit<-fitted(ARIMAfit)
unscaled_train<-unscale(train_fit,log_data$Argentina_Production)
unlog_train<-exp(unscaled_train)
unlog_train=unlog_train-1

## combination of training fitted and test values
final_predicted<-c(unlog_train,final_test)

## Creating time series for fitted data
pred=ts(final_predicted,start=c(min_year,1),frequency=12)

## Plotting the original and validated dataset
org_ts <- ts(argentina_prod[,c(3)],start=c(min_year,1),frequency=12)
set_plot_dir()
jpeg('Argentina_Production (Soya).jpg')
plot(org_ts,ylab="Soya Production ('000 tonnes)", main="Production Forecast - Argentina")
lines(pred,col="red")
dev.off()

dataframe=as.data.frame(final_predicted)
colnames(dataframe)<-c("Predicted Production")
Year<-argentina_prod$Ano
Month<-argentina_prod$Mes
dataframe<-cbind(Year,Month,dataframe)
dataframe$`Predicted Production`<-ifelse(dataframe$`Predicted Production`<0,0,dataframe$`Predicted Production`)

set_covariate_dir()
write.csv(dataframe,"Predicted-Production_Argentina.csv",row.names = FALSE)

## unscaling all
unscale<-unscale(df_test,log_data)

## Unlogging all
unlog<-exp(unscale)
final<-unlog-1

confidence<-final
confidence <- cbind(Year = rownames(confidence), confidence)
Year<-as.character(confidence$Year)
confidence_values<-apply(confidence[2:14], 2, FUN = function(x){ifelse(x<0,0,x)})
confidence<-cbind(Year,confidence_values)
rownames(confidence) <- NULL

DF<-confidence
DF<-DF[,c("Year","Point.Forecast","Lo.80","Hi.80","Lo.95","Hi.95")]
rownames(DF)<-DF[,c("Year")]
DF<-as.data.frame(DF)
DF$Year<-NULL
colnames(DF)<-c("Argentina Soya Production","Low 80","High 80","Low 95","High 95")

set_covariate_dir()
write.csv(confidence,"Confidence_Argentina.csv",row.names = FALSE)
