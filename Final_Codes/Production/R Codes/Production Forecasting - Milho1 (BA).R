## To clear the screen
rm(list=ls())

library(forecast)
library(tseries)
library(imputeTS)
library(dplyr)
library(plyr)
library(tidyr)

setwd("..")
setwd("..")
setwd("..")
path<-paste0(getwd())
print(path)
path1<-paste0(path,"/Production/Data Directory/")
path2<-paste0(path,"/Production/Data Directory/Plots/")
path3<-paste0(path,"/Production/Data Directory/Prediction - Covariates/")
path4<-paste0(path,"/Production/Data Directory/Milho1 Input Files/")
path5<-paste0(path,"/Production/Data Directory/Jan2019/Milho1_-_Production_Model_Re-Run_Results/Milho1 Predicted Production New Results/")
path6<-paste0(path,"/Production/Data Directory/Jan2019/Milho1_-_Production_Model_Re-Run_Results/Milho1 Confidence New Results/")

set_production_dir=function()
{
  setwd(path1)
}

set_input_dir=function()
{
  setwd(path4)
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

set_production_dir()
year_variables=read.csv("year_variables.csv",header = FALSE)
agro_yearly=read.csv("Monthly_Final.csv",header = TRUE)

max_year=year_variables$V2[1]
min_year=year_variables$V2[2]
max_month=year_variables$V2[3]
min_month=year_variables$V2[4]
year_pred=year_variables$V2[5]

data_points=(max_year-min_year)*12-(min_month-1)+max_month

max_len=((year_pred-max_year)*12)+(12-max_month)

total_data_points=data_points+max_len

## Yearly value for predicted years
agro_yearly<-subset(agro_yearly,Years<=year_pred)

agro_yearly$Month<-as.numeric(agro_yearly$Month)
agro_yearly$Years<-as.numeric(agro_yearly$Years)

agro_yearly$Start_int<-ifelse(agro_yearly$Month<=7,-1,0)
agro_yearly$Start_Year<-agro_yearly$Years+agro_yearly$Start_int

agro_yearly$End_int<-ifelse(agro_yearly$Month>7,1,0)
agro_yearly$End_Year<-agro_yearly$Years+agro_yearly$End_int

agro_yearly<-transform(agro_yearly,Agro_Year=paste0(Start_Year,"-",End_Year))
agro_yearly$Start_int<-NULL
#agro_yearly$Start<-NULL
agro_yearly$End_int<-NULL
#agro_yearly$End<-NULL

agro_yearly_pred<-subset(agro_yearly, UF=='BA' & Produto=='Milho 1 Safra' & Type=='Producao' & Start_Year>=(max_year-1))
#agro_yearly_pred = agro_yearly_pred %>% distinct(Yearly_value)
agro_yearly_pred<-ddply(agro_yearly_pred, .(Agro_Year), summarise, Yearly_value = mean(Yearly_value))
#agro_yearly_pred<-agro_yearly_pred %>% group_by(Agro_Year)%>% summarise(Yearly_value=sum(Monthly_Value))
#agro_yearly_pred<-subset(agro_yearly_pred, Month==4, select = c(Years, Yearly_value))
#agro_yearly_pred<-agro_yearly_pred$Yearly_value
agro_yearly_pred$Yearly_value[1]

## Monthly value for actual year
agro_monthly_act<-subset(agro_yearly, UF=='BA' & Produto=='Milho 1 Safra' & Type=='Producao' & Start_Year>=(max_year-1))
#agro_monthly_act<-agro_monthly_act %>% summarise(Actual_prediction=sum(Monthly_Value))
agro_monthly_act<-ddply(agro_monthly_act, .(Agro_Year), summarise, monthly_act = sum(Monthly_Value))
#agro_monthly_act<-unlist(agro_monthly_act)
agro_monthly_act[[1]]

agro_len=nrow(agro_yearly_pred)

# if(max_month==12)
# {
#   agroconsult_to_adjust=agro_yearly_pred$Yearly_value[2]
# }

# if(max_month<12)
# {
data3<-merge(x = agro_yearly_pred, y = agro_monthly_act, by = "Agro_Year", all.x = TRUE)
data3$monthly_act<-ifelse((data3$monthly_act/data3$Yearly_value)<=0.1,data3$Yearly_value,data3$monthly_act)
#zero_added=rep(0, agro_len-1)
#agro_monthly_act=c(agro_monthly_act$Yearly_value,zero_added)
#agroconsult_to_adjust_1=agro_yearly_pred$Yearly_value-agro_monthly_act$Yearly_value
data3$agroconsult_to_adjust=data3$Yearly_value-data3$monthly_act
# }

## Yearly list
year_list<-subset(agro_yearly, UF=='BA' & Produto=='Milho 1 Safra' & Type=='Producao')
year_list<-year_list$Years
year_list

## Yearly values for 2012 to 2019
agro_yearly<-subset(agro_yearly, UF=='BA' & Produto=='Milho 1 Safra' & Type=='Producao')
#agro_yearly<-agro_yearly %>% distinct(Years,Yearly_value)
#agro_yearly<-agro_yearly %>% group_by(Years)%>% summarise(Yearly_value=sum(Monthly_Value))
#agro_yearly<-subset(agro_yearly, Month==4, select = c(Agro_Year, Yearly_value))
agro_yearly<-ddply(agro_yearly, .(Agro_Year), summarise, Yearly_value = mean(Yearly_value))

set_input_dir()
routes <- read.csv("Milho1_BA.csv", header = T, stringsAsFactors = FALSE)
routes[is.na(routes)] <- 0

routes$Production<-routes$Production+1

log_data<-log(routes[,c(2:4)])

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

# ## Auto Arima
# 
# ARIMAfit <- auto.arima(ts_pred)
# pred_test <- forecast(ARIMAfit,h=max_len,level = c(70,75,80,85,90,95))

## Manual Arima Fit
#ARIMAfit <- arima(ts_pred,order=c(0,0,0),seasonal=list(order=c(1,1,1),period=12),xreg = train_pred[,c(2:3)])
ARIMAfit <- auto.arima(ts_pred,xreg = train_pred[,c(2:3)])

pred_test <- forecast(ARIMAfit,h=max_len,xreg = test_pred[,c(2:3)],level = c(70,75,80,85,90,95))

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
org_ts <- ts(routes[,c(2)],start=c(min_year,1),frequency=12)
set_plot_dir()
jpeg('Milho1_BA.jpg')
plot(org_ts,ylab="Production ('000 tonnes)", main="Production Forecast - Milho1 (BA)")
lines(pred,col="red")
dev.off()

dataframe=as.data.frame(final_predicted)
colnames(dataframe)<-c("Predicted Production")
Year<-routes$Date
dataframe<-cbind(Year,dataframe)
dataframe$`Predicted Production`<-ifelse(dataframe$`Predicted Production`<0,0,dataframe$`Predicted Production`)

set_predicted_dir()
write.csv(dataframe,file="Predicted-Production_BA.csv",row.names = FALSE)

dataframe_2<-cbind(year_list,dataframe)
colnames(dataframe_2)<-c('Year','Date','Predicted')

data_new=separate(dataframe_2,Date,into=c("Month" , "Day","Year" ),sep="/")
data_new$Month<-as.numeric(data_new$Month)
data_new$Year<-as.numeric(data_new$Year)

data_new$Start_int<-ifelse(data_new$Month<=7,-1,0)
data_new$Start_Year<-data_new$Year+data_new$Start_int

data_new$End_int<-ifelse(data_new$Month>7,1,0)
data_new$End_Year<-data_new$Year+data_new$End_int

data_new<-transform(data_new,Agro_Year=paste0(Start_Year,"-",End_Year))
Agro_Year<-data_new$Agro_Year
dataframe_2<-cbind(dataframe_2,Agro_Year)

dataframe_3<-ddply(dataframe_2, .(Agro_Year), summarise, sum_prediction = sum(Predicted))
colnames(dataframe_3)=c('Agro_Year','Sum_Prediction')

final_dataframe=merge(x = agro_yearly, y = dataframe_3, by = "Agro_Year", all.x = TRUE)

## combining prediction and agrocunsult values together
#final_dataframe=cbind(agro_yearly,dataframe_3$Sum_Prediction)
colnames(final_dataframe)<-c('Agro_Year','Agroconsult','Prediction')
final_dataframe$Ratio<-final_dataframe$Agroconsult/final_dataframe$Prediction
final_dataframe$Ratio<-ifelse(final_dataframe$Ratio<=2,final_dataframe$Ratio,1)

new_dataframe=merge(x = dataframe_2, y = final_dataframe, by = "Agro_Year", all.x = TRUE)
Date<-new_dataframe$Date

data_new=separate(new_dataframe,Date,into=c("Month","Day","Year"),sep="/")
data_new=cbind(data_new,Date)
data_new$Month<-as.numeric(data_new$Month)
data_new$Year<-as.numeric(data_new$Year)
data_new<-data_new[with(data_new, order(Year, Month)),]

Date<-data_new$Date

rownames(data_new) <- 1:nrow(data_new)
data_new$Ratio[is.na(data_new$Ratio)] <- 1

new_dataframe<-data_new
new_dataframe$Predicted<-new_dataframe$Predicted*new_dataframe$Ratio
new_dataframe$Year<-NULL
new_dataframe$Agroconsult<-NULL
new_dataframe$Prediction<-NULL
new_dataframe$Ratio<-NULL
new_dataframe$Day<-NULL
new_dataframe$Month<-NULL
new_dataframe$Date<-NULL
new_dataframe<-cbind(Date,new_dataframe)
new_dataframe$Agro_Year<-NULL
colnames(new_dataframe)<-c("Year","Predicted Production")

write.csv(new_dataframe,file="Predicted-Production_BA.csv",row.names = FALSE)

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

set_confidence_dir()
write.csv(confidence,file="Confidence_BA.csv",row.names = FALSE)

Year=tail(year_list, max_len)
df2=as.data.frame(confidence_values)
str(df2)
df2=cbind(Year,df2)
df2$Month<-(max_month+1):12

df2$Month<-as.numeric(df2$Month)
df2$Year<-as.numeric(df2$Year)

df2$Start_int<-ifelse(df2$Month<=7,-1,0)
df2$Start_Year<-df2$Year+df2$Start_int

df2$End_int<-ifelse(df2$Month>7,1,0)
df2$End_Year<-df2$Year+df2$End_int

df2<-transform(df2,Agro_Year=paste0(Start_Year,"-",End_Year))

predicted<-ddply(df2, .(Agro_Year), summarise, Predicted_Prod = sum(Point.Forecast))
#predicted=df2 %>% group_by(Agro_Year) %>% summarise(Predicted_Prod=sum(Point.Forecast))
#actual_pred=cbind(predicted,agroconsult_to_adjust)
actual_pred=merge(x = predicted, y = data3, by = "Agro_Year", all.x = TRUE)
actual_pred$Ratio=actual_pred$agroconsult_to_adjust/actual_pred$Predicted_Prod

confidence_new_data=merge(x = df2, y = actual_pred, by = "Agro_Year",all.x=TRUE)

confidence_new_data[is.na(confidence_new_data)] <- 0

confidence_new_data[3:15]<-confidence_new_data$Ratio*confidence_new_data[3:15]
# confidence_new_data$Predicted_Prod<-NULL
# confidence_new_data$agroconsult_to_adjust<-NULL
# confidence_new_data$Ratio<-NULL
confidence_new_data<-confidence_new_data[,c(1:15)]
Year<-confidence[,1]
confidence_new_data$Year<-Year
# confidence_new_data$Years<-NULL
# confidence_new_data$Yearly_value<-NULL
confidence_new_data$Agro_Year<-NULL
rownames(confidence_new_data) <- NULL

write.csv(confidence_new_data,file="Confidence_BA.csv",row.names = FALSE)
set_plot_dir()
