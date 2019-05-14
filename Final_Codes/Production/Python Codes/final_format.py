# -*- coding: utf-8 -*-
"""
Created on Mon Apr  8 15:26:55 2019

@author: agupta466
"""
import pandas as pd
import numpy as np
import fnmatch
import datetime
import sys
from pathlib import Path
import os
import colorama
from colorama import init
import termcolor
from termcolor import colored  

here = Path(__file__).resolve()
path = here.parents[2]

sys.path.append(str(path)+"/Export/Python Codes")
sys.path.append(str(path)+"/Production/Python Codes")

import Forecasting_Export as export
from Forecasting_Export import add_state_code, read_all_files, change_date_to_month, repeat_fun, calculating_total_zones, get_ratio_file, retrieving_zones, prev_years_ratios, valid_years, preprocessing_state_to_zone, calculate_state_to_zone, retrieving_confidence, add_confidence, add_confidence2, merge_all_states, try_cutoff, get_date, Replace

import Forecasting_Production as production
from Forecasting_Production import year_preprocessing, get_state_file, get_harvest_plantation_file, data_preprocessing_yearly

encoding='ISO-8859-15'
prefix='20'

def non_states_monthly(actual_data,pred_data,year_var,crop_type):
    prod_states=actual_data['Estado'].unique()
    model_states=pred_data['UF'].unique()
    non_model_state_list=list(set(prod_states).difference(model_states))
    
    production_subset=actual_data[(actual_data['Type']=='Producao')&(actual_data['Produto']==crop_type)]
    print(production_subset)
    production_subset=production_subset[production_subset['Estado'].isin(non_model_state_list)]
    print(production_subset)
    
    production_subset['Years'] = production_subset['Years'].apply(lambda x: x.split('-')[1])
    production_subset['Years'] = production_subset['Years'].astype(int)
    
    all_prod_list=production_subset['Estado'].unique()
    no_prod_states_list=list(set(non_model_state_list).difference(all_prod_list))
    print(no_prod_states_list)
    
    month_list=list(range(1,13))
    year_list=list(range(year_var[1][1],(int(year_var[1][4])+1)))
    print(year_list)
    columns=['Produto','UF','Type','Years','Month','Percentage','Yearly_value','Monthly_Value']
    df1=pd.DataFrame(0,index=np.arange(len(month_list)),columns=columns)
    df=pd.DataFrame(columns=columns)
    
    if(crop_type=='Milho 1 Safra'):
        percentage_list=[0,0.25,0.25,0.25,0.25,0,0,0,0,0,0,0]
    if(crop_type=='Milho 2 Safra'):
        percentage_list=[0,0,0,0,0.25,0.25,0.25,0.25,0,0,0,0]
    if(crop_type=='Soja'):
        percentage_list=[0,0,0.25,0.25,0.25,0.25,0,0,0,0,0,0]
    
    for i in range(len(no_prod_states_list)):
        for j in range(len(year_list)):
            df1['Years']=year_list[j]
            df1['Month']=month_list
            df1['Type']='Producao'
            df1['Produto']=crop_type
            df1['UF']=no_prod_states_list[i]
            df1['Percentage']=percentage_list
            df1['Yearly_value']=0
            df1['Monthly_Value']=0   
            df=df.append(df1)
        
    for index,row in production_subset.iloc[:,:].iterrows():    
        df1['Years']=production_subset.loc[index,'Years']
        df1['Month']=month_list
        df1['Type']='Producao'
        df1['Produto']=crop_type
        df1['UF']=production_subset.loc[index,'Estado']
        df1['Percentage']=percentage_list
        df1['Yearly_value']=production_subset.loc[index,'Value']
        df1['Monthly_Value']=0
        df=df.append(df1)   
        
    df['Monthly_Value']=df['Yearly_value']*df['Percentage']
    df=df.reset_index(drop=True)
    return(df)

def getting_all_zones(vli_agro,year_var):
    year=valid_years(year_var)
    len_year=len(year)
    vli_agro=vli_agro.iloc[0:57]
    vli_agro=vli_agro.reset_index(level=1)
    vli_agro_list=[0,1,2,4,6,7,8,9]
    vli_agro_list=vli_agro_list[:(len_year)]
    vli_agro=vli_agro.iloc[:, lambda vli_agro: vli_agro_list]
    vli_agro.columns = year
    vli_agro.index.name='Zonas'
    return(vli_agro)

def retrieving_production_predictions(p):   
    # Getting Results from Production Predictions
    all_files=os.listdir(p)
    export_predict_files = fnmatch.filter(all_files, "*Predicted-Production*")
    df = read_all_files(export_predict_files,p)
    
    df.rename(columns={'Year': 'Date'}, inplace=True)
    df=change_date_to_month(df)    
    complete_list_code=add_state_code(df,export_predict_files,'predicted_production')
    df.insert(2,'UF',complete_list_code)
    return(df)
    
def retrieving_actual_production(actual_data,year_var):
    max_year=year_var[1][0]
    min_year=year_var[1][1]
    max_month=year_var[1][2]
    print(actual_data)
    actual_data1 = actual_data[(actual_data['Years']>=min_year)&(actual_data['Years']<max_year)]
    actual_data2 = actual_data[(actual_data['Years']==max_year)&(actual_data['Month']<=max_month)]
    final_actual = pd.concat([actual_data1.reset_index(drop=True),actual_data2.reset_index(drop=True)],axis=0)
    final_actual = final_actual.reset_index(drop=True)
    return(final_actual)

def retrieving_predicted_production(pred_data,year_var):
    max_year=year_var[1][0]
    max_month=year_var[1][2]
    pred_data1 = pred_data[(pred_data['Year']==max_year)&(pred_data['Month']>max_month)]
    pred_data2 = pred_data[(pred_data['Year']>max_year)]
    pred_full = pd.concat([pred_data1.reset_index(drop=True),pred_data2.reset_index(drop=True)],axis=0)
    pred_full = pred_full.reset_index(drop=True)
    return(pred_full)
    
def calculating_zones_ratio(vli_agro,year_var):
    year_list=valid_years(year_var)              
    len_year_list=len(year_list)    
    for index,row in vli_agro.iloc[:,:].iterrows():
        for i in range(len(year_list)):
            if (row[3+len_year_list+i]!=0) and (row[3+i]!=0):
                vli_agro.loc[index,'Ratio-'+str(year_list[i])] = row[3+i]/row[3+len_year_list+i]
            if (row[3+len_year_list+i]==0):
                vli_agro.loc[index,'Ratio-'+str(year_list[i])] = 0
            if (row[3+len_year_list+i]==0) and (row[3+i]==0):
                vli_agro.loc[index,'Ratio-'+str(year_list[i])] = 1 
    
    vli_agro=prev_years_ratios(vli_agro,year_var)      
    return(vli_agro)
        
def main():
    
    init()
    print(colored('\nProduction Forecast Results - Format Building Starts\n','green'))
    
    ## Initialize state file 
    state1=get_state_file('../Input Files/')
    
    ## Reading harvest plantation monthly division file
    harvest_plantation=get_harvest_plantation_file('../Input Files/',state1)
 
    ## Years & Months Pre-processing (monthly file)
    years=harvest_plantation.iloc[:,5:]
    years=year_preprocessing(years)
    year_list_monthly=years.columns
    
    harvest_plantation_yearly=pd.read_excel("../Input Files/Detalhe_Safra.xls",encoding=encoding)
    combined_yearly=data_preprocessing_yearly(harvest_plantation_yearly,year_list_monthly)
    
    year_var=pd.read_csv(str(path)+'/Production/Data Directory/year_variables.csv',header=None)
# =============================================================================
#     if(year_var[1][2]==12):
#         year_var[1][0]=year_var[1][0]+1
#         year_var[1][2]=0
# =============================================================================
    
    print(colored('\nCalculating Brazil State to Zone Ratios...\n','green'))
    print(colored('\nReading Zone Wise Production File for Soya and Milho...\n','green'))
    
    ## calculating State to Zone Ratio - Soya
    vli_agro=pd.ExcelFile(str(path)+"/Production/Input Files/Prod_longo_prazo.xlsx",encoding=encoding)
    
    print(colored('\nCalculating State to Zones Ratio for Soya...\n','green'))
    
    vli_agro_soya = pd.read_excel(vli_agro, 'Resultados soja e farelo')
    vli_agro_soya=vli_agro_soya.iloc[11:68]
    
    soya_zones=getting_all_zones(vli_agro_soya,year_var)
    all_zones_soya=retrieving_zones(soya_zones)
    
    final_zones_soya=calculating_total_zones(all_zones_soya,year_var)
    final_zones_soya.to_csv(str(path)+'/Production/Data Directory/Soya_Zones_values.csv',encoding=encoding,index=False)    
    
    zones_ratio_soya = calculating_zones_ratio(final_zones_soya,year_var)
    zones_ratio_soya.to_csv(str(path)+'/Production/Data Directory/Soya_State_to_Zone_Ratio.csv',encoding=encoding,index=False)
    
    print(colored('\nSoya - State to Zones Ratio Completed!...\n','green'))
    
    print(colored('\nCalculating State to Zones Ratio for Milho...\n','green'))
    ## Calculating State to Zone Ratio - Milho
    vli_agro_milho = pd.read_excel(vli_agro, 'Resultados milho')
    vli_agro_milho=vli_agro_milho.iloc[8:65]
    
    milho_zones=getting_all_zones(vli_agro_milho,year_var)
    all_zones_milho=retrieving_zones(milho_zones)
    
    final_zones_milho=calculating_total_zones(all_zones_milho,year_var)
    final_zones_milho.to_csv(str(path)+'/Production/Data Directory/Milho_Zones_values.csv',encoding=encoding,index=False)
       
    zones_ratio_milho=calculating_zones_ratio(final_zones_milho,year_var)
    zones_ratio_milho.to_csv(str(path)+'/Production/Data Directory/Milho_State_to_Zone_Ratio.csv',encoding=encoding,index=False)
    
    print(colored('\nMilho - State to Zones Ratio Completed!...\n','green'))
    
    print(colored('\nRetrieving Predictions for All Crops - Milho 1ª Safra, Milho 2ª Safra & Soya...\n','green'))
    
    ## Retrieving Prediction Values - All Crops
    milho1_path=str(path)+'/Production/Data Directory/Jan2019/Milho1_-_Production_Model_Re-Run_Results/Milho1 Predicted Production New Results/'
    milho1_predict=retrieving_production_predictions(milho1_path)
    
    milho2_path=str(path)+'/Production/Data Directory/Jan2019/Milho2_Production_Model_Re-Run_Results/Milho2 Predicted Production New Results/'
    milho2_predict=retrieving_production_predictions(milho2_path)
    
    soya_path=str(path)+'/Production/Data Directory/Jan2019/Production_Re-Run_Results_-_Soya/Soya_Predicted_Production_New_Results/'
    soya_predict=retrieving_production_predictions(soya_path)
    
    print(colored('\nMilho 1ª Safra - Converting Non Model States Agroconsult Yearly Estimated into Monthly...\n','green'))
    
    ## Converting Non Model States Agroconsult Yearly Estimates into Monthly - All Crops
    milho1_non_model=non_states_monthly(combined_yearly,milho1_predict,year_var,"Milho 1 Safra")
    milho1_non_model.to_csv(str(path)+'/Production/Data Directory/Milho1_Non_Model.csv',index=False,encoding=encoding)
    
    print(colored('\nMilho 1ª Safra - Conversion Completed!...\n','green'))
    print(colored('\nMilho 2ª Safra - Converting Non Model States Agroconsult Yearly Estimated into Monthly...\n','green'))
    
    milho2_non_model=non_states_monthly(combined_yearly,milho2_predict,year_var,"Milho 2 Safra")
    milho2_non_model.to_csv(str(path)+'/Production/Data Directory/Milho2_Non_Model.csv',index=False,encoding=encoding)
    
    print(colored('\nMilho 2ª Safra - Conversion Completed!...\n','green'))
    print(colored('\nSoya - Converting Non Model States Agroconsult Yearly Estimated into Monthly...\n','green'))
    
    soya_non_model=non_states_monthly(combined_yearly,soya_predict,year_var,"Soja")
    soya_non_model.to_csv(str(path)+'/Production/Data Directory/Soya_Non_Model.csv',index=False,encoding=encoding)
    
    print(colored('\nSoya - Conversion Completed!...\n','green'))
    print(colored('\nRetrieving Actual Values for all Model States - All Crops...\n','green'))
    
    ## Reading Actual States for which Models developed
    monthly_final=pd.read_csv(str(path)+'/Production/Data Directory/Monthly_Final.csv',encoding=encoding)
    monthly_final['Years']=monthly_final['Years'].astype(int)
    monthly_final['Month']=monthly_final['Month'].astype(int)
    
    del monthly_final['Cumulative_Prod']
    
    soya_actual=monthly_final[(monthly_final['Type']=='Producao')&(monthly_final['Produto']=='Soja')]
    soya_actual=retrieving_actual_production(soya_actual,year_var)
    
    milho1_actual=monthly_final[(monthly_final['Type']=='Producao')&(monthly_final['Produto']=='Milho 1 Safra')]
    milho1_actual=retrieving_actual_production(milho1_actual,year_var)
    
    milho2_actual=monthly_final[(monthly_final['Type']=='Producao')&(monthly_final['Produto']=='Milho 2 Safra')]
    milho2_actual=retrieving_actual_production(milho2_actual,year_var)
    
    ## ---------------------------------------------- Soya -------------------------------------------------- ##
    
    print(colored('\nCalculating Zone-wise Production Values for all Model States - Soya...\n','green'))
    
    ## Calculating Zone-wise Actuals for Actual States
    soya_actual=soya_actual[['Years','Month','UF','Monthly_Value']]
    soya_state_to_zone=pd.read_csv(str(path)+'/Production/Data Directory/Soya_State_to_Zone_Ratio.csv',encoding=encoding)
    soya_state_to_zone=preprocessing_state_to_zone(soya_state_to_zone)
    
    zonewise_soya_model_actual=calculate_state_to_zone(soya_actual,soya_state_to_zone,'pred')
    zonewise_soya_model_actual.columns=['Year','Month','UF','Zonas','Production']
    
    print(colored('\nCalculating Zone-wise Production Values for all Non - Model States - Soya...\n','green'))
    
    soya_non_model=pd.read_csv(str(path)+'/Production/Data Directory/Soya_Non_Model.csv',encoding=encoding)
    
    soya_non_model_actual=retrieving_actual_production(soya_non_model,year_var)
    soya_non_model_actual=soya_non_model_actual[['Years','Month','UF','Monthly_Value']]
    
    zonewise_soya_nonmodel_actual=calculate_state_to_zone(soya_non_model_actual,soya_state_to_zone,'pred')
    zonewise_soya_nonmodel_actual.columns=['Year','Month','UF','Zonas','Production']
    
    ## Combining Model and Non-Model States
    all_states_soya_zonewise=pd.concat([zonewise_soya_model_actual,zonewise_soya_nonmodel_actual],axis=0)
    all_states_soya_zonewise=all_states_soya_zonewise.reset_index(drop=True)
    all_states_soya_zonewise=add_confidence(all_states_soya_zonewise)
    all_states_soya_zonewise['Tipo']='Real'
    all_states_soya_zonewise['Tipo Ajustado']='Real / Calculada'
    all_states_soya_zonewise.to_csv(str(path)+'/Production/Data Directory/Soya_Actual_Production.csv',index=False,encoding=encoding)
    
    ## Getting Confidence Results for Predicted States
    p=str(path)+'/Production/Data Directory/Jan2019/Production_Re-Run_Results_-_Soya/Soya_Confidence_New_Results/'
    soya_confidence_result=retrieving_confidence(p,'prod')
    soya_confidence_result.rename(columns={'Month_Num': 'Month','Ano': 'Year'}, inplace=True)
    predicted_soya_prod=retrieving_predicted_production(soya_predict,year_var)
    predicted_soya_prod=pd.merge(predicted_soya_prod[['Year','Month','UF']],soya_confidence_result[['Year','Month','UF','Point.Forecast']],on=['Year','Month','UF'],how='outer',right_index=False,sort=True,indicator=True).query("_merge == 'both'").drop('_merge',axis=1)
    predicted_soya_zonewise=calculate_state_to_zone(predicted_soya_prod,soya_state_to_zone,'pred')
      
    #soya_confidence_result=soya_confidence_result.drop(soya_confidence_result.columns[15],axis=1)
    del soya_confidence_result['Point.Forecast']
    soya_confidence_result=calculate_state_to_zone(soya_confidence_result,soya_state_to_zone,'confidence')
    soya_confidence_result.to_csv(str(path)+'/Production/Data Directory/Soya_Confidence_Production.csv',encoding=encoding,index=False)
    
    ## Merging the Confidence Interval Results with Export Predictions
    soya_zonewise_predicted=pd.merge(predicted_soya_zonewise,soya_confidence_result,on=['Year','Month','UF','Zonas'],how='outer',right_index=False,sort=True,indicator=True).query("_merge != 'right_only'").drop('_merge',axis=1)
    soya_zonewise_predicted.fillna(0)
    soya_zonewise_predicted['Tipo']='Prevista'
    soya_zonewise_predicted['Tipo Ajustado']='Prevista'
    soya_zonewise_predicted.rename(columns={'Zones_Export': 'Production'}, inplace=True)
    soya_zonewise_predicted.to_csv(str(path)+'/Production/Data Directory/Soya_Predicted_Production.csv',encoding=encoding,index=False)
    
    ## Getting all Fitted Values - Zones
    soya_predict.rename(columns={'Year': 'Years'}, inplace=True)
    fitted_production_soya=retrieving_actual_production(soya_predict,year_var)  
    fitted_zonewise_production_soya=calculate_state_to_zone(fitted_production_soya,soya_state_to_zone,'pred')
    fitted_zonewise_production_soya=add_confidence(fitted_zonewise_production_soya)
    fitted_zonewise_production_soya.rename(columns={'Zones_Export': 'Production'}, inplace=True)
    fitted_zonewise_production_soya['Tipo']='Calculada'
    fitted_zonewise_production_soya['Tipo Ajustado']='Real / Calculada'
    fitted_zonewise_production_soya.to_csv(str(path)+'/Production/Data Directory/Soya_ModelFitted_Production.csv',encoding=encoding,index=False)
    
    ## Getting Non Model Predicted Values - Zones
    soya_non_model.rename(columns={'Years': 'Year'}, inplace=True)
    soya_non_model_pred=retrieving_predicted_production(soya_non_model,year_var)
    soya_non_model_pred=soya_non_model_pred[['Year','Month','UF','Monthly_Value']]
    soya_zonewise_non_model_pred=calculate_state_to_zone(soya_non_model_pred,soya_state_to_zone,'pred')
    soya_zonewise_non_model_pred=add_confidence2(soya_zonewise_non_model_pred)
    soya_zonewise_non_model_pred.rename(columns={'Zones_Export': 'Production'}, inplace=True)
    soya_zonewise_non_model_pred['Tipo']='Prevista'
    soya_zonewise_non_model_pred['Tipo Ajustado']='Prevista'
    soya_zonewise_non_model_pred.to_csv(str(path)+'/Production/Data Directory/Soya_NonModel_Predicted_Production.csv',encoding=encoding,index=False)
    
    ## Getting Non Model Fitted Values - Zones 
    zonewise_soya_nonmodel_fitted=zonewise_soya_nonmodel_actual
    zonewise_soya_nonmodel_fitted['Production']=0
    zonewise_soya_nonmodel_fitted=add_confidence(zonewise_soya_nonmodel_fitted)
    zonewise_soya_nonmodel_fitted['Tipo']='Calculada'
    zonewise_soya_nonmodel_fitted['Tipo Ajustado']='Real / Calculada'
    zonewise_soya_nonmodel_fitted.to_csv(str(path)+'/Production/Data Directory/Soya_NonModel_Fitted_Production.csv',encoding=encoding,index=False)
    
    print(colored('\nCombining Zone-wise Model and Non-Model States Production Values - Soya...\n','green'))
    
    soya_prod=merge_all_states(all_states_soya_zonewise,fitted_zonewise_production_soya,soya_zonewise_predicted,soya_zonewise_non_model_pred,zonewise_soya_nonmodel_fitted)  
    
    soya_prod.insert(0,'Produto','Soja')
    soya_prod=soya_prod[['Produto','Month','Year','UF','Zonas','Low','Production','High','Tipo','Tipo Ajustado','Conf']]
    soya_prod.columns=['Produto',u'Mês','Ano','UF',u'Microrregião',u'Produção Mín (Kt)',u'Produção Med (Kt)',u'Produção Max (Kt)','Tipo','Tipo Ajustado',u'% Confiança']
    
    print(colored('\nZone-Wise Production Results - Soya Completed!...\n','green'))
    
    ## --------------------------------------------- Milho 1 -------------------------------------------- ##
    
    print(colored('\nCalculating Zone-wise Production Values for all Model States - Milho 1ª Safra...\n','green'))
    
    milho1_actual=milho1_actual[['Years','Month','UF','Monthly_Value']]
    milho_state_to_zone=pd.read_csv(str(path)+'/Production/Data Directory/Milho_State_to_Zone_Ratio.csv',encoding=encoding)
    milho_state_to_zone=preprocessing_state_to_zone(milho_state_to_zone)
    
    zonewise_milho1_model_actual=calculate_state_to_zone(milho1_actual,milho_state_to_zone,'pred')
    zonewise_milho1_model_actual.columns=['Year','Month','UF','Zonas','Production']
    
    print(colored('\nCalculating Zone-wise Production Values for all Non Model States - Milho 1ª Safra...\n','green'))
    
    milho1_non_model=pd.read_csv(str(path)+'/Production/Data Directory/Milho1_Non_Model.csv',encoding=encoding)
    
    milho1_non_model_actual=retrieving_actual_production(milho1_non_model,year_var)
    milho1_non_model_actual=milho1_non_model_actual[['Years','Month','UF','Monthly_Value']]
    
    zonewise_milho1_nonmodel_actual=calculate_state_to_zone(milho1_non_model_actual,milho_state_to_zone,'pred')
    zonewise_milho1_nonmodel_actual.columns=['Year','Month','UF','Zonas','Production']
    
    ## Combining Model and Non-Model States - Soya
    all_states_milho1_zonewise=pd.concat([zonewise_milho1_model_actual,zonewise_milho1_nonmodel_actual],axis=0)
    all_states_milho1_zonewise=all_states_milho1_zonewise.reset_index(drop=True)
    all_states_milho1_zonewise=add_confidence(all_states_milho1_zonewise)
    all_states_milho1_zonewise['Tipo']='Real'
    all_states_milho1_zonewise['Tipo Ajustado']='Real / Calculada'
    all_states_milho1_zonewise.to_csv(str(path)+'/Production/Data Directory/Milho1_Actual_Production.csv',index=False,encoding=encoding)
    
    ## Getting Confidence Results for Predicted States
    p=str(path)+'/Production/Data Directory/Jan2019/Milho1_-_Production_Model_Re-Run_Results/Milho1 Confidence New Results/'
    milho1_confidence_result=retrieving_confidence(p,'prod')
    milho1_confidence_result.rename(columns={'Month_Num': 'Month','Ano': 'Year'}, inplace=True)
    
    predicted_milho1_prod=retrieving_predicted_production(milho1_predict,year_var)
    predicted_milho1_prod=pd.merge(predicted_milho1_prod[['Year','Month','UF']],milho1_confidence_result[['Year','Month','UF','Point.Forecast']],on=['Year','Month','UF'],how='outer',right_index=False,sort=True,indicator=True).query("_merge != 'right_only'").drop('_merge',axis=1)    
    predicted_milho1_zonewise=calculate_state_to_zone(predicted_milho1_prod,milho_state_to_zone,'pred')
      
    del milho1_confidence_result['Point.Forecast']
    milho1_confidence_result=calculate_state_to_zone(milho1_confidence_result,milho_state_to_zone,'confidence')
    milho1_confidence_result.to_csv(str(path)+'/Production/Data Directory/Milho1_Confidence_Production.csv',encoding=encoding,index=False)
    
    ## Merging the Confidence Interval Results with Export Predictions
    milho1_zonewise_predicted=pd.merge(predicted_milho1_zonewise,milho1_confidence_result,on=['Year','Month','UF','Zonas'],how='outer',right_index=False,sort=True,indicator=True).query("_merge != 'right_only'").drop('_merge',axis=1)
    milho1_zonewise_predicted.fillna(0)
    milho1_zonewise_predicted['Tipo']='Prevista'
    milho1_zonewise_predicted['Tipo Ajustado']='Prevista'
    milho1_zonewise_predicted.rename(columns={'Zones_Export': 'Production'}, inplace=True)
    milho1_zonewise_predicted.to_csv(str(path)+'/Production/Data Directory/Milho1_Predicted_Production.csv',encoding=encoding,index=False)
    
    ## Getting all Fitted Values - Zones
    milho1_predict.rename(columns={'Year': 'Years'}, inplace=True)
    fitted_production_milho1=retrieving_actual_production(milho1_predict,year_var)  
    fitted_zonewise_production_milho1=calculate_state_to_zone(fitted_production_milho1,milho_state_to_zone,'pred')
    fitted_zonewise_production_milho1=add_confidence(fitted_zonewise_production_milho1)
    fitted_zonewise_production_milho1.rename(columns={'Zones_Export': 'Production'}, inplace=True)
    fitted_zonewise_production_milho1['Tipo']='Calculada'
    fitted_zonewise_production_milho1['Tipo Ajustado']='Real / Calculada'
    fitted_zonewise_production_milho1.to_csv(str(path)+'/Production/Data Directory/Milho1_ModelFitted_Production.csv',encoding=encoding,index=False)
    
    ## Getting Non Model Predicted Values - Zones
    milho1_non_model.rename(columns={'Years': 'Year'}, inplace=True)
    milho1_non_model_pred=retrieving_predicted_production(milho1_non_model,year_var)
    milho1_non_model_pred=milho1_non_model_pred[['Year','Month','UF','Monthly_Value']]
    milho1_zonewise_non_model_pred=calculate_state_to_zone(milho1_non_model_pred,milho_state_to_zone,'pred')
    milho1_zonewise_non_model_pred=add_confidence2(milho1_zonewise_non_model_pred)
    milho1_zonewise_non_model_pred.rename(columns={'Zones_Export': 'Production'}, inplace=True)
    milho1_zonewise_non_model_pred['Tipo']='Prevista'
    milho1_zonewise_non_model_pred['Tipo Ajustado']='Prevista'
    milho1_zonewise_non_model_pred.to_csv(str(path)+'/Production/Data Directory/Milho1_NonModel_Predicted_Production.csv',encoding=encoding,index=False)
    
    ## Getting Non Model Fitted Values - Zones 
    zonewise_milho1_nonmodel_fitted=zonewise_milho1_nonmodel_actual
    zonewise_milho1_nonmodel_fitted['Production']=0
    zonewise_milho1_nonmodel_fitted=add_confidence(zonewise_milho1_nonmodel_fitted)
    zonewise_milho1_nonmodel_fitted['Tipo']='Calculada'
    zonewise_milho1_nonmodel_fitted['Tipo Ajustado']='Real / Calculada'
    zonewise_milho1_nonmodel_fitted.to_csv(str(path)+'/Production/Data Directory/Milho1_NonModel_Fitted_Production.csv',encoding=encoding,index=False)
    
    print(colored('\nCombining Zone-wise Model and Non-Model States Production Values - Milho 1ª Safra...\n','green'))
    
    milho1_prod=merge_all_states(all_states_milho1_zonewise,fitted_zonewise_production_milho1,milho1_zonewise_predicted,milho1_zonewise_non_model_pred,zonewise_milho1_nonmodel_fitted)  
    
    milho1_prod.insert(0,'Produto','Milho 1ª Safra')
    milho1_prod=milho1_prod[['Produto','Month','Year','UF','Zonas','Low','Production','High','Tipo','Tipo Ajustado','Conf']]
    milho1_prod.columns=['Produto',u'Mês','Ano','UF',u'Microrregião',u'Produção Mín (Kt)',u'Produção Med (Kt)',u'Produção Max (Kt)','Tipo','Tipo Ajustado',u'% Confiança']
    
    print(colored('\nZone-Wise Production Results - Milho 1ª Safra Completed!...\n','green'))
    
    ## --------------------------------------------- Milho 2 ------------------------------------------------ ##
    
    print(colored('\nCalculating Zone-wise Production Values for all Model States - Milho 2ª Safra...\n','green'))
    
    milho2_actual=milho2_actual[['Years','Month','UF','Monthly_Value']]
    milho_state_to_zone=pd.read_csv(str(path)+'/Production/Data Directory/Milho_State_to_Zone_Ratio.csv',encoding=encoding)
    milho_state_to_zone=preprocessing_state_to_zone(milho_state_to_zone)
    
    zonewise_milho2_model_actual=calculate_state_to_zone(milho2_actual,milho_state_to_zone,'pred')
    zonewise_milho2_model_actual.columns=['Year','Month','UF','Zonas','Production']
    
    print(colored('\nCalculating Zone-wise Production Values for all Non Model States - Milho 2ª Safra...\n','green'))
    
    milho2_non_model=pd.read_csv(str(path)+'/Production/Data Directory/Milho2_Non_Model.csv',encoding=encoding)
    
    milho2_non_model_actual=retrieving_actual_production(milho2_non_model,year_var)
    milho2_non_model_actual=milho2_non_model_actual[['Years','Month','UF','Monthly_Value']]
    
    zonewise_milho2_nonmodel_actual=calculate_state_to_zone(milho2_non_model_actual,milho_state_to_zone,'pred')
    zonewise_milho2_nonmodel_actual.columns=['Year','Month','UF','Zonas','Production']
    
    ## Combining Model and Non-Model States - Soya
    all_states_milho2_zonewise=pd.concat([zonewise_milho2_model_actual,zonewise_milho2_nonmodel_actual],axis=0)
    all_states_milho2_zonewise=all_states_milho2_zonewise.reset_index(drop=True)
    all_states_milho2_zonewise=add_confidence(all_states_milho2_zonewise)
    all_states_milho2_zonewise['Tipo']='Real'
    all_states_milho2_zonewise['Tipo Ajustado']='Real / Calculada'
    all_states_milho2_zonewise.to_csv(str(path)+'/Production/Data Directory/Milho2_Actual_Production.csv',index=False,encoding=encoding)
    
    ## Getting Confidence Results for Predicted States
    p=str(path)+'/Production/Data Directory/Jan2019/Milho2_Production_Model_Re-Run_Results/Milho2 Confidence New Results/'
    milho2_confidence_result=retrieving_confidence(p,'prod')
    milho2_confidence_result.rename(columns={'Month_Num': 'Month','Ano': 'Year'}, inplace=True)
    
    predicted_milho2_prod=retrieving_predicted_production(milho2_predict,year_var)
    predicted_milho2_prod=pd.merge(predicted_milho2_prod[['Year','Month','UF']],milho2_confidence_result[['Year','Month','UF','Point.Forecast']],on=['Year','Month','UF'],how='outer',right_index=False,sort=True,indicator=True).query("_merge != 'right_only'").drop('_merge',axis=1)    
    predicted_milho2_zonewise=calculate_state_to_zone(predicted_milho2_prod,milho_state_to_zone,'pred')
      
    del milho2_confidence_result['Point.Forecast']
    milho2_confidence_result=calculate_state_to_zone(milho2_confidence_result,milho_state_to_zone,'confidence')
    milho2_confidence_result.to_csv(str(path)+'/Production/Data Directory/Milho2_Confidence_Production.csv',encoding=encoding,index=False)
    
    ## Merging the Confidence Interval Results with Export Predictions
    milho2_zonewise_predicted=pd.merge(predicted_milho2_zonewise,milho2_confidence_result,on=['Year','Month','UF','Zonas'],how='outer',right_index=False,sort=True,indicator=True).query("_merge != 'right_only'").drop('_merge',axis=1)
    milho2_zonewise_predicted.fillna(0)
    milho2_zonewise_predicted['Tipo']='Prevista'
    milho2_zonewise_predicted['Tipo Ajustado']='Prevista'
    milho2_zonewise_predicted.rename(columns={'Zones_Export': 'Production'}, inplace=True)
    milho2_zonewise_predicted.to_csv(str(path)+'/Production/Data Directory/Milho2_Predicted_Production.csv',encoding=encoding,index=False)
    
    ## Getting all Fitted Values - Zones
    milho2_predict.rename(columns={'Year': 'Years'}, inplace=True)
    fitted_production_milho2=retrieving_actual_production(milho2_predict,year_var)  
    fitted_zonewise_production_milho2=calculate_state_to_zone(fitted_production_milho2,milho_state_to_zone,'pred')
    fitted_zonewise_production_milho2=add_confidence(fitted_zonewise_production_milho2)
    fitted_zonewise_production_milho2.rename(columns={'Zones_Export': 'Production'}, inplace=True)
    fitted_zonewise_production_milho2['Tipo']='Calculada'
    fitted_zonewise_production_milho2['Tipo Ajustado']='Real / Calculada'
    fitted_zonewise_production_milho2.to_csv(str(path)+'/Production/Data Directory/Milho2_ModelFitted_Production.csv',encoding=encoding,index=False)
    
    ## Getting Non Model Predicted Values - Zones
    milho2_non_model.rename(columns={'Years': 'Year'}, inplace=True)
    milho2_non_model_pred=retrieving_predicted_production(milho2_non_model,year_var)
    milho2_non_model_pred=milho2_non_model_pred[['Year','Month','UF','Monthly_Value']]
    milho2_zonewise_non_model_pred=calculate_state_to_zone(milho2_non_model_pred,milho_state_to_zone,'pred')
    milho2_zonewise_non_model_pred=add_confidence2(milho2_zonewise_non_model_pred)
    milho2_zonewise_non_model_pred.rename(columns={'Zones_Export': 'Production'}, inplace=True)
    milho2_zonewise_non_model_pred['Tipo']='Prevista'
    milho2_zonewise_non_model_pred['Tipo Ajustado']='Prevista'
    milho2_zonewise_non_model_pred.to_csv(str(path)+'/Production/Data Directory/Milho2_NonModel_Predicted_Production.csv',encoding=encoding,index=False)
    
    ## Getting Non Model Fitted Values - Zones 
    zonewise_milho2_nonmodel_fitted=zonewise_milho2_nonmodel_actual
    zonewise_milho2_nonmodel_fitted['Production']=0
    zonewise_milho2_nonmodel_fitted=add_confidence(zonewise_milho2_nonmodel_fitted)
    zonewise_milho2_nonmodel_fitted['Tipo']='Calculada'
    zonewise_milho2_nonmodel_fitted['Tipo Ajustado']='Real / Calculada'
    zonewise_milho2_nonmodel_fitted.to_csv(str(path)+'/Production/Data Directory/Milho2_NonModel_Fitted_Production.csv',encoding=encoding,index=False)
    
    print(colored('\nCombining Zone-wise Model and Non-Model States Production Values - Milho 2ª Safra...\n','green'))
    
    milho2_prod=merge_all_states(all_states_milho2_zonewise,fitted_zonewise_production_milho2,milho2_zonewise_predicted,milho2_zonewise_non_model_pred,zonewise_milho2_nonmodel_fitted)  
    
    milho2_prod.insert(0,'Produto','Milho 2ª Safra')
    milho2_prod=milho2_prod[['Produto','Month','Year','UF','Zonas','Low','Production','High','Tipo','Tipo Ajustado','Conf']]
    milho2_prod.columns=['Produto',u'Mês','Ano','UF',u'Microrregião',u'Produção Mín (Kt)',u'Produção Med (Kt)',u'Produção Max (Kt)','Tipo','Tipo Ajustado',u'% Confiança']
    
    print(colored('\nZone-Wise Production Results - Milho 2ª Safra Completed!...\n','green'))
    print(colored('\nStarted Combining Zone-Wise Production Results - All Crops...\n','green'))
    
    ## Combining all crops together
    all_crops=pd.concat([soya_prod.reset_index(drop=True),milho1_prod.reset_index(drop=True)],axis=0)
    all_crops=pd.concat([all_crops.reset_index(drop=True),milho2_prod.reset_index(drop=True)],axis=0)
    all_crops=all_crops.reset_index(drop=True)
    
    state1=get_state_file(str(path)+"/Production/Input Files/") 
    
    all_crops=pd.merge(all_crops,state1,left_on=['UF'],right_on=['State_Code'],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    del all_crops['State_Code']
    
    all_crops=all_crops[['Produto',u'Mês','Ano','State_Name','UF',u'Microrregião',u'Produção Mín (Kt)',u'Produção Med (Kt)',u'Produção Max (Kt)','Tipo','Tipo Ajustado',u'% Confiança']]
    all_crops['State_Name']=all_crops['State_Name'].str.upper()
    all_crops.rename(columns={'State_Name': 'Estado'}, inplace=True)
    
    all_crops.iloc[:,11] = all_crops.iloc[:,11].apply(pd.to_numeric, errors = 'coerce')
    
    for field in all_crops.iloc[:,6:9]:        
        all_crops[field] = all_crops[field].map(try_cutoff)
    
    print(colored('\nCombining Zone-Wise Production Results - All Crops Completed!...\n','green'))
    
    ## Saving in Indian Settings
    date=get_date()
    all_crops.to_csv(str(path)+'/Production/Output Files/'+date+u'_Produção.csv',index=False,encoding=encoding)
    all_crops.to_csv(str(path)+'/Export/Input Files/'+date+u'_Produção.csv',index=False,encoding=encoding)
    all_crops.to_csv(str(path)+'/Excess Exportable/Input Files/'+date+u'_Produção.csv',index=False,encoding=encoding)
    all_crops.to_csv(str(path)+'/Production/Output Files/Produção.csv',index=False,encoding=encoding)
    
    print(colored('\nZone-Wise Production Prediction Results Saving in Database !!!...\n','green'))
    
    ## Saving in Brazilian Settings
    all_crops[[u'Produção Mín (Kt)',u'Produção Med (Kt)',u'Produção Max (Kt)']] = all_crops[[u'Produção Mín (Kt)',u'Produção Med (Kt)',u'Produção Max (Kt)']].astype(str)
    for field in all_crops.iloc[:,6:9]:
        all_crops[field] = all_crops[field].map(Replace)
    
    all_crops.to_excel(str(path)+'/Production/Output Files/'+date+u'_Produção.xlsx',index=False,encoding=encoding)
    all_crops.to_excel(str(path)+'/Excess Exportable/Input Files/'+date+u'_Produção.xlsx',index=False,encoding=encoding)
    
    print(colored('\nProduction Forecasting & Output Results Process Completed !!!...\n','green'))
    
if __name__ == '__main__':
    main()
