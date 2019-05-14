# -*- coding: utf-8 -*-
"""
Created on Thu Feb 21 10:46:15 2019

@author: agupta466
"""
import pandas as pd
import numpy as np
import datetime
import sys
from pathlib import Path
import os
import argparse
import colorama
from colorama import init
import termcolor
from termcolor import colored 

## Reading the Command Line Arguments for Year Month R_Home, R_User, R_Library_Path
parser = argparse.ArgumentParser(description='Pass Year and Month as Arguments')

parser.add_argument('year_max', action="store", type=int)
parser.add_argument('month_max', action="store", type=int)
parser.add_argument('R_HOME', action="store", type=str)
parser.add_argument('R_USER', action="store", type=str)
parser.add_argument('R_LIB', action="store", type=str)

results = parser.parse_args()  

# setting temporary PATH variables
os.environ['R_HOME'] = results.R_HOME 
os.environ['R_USER'] = results.R_USER

# =============================================================================
# #setting temporary PATH variables
# os.environ['R_HOME'] = 'C:\Program Files\R\R-3.5.1' 
# os.environ['R_USER'] = 'agupta466' 
# #os.environ['R_LIBS_USER'] = '\envs\rstudio\Library'
# =============================================================================

from rpy2.robjects import Environment, reval
# importing rpy2
import rpy2.robjects as robjects
r=robjects.r

from rpy2.robjects import pandas2ri
pandas2ri.activate()

from rpy2.robjects.packages import importr
import rpy2.robjects as ro

ro.r('''.libPaths('{}')'''.format(results.R_LIB))
#ro.r('''.libPaths('C:/Users/agupta466/Documents/R/win-library/3.5')''')

forecast = importr('forecast')
imputeTS = importr('imputeTS')

here = Path(__file__).resolve()
path = here.parents[2]

sys.path.append(str(path)+"/Export/Python Codes")

globalenv=Environment()

encoding='ISO-8859-15'
prefix='20'

def get_state_file(path):
    state_code = pd.read_excel(str(path)+"Rotas_Regiao.xlsx",encoding=encoding)
    state = state_code[['UF','UF.1']]
    state1 = state.drop_duplicates(subset=['UF','UF.1'])
    state1.columns=['State_Code','State_Name']
    return(state1)
    
def get_state_code(state1,state_name):
    for index,row in state1.iloc[:,:].iterrows():
        if state_name==row[1]:
            return(row[0])
        if state_name=='Total':
            return('BS')

def get_harvest_plantation_file(path,state1):
    harvest_plantation=pd.read_excel(str(path)+"Colheita_Plantio_UF.xls",encoding=encoding)
    for index,row in harvest_plantation.iloc[:,:].iterrows():
        harvest_plantation.loc[index,'UF'] = get_state_code(state1,row[0])
    return(harvest_plantation)

def replace_dash(year,value):
    year[value]=year[value].replace(['-'], 0)
    return year

def data_subsetting(df,estado_info,year_list_monthly):
    df.columns = df.iloc[0]
    df=df.drop(df.index[[0,1]])
    
    ## Years & Months Pre-processing (Plantation yearly file)
    years=year_preprocessing(df)
    year_list=years.columns
    df.columns = year_list
    
    ## Selecting years in Monthly Harvest Plantation file
    df=df.loc[:, df.columns.isin(year_list_monthly)]
    
    ## Subsetting Soja, Milho1 and Milho2
    df=pd.concat([estado_info.reset_index(drop=True),df.reset_index(drop=True)],axis=1)
    df=df[(df['Produto']=='Soja')|(df['Produto']==u'Milho 1ª Safra')|(df['Produto']==u'Milho 2ª Safra')]
    df=df[(df['Estado']!='Total')]
    for index,row in df.iloc[::].iterrows():
        if(row[0]==u'Milho 1ª Safra'):
            df.loc[index,'Produto']='Milho 1 Safra'
        if(row[0]==u'Milho 2ª Safra'):
            df.loc[index,'Produto']='Milho 2 Safra'
        else:
            continue
    df=df.reset_index(drop=True)
    
    return(df)

def data_preprocessing_yearly(harvest_plantation_yearly,year_list_monthly):
    harvest_plantation_yearly.index.name=['Produto','Estado']
    harvest_plantation_yearly=harvest_plantation_yearly.reset_index()
    harvest_plantation_yearly=harvest_plantation_yearly.rename(columns={harvest_plantation_yearly.columns[0]: "Produto",harvest_plantation_yearly.columns[1]: "Estado"})
    estado_info=harvest_plantation_yearly[['Produto','Estado']]
    estado_info=estado_info.iloc[2:,:]
    df_plant = harvest_plantation_yearly.filter(regex=u'Área Plantada')
    df_plant = data_subsetting(df_plant,estado_info,year_list_monthly)
    df_plant['Type'] = 'Area Plantada'
    
    df_prod = harvest_plantation_yearly.filter(regex=u'Produção')
    df_prod = data_subsetting(df_prod,estado_info,year_list_monthly)
    df_prod['Type'] = 'Producao'
    
    df=pd.concat([df_plant.reset_index(drop=True),df_prod.reset_index(drop=True)],axis=0)
    df=df.reset_index(drop=True)
    unpivoted_df=pd.melt(df, id_vars=['Produto','Estado','Type'], var_name='Years', value_name='Value')
    unpivoted_df=unpivoted_df.reset_index(drop=True)
    unpivoted_df["Years"] = unpivoted_df["Years"].astype(int)
    unpivoted_df["Value"] = unpivoted_df["Value"].astype(float)
    print(unpivoted_df)
    
    unpivoted_df['Year_Start']=unpivoted_df['Years']-1        
    print(unpivoted_df)
    
    unpivoted_df['Years']=unpivoted_df['Year_Start'].astype(str)+'-'+unpivoted_df['Years'].astype(str)
    print(unpivoted_df)
    
    del unpivoted_df['Year_Start']
    print(unpivoted_df)
    
    return(unpivoted_df)

def year_preprocessing(years):
    #years=harvest_plantation.iloc[:,5:]
    years.columns=years.columns.str.split('/').str[1]
    years.columns = prefix + years.columns.astype(str)
    year=years.columns
    valid_years=[i for i in year if len(i)==4]
    years = years[years.columns.intersection(valid_years)]
    for column in years:
        replace_dash(years,column)
    years=years.reset_index(level=0, drop=True)
    return(years)

def month_preprocessing(new_harvest_plantation):
    new_harvest_plantation["Month"]= new_harvest_plantation["Quinzena"].str.split("-").str[1]
    new_harvest_plantation.insert(1,'Month_Name',new_harvest_plantation['Month'].str[:4])
    new_harvest_plantation['Month_Name']= new_harvest_plantation['Month_Name'].str.strip()
    new_harvest_plantation=new_harvest_plantation.drop(['Month'], axis=1)
    new_harvest_plantation=new_harvest_plantation.drop(['Quinzena'],axis=1)
    return(new_harvest_plantation)

def get_month_num(month_name):
    if month_name=='abr':
        return(4)
    if month_name=='ago':
        return(8)
    if month_name=='jul':
        return(7)
    if month_name=='jun':
        return(6)
    if month_name=='mai':
        return(5)
    if month_name=='set':
        return(9)
    if month_name=='mar':
        return(3)
    if month_name=='fev':
        return(2)
    if month_name=='jan':
        return(1)
    if month_name=='out':
        return(10)
    if month_name=='nov':
        return(11)
    if month_name=='dez':
        return(12)

def get_maxima(state_subset,years):
    state_subset=state_subset.drop(['Cultura'], axis=1)
    state_subset=state_subset.drop(['Fase Safra'], axis=1)
    for index,row in state_subset.iloc[:,:].iterrows():
        state_subset.loc[index,'Month']=get_month_num(state_subset.loc[index,'Month_Name'])
    state_subset=state_subset.drop(['Month_Name'], axis=1)
    unpivot_years=pd.melt(state_subset, id_vars=['UF','Month'], var_name='Years', value_name='Percentage')   
    df1=unpivot_years.sort_values(['Percentage'],ascending=False).groupby(['UF','Years','Month']).max()
    print(df1)
    
# =============================================================================
#     df1=df1.reset_index()
#     
#     state_code=list(set(df1['UF']))
#     year_list=list(set(df1['Years']))
#     
# =============================================================================
# =============================================================================
#     df=pd.DataFrame(columns=df1.columns)
#     for i in range(len(state_code)):
#         for j in range(len(year_list)):
#             max_percentage_year=df1.sort_values(['Percentage'],ascending=False).groupby(['UF','Years']).max()
#             if(max_percentage_year['Per']<100):
#                 df_new=pd.DataFrame(0,index=np.arange(len(1)),columns=df1.columns)
#                 if()
# =============================================================================
                
    
    return(df1)
    
def data_preprocessing_maxima(maxima):
    maxima.insert(0,'UF',maxima.index.get_level_values('UF'))
    maxima.insert(1,'Years',maxima.index.get_level_values('Years'))
    maxima.insert(2,'Month',maxima.index.get_level_values('Month'))
    maxima["Month"] = maxima["Month"].astype(int)
    maxima=maxima.reset_index(level=[0,1,2], drop=True)
    return(maxima)
    
def year_left_calculate(subset,year_list):
    total_months=list(range(1,13))
    #state_code=subset.drop_duplicates(subset=['UF'])
    state_code=list(set(subset['UF']))
    df=pd.DataFrame(columns=subset.columns)
    for i in range(len(year_list)):
        month=list(subset[subset['Years']==year_list[i]]['Month'])
        month_left=set(total_months).difference(month)
        code=state_code*len(month_left)
        df1=pd.DataFrame(0,index=np.arange(len(month_left)),columns=subset.columns)
        df1['Month']=month_left
        df1['UF']=code
        df1['Years']=year_list[i]
        df=pd.concat([df.reset_index(drop=True), df1.reset_index(drop=True)], axis=0)
    df=df.reset_index(drop=True)
    return(df)

def calculate_maxima_other(maxima,maxima_other):
    maxima_full=maxima.append(maxima_other)
    maxima_full=maxima_full.reset_index(level=0,drop=True)
    maxima_full=maxima_full.sort_values(['Years','Month']).reset_index(level=0,drop=True)
    return(maxima_full)

def calculate_maxima(crop,maxima_data,year_list):
    ## Statewise Calculation of Percentage for all months in year
    df=pd.DataFrame(columns=maxima_data.columns)
    
    if((crop=='Soya') or (crop=='Milho1')):
        ## BA - Bahia
        BA_maxima=maxima_data[maxima_data['UF']=='BA']
        BA_other=year_left_calculate(BA_maxima,year_list)
        BA_full=calculate_maxima_other(BA_maxima,BA_other)
        df=pd.concat([df.reset_index(drop=True), BA_full.reset_index(drop=True)], axis=0)
    
    if((crop=='Soya') or (crop=='Milho1') or (crop=='Milho2')):
        ## GO - Goias
        GO_maxima=maxima_data[maxima_data['UF']=='GO']
        GO_other=year_left_calculate(GO_maxima,year_list)
        GO_full=calculate_maxima_other(GO_maxima,GO_other)
        df=pd.concat([df.reset_index(drop=True), GO_full.reset_index(drop=True)], axis=0)
    
    if((crop=='Soya') or (crop=='Milho1')):
        ## MA - Maranhao
        MA_maxima=maxima_data[maxima_data['UF']=='MA']
        MA_other=year_left_calculate(MA_maxima,year_list)
        MA_full=calculate_maxima_other(MA_maxima,MA_other)
        df=pd.concat([df.reset_index(drop=True), MA_full.reset_index(drop=True)], axis=0)
    
    if((crop=='Soya') or (crop=='Milho1')):
        ## MG - Minas Gerias
        MG_maxima=maxima_data[maxima_data['UF']=='MG']
        MG_other=year_left_calculate(MG_maxima,year_list)
        MG_full=calculate_maxima_other(MG_maxima,MG_other)
        df=pd.concat([df.reset_index(drop=True), MG_full.reset_index(drop=True)], axis=0)
    
    if((crop=='Soya') or (crop=='Milho2')):
        ## MS - Mato Grosso Du Sul
        MS_maxima=maxima_data[maxima_data['UF']=='MS']
        MS_other=year_left_calculate(MS_maxima,year_list)
        MS_full=calculate_maxima_other(MS_maxima,MS_other)
        df=pd.concat([df.reset_index(drop=True), MS_full.reset_index(drop=True)], axis=0)
    
    if((crop=='Soya') or (crop=='Milho2')):
        ## MT - Mato Grosso 
        MT_maxima=maxima_data[maxima_data['UF']=='MT']
        MT_other=year_left_calculate(MT_maxima,year_list)
        MT_full=calculate_maxima_other(MT_maxima,MT_other)
        df=pd.concat([df.reset_index(drop=True), MT_full.reset_index(drop=True)], axis=0)
    
    if((crop=='Soya') or (crop=='Milho1') or (crop=='Milho2')):
        ## PR - Parana
        PR_maxima=maxima_data[maxima_data['UF']=='PR']
        PR_other=year_left_calculate(PR_maxima,year_list)
        PR_full=calculate_maxima_other(PR_maxima,PR_other)
        df=pd.concat([df.reset_index(drop=True), PR_full.reset_index(drop=True)], axis=0)
    
    if((crop=='Soya') or (crop=='Milho1')):
        ## RS - Rio Grande Do Sul
        RS_maxima=maxima_data[maxima_data['UF']=='RS']
        RS_other=year_left_calculate(RS_maxima,year_list)
        RS_full=calculate_maxima_other(RS_maxima,RS_other)
        df=pd.concat([df.reset_index(drop=True), RS_full.reset_index(drop=True)], axis=0)
    
    if((crop=='Soya')):
        ## PI - Piaui
        PI_maxima=maxima_data[maxima_data['UF']=='PI']
        PI_other=year_left_calculate(PI_maxima,year_list)
        PI_full=calculate_maxima_other(PI_maxima,PI_other)
        df=pd.concat([df.reset_index(drop=True), PI_full.reset_index(drop=True)], axis=0)
    
    if((crop=='Soya')):
        ## RO - Rondonia
        RO_maxima=maxima_data[maxima_data['UF']=='RO']
        RO_other=year_left_calculate(RO_maxima,year_list)
        RO_full=calculate_maxima_other(RO_maxima,RO_other)
        df=pd.concat([df.reset_index(drop=True), RO_full.reset_index(drop=True)], axis=0)
    
    if((crop=='Soya') or (crop=='Milho1')):
        ## SC - Santa Catarina
        SC_maxima=maxima_data[maxima_data['UF']=='SC']
        SC_other=year_left_calculate(SC_maxima,year_list)
        SC_full=calculate_maxima_other(SC_maxima,SC_other)
        df=pd.concat([df.reset_index(drop=True), SC_full.reset_index(drop=True)], axis=0)
    
    if((crop=='Soya') or (crop=='Milho1')):
        ## SP - Sao Paolo
        SP_maxima=maxima_data[maxima_data['UF']=='SP']
        SP_other=year_left_calculate(SP_maxima,year_list)
        SP_full=calculate_maxima_other(SP_maxima,SP_other)
        df=pd.concat([df.reset_index(drop=True), SP_full.reset_index(drop=True)], axis=0)
    
    if((crop=='Soya')):
        ## TO - Tocantino
        TO_maxima=maxima_data[maxima_data['UF']=='TO']
        TO_other=year_left_calculate(TO_maxima,year_list)
        TO_full=calculate_maxima_other(TO_maxima,TO_other)
        df=pd.concat([df.reset_index(drop=True), TO_full.reset_index(drop=True)], axis=0)
    
    df=df.reset_index(drop=True)
    
    return(df)
    
def combine_plantation_harvest(new_harvest_plantation,year_list_monthly):
    
    # Subsetting for Harvest (%)
    soya_harvest=new_harvest_plantation[(new_harvest_plantation[u'Cultura']=='Soja') & (new_harvest_plantation['Fase Safra']=='Colheita')]
    milho1_harvest=new_harvest_plantation[(new_harvest_plantation[u'Cultura']==u'Milho 1ª Safra') & (new_harvest_plantation['Fase Safra']=='Colheita')]
    milho2_harvest=new_harvest_plantation[(new_harvest_plantation[u'Cultura']==u'Milho 2ª Safra') & (new_harvest_plantation['Fase Safra']=='Colheita')]
    
    ## Subsetting for Plantation (%)
    soya_plantation=new_harvest_plantation[(new_harvest_plantation[u'Cultura']=='Soja') & (new_harvest_plantation['Fase Safra']=='Plantio')]
    milho1_plantation=new_harvest_plantation[(new_harvest_plantation[u'Cultura']==u'Milho 1ª Safra') & (new_harvest_plantation['Fase Safra']=='Plantio')]
    milho2_plantation=new_harvest_plantation[(new_harvest_plantation[u'Cultura']==u'Milho 2ª Safra') & (new_harvest_plantation['Fase Safra']=='Plantio')]
    
    ## Calculating Production Maxima for all crops
    soya_prod_maxima=get_maxima(soya_harvest,year_list_monthly)
    milho1_prod_maxima=get_maxima(milho1_harvest,year_list_monthly)
    milho2_prod_maxima=get_maxima(milho2_harvest,year_list_monthly)
    
    ## Calculating Plantation Maxima for all Crops
    soya_plant_maxima=get_maxima(soya_plantation,year_list_monthly)
    milho1_plant_maxima=get_maxima(milho1_plantation,year_list_monthly)
    milho2_plant_maxima=get_maxima(milho2_plantation,year_list_monthly)
    
    ## Combining all states and all years into one dataframe (for Maxima Ratios)
    ## Soya Production
    soya_prod_maxima=data_preprocessing_maxima(soya_prod_maxima)
    soya_prod_all=calculate_maxima("Soya",soya_prod_maxima,year_list_monthly)
    soya_prod_all.insert(0,'Produto','Soja')
    
    df=pd.DataFrame(columns=soya_prod_all.columns)
    
    df=pd.concat([df.reset_index(drop=True), soya_prod_all.reset_index(drop=True)], axis=0)
    
    ## Milho1 Production
    milho1_prod_maxima=data_preprocessing_maxima(milho1_prod_maxima)
    milho1_prod_all=calculate_maxima("Milho1",milho1_prod_maxima,year_list_monthly)
    milho1_prod_all.insert(0,'Produto','Milho 1 Safra')
    
    df=pd.concat([df.reset_index(drop=True), milho1_prod_all.reset_index(drop=True)], axis=0)
    
    ## Milho2 Production
    milho2_prod_maxima=data_preprocessing_maxima(milho2_prod_maxima)
    milho2_prod_all=calculate_maxima("Milho2",milho2_prod_maxima,year_list_monthly)
    milho2_prod_all.insert(0,'Produto','Milho 2 Safra')
    
    df=pd.concat([df.reset_index(drop=True), milho2_prod_all.reset_index(drop=True)], axis=0)
    df=df.reset_index(drop=True)
    
    df.insert(2,'Type','Producao')

    ## Combining all states and all years into one dataframe (for Maxima Ratios)
    ## Soya Plantation
    soya_plant_maxima=data_preprocessing_maxima(soya_plant_maxima)
    soya_plant_all=calculate_maxima("Soya",soya_plant_maxima,year_list_monthly)
    soya_plant_all.insert(0,'Produto','Soja')
    
    df1=pd.DataFrame(columns=soya_plant_all.columns)
    
    df1=pd.concat([df1.reset_index(drop=True), soya_plant_all.reset_index(drop=True)], axis=0)
    
    ## Milho1 Production
    milho1_plant_maxima=data_preprocessing_maxima(milho1_plant_maxima)
    milho1_plant_all=calculate_maxima("Milho1",milho1_plant_maxima,year_list_monthly)
    milho1_plant_all.insert(0,'Produto','Milho 1 Safra')
    
    df1=pd.concat([df1.reset_index(drop=True), milho1_plant_all.reset_index(drop=True)], axis=0)
    
    ## Milho2 Production
    milho2_plant_maxima=data_preprocessing_maxima(milho2_plant_maxima)
    milho2_plant_all=calculate_maxima("Milho2",milho2_plant_maxima,year_list_monthly)
    milho2_plant_all.insert(0,'Produto','Milho 2 Safra')
    
    df1=pd.concat([df1.reset_index(drop=True), milho2_plant_all.reset_index(drop=True)], axis=0)
    df1=df1.reset_index(drop=True)
    
    df1.insert(2,'Type','Area Plantada')
    
    df=pd.concat([df1.reset_index(drop=True), df.reset_index(drop=True)], axis=0)
    df=df.reset_index(drop=True)
    df['Years']=df['Years'].astype(int)
    df['Month']=df['Month'].astype(int)
    df['Percentage']=df['Percentage'].astype(float)
    
    df['Years']=df['Years'].astype(int)
    
    milho2_cycle_start=df[(df['Month']<=11)&(df['Produto']=='Milho 2 Safra')]
    milho2_cycle_start['Year_Start'] = milho2_cycle_start['Years']-1
    milho2_cycle_start['Year_End'] = milho2_cycle_start['Years']
    
    milho2_cycle_end=df[(df['Month']>11)&(df['Produto']=='Milho 2 Safra')]
    milho2_cycle_end['Year_Start'] = milho2_cycle_end['Years']
    milho2_cycle_end['Year_End'] = milho2_cycle_end['Years']+1
        
    soya_milho1_cycle_start=df[(df['Month']<=7)&(df['Produto']!='Milho 2 Safra')]
    soya_milho1_cycle_start['Year_Start'] = soya_milho1_cycle_start['Years']-1
    soya_milho1_cycle_start['Year_End'] = soya_milho1_cycle_start['Years']
    
    soya_milho1_cycle_end=df[(df['Month']>7)&(df['Produto']!='Milho 2 Safra')]
    soya_milho1_cycle_end['Year_Start'] = soya_milho1_cycle_end['Years']
    soya_milho1_cycle_end['Year_End'] = soya_milho1_cycle_end['Years']+1
    
    milho2_combined=pd.concat([milho2_cycle_start.reset_index(drop=True),milho2_cycle_end.reset_index(drop=True)],axis=0)
    print(milho2_combined)
    soya_milho1_combined=pd.concat([soya_milho1_cycle_start.reset_index(drop=True),soya_milho1_cycle_end.reset_index(drop=True)],axis=0)
    
    df=pd.concat([soya_milho1_combined.reset_index(drop=True),milho2_combined.reset_index(drop=True)],axis=0)
    df=df.reset_index(drop=True)
    df=df.sort_values(['Produto','Type','UF','Years','Month'])
    
    df=df.reset_index(drop=True)
     
    df['Year_Start']=df['Year_Start'].astype(int)
    df['Year_End']=df['Year_End'].astype(int)
    
    df['Years']=df['Year_Start'].astype(str)+'-'+df['Year_End'].astype(str)
    print(df)
    
    del df['Year_Start']
    del df['Year_End']
    
    #df['Years']=df['Years'].astype(int)
    print(df)
    
    df=df.sort_values(['Produto','Type','UF','Years','Month'])
    df=df.reset_index(drop=True)
    return(df)

def removing_negative_monthly(combined_ratio):
    print(colored('\nConverting Negative Values to Zero Started!!!....\n','green'))
    combined_ratio['Monthly_Value'][combined_ratio['Monthly_Value'] < 0] = 0
# =============================================================================
#     for index,row in combined_ratio.iloc[:,:].iterrows():
#         if (combined_ratio.loc[index,'Monthly_Value']<0):
#             combined_ratio.loc[index,'Monthly_Value'] = 0
#         else:
#             combined_ratio.loc[index,'Monthly_Value'] = combined_ratio.loc[index,'Monthly_Value'] 
# =============================================================================
    print(colored('\nConverting Negative Values to Zero Completed!!!....\n','green'))
    return(combined_ratio)

def cumulative_to_monthly(combined_ratio,year_max):
    combined_ratio['Years']=combined_ratio['Years'].astype(int)
    combined_ratio['Month']=combined_ratio['Month'].astype(int)
    
# =============================================================================
#     for index,row in combined_ratio.iloc[:,:].iterrows():
#         counter=index-1
#         if (counter<0):
#             combined_ratio.loc[index,'Monthly_Value'] = combined_ratio.loc[index,'Cumulative_Prod']
#         if ((counter>=0) & ((combined_ratio.loc[index,'Produto']!=combined_ratio.loc[counter,'Produto'])|(combined_ratio.loc[index,'Type']!=combined_ratio.loc[counter,'Type'])|(combined_ratio.loc[index,'UF']!=combined_ratio.loc[counter,'UF']))):
#             combined_ratio.loc[index,'Monthly_Value'] = combined_ratio.loc[index,'Cumulative_Prod']
#         if ((counter>=0) & (combined_ratio.loc[index,'Produto']==combined_ratio.loc[counter,'Produto']) & (combined_ratio.loc[index,'Type']==combined_ratio.loc[counter,'Type']) & (combined_ratio.loc[index,'UF']==combined_ratio.loc[counter,'UF'])):
#             combined_ratio.loc[index,'Monthly_Value'] = combined_ratio.loc[index,'Cumulative_Prod']-combined_ratio.loc[counter,'Cumulative_Prod']
#         if ((counter>=0) & (combined_ratio.loc[index,'Produto']==combined_ratio.loc[counter,'Produto']) & (combined_ratio.loc[index,'Type']==combined_ratio.loc[counter,'Type']) & (combined_ratio.loc[index,'UF']==combined_ratio.loc[counter,'UF']) & (combined_ratio.loc[index,'Years']!=combined_ratio.loc[counter,'Years']) & (combined_ratio.loc[index,'Yearly_value']==combined_ratio.loc[counter,'Yearly_value']) & (combined_ratio.loc[index,'Percentage']!=combined_ratio.loc[counter,'Percentage'])):
#             combined_ratio.loc[index,'Monthly_Value'] = combined_ratio.loc[index,'Cumulative_Prod']-combined_ratio.loc[counter,'Cumulative_Prod']
#         else:
#             combined_ratio.loc[index,'Monthly_Value'] = combined_ratio.loc[index,'Cumulative_Prod']
# =============================================================================
    combined_ratio['Yearly_value']=combined_ratio['Yearly_value'].fillna(0)
    combined_ratio['Monthly_Value']=combined_ratio.Cumulative_Prod.diff()
    combined_ratio.loc[0,'Monthly_Value']=combined_ratio.loc[0,'Cumulative_Prod']
    
    print(combined_ratio)
    
    combined_ratio['Monthly_Value'][combined_ratio['Monthly_Value'] < 0] = combined_ratio['Yearly_value']+combined_ratio['Monthly_Value']
    print(combined_ratio)
    
# =============================================================================
#     for index,row in combined_ratio.iloc[:,:].iterrows():
#         counter=index-1
#         if (counter<0):
#             combined_ratio.loc[index,'Monthly_Value'] = combined_ratio.loc[index,'Cumulative_Prod']
#             continue
#         if (combined_ratio.loc[index,'Cumulative_Prod']==0):
#             combined_ratio.loc[index,'Monthly_Value'] = 0
#         if (combined_ratio.loc[index,'Cumulative_Prod'] - combined_ratio.loc[counter,'Cumulative_Prod']>=0):
#             print(combined_ratio.loc[index,'Cumulative_Prod'])
#             print()
#             combined_ratio.loc[index,'Monthly_Value'] = combined_ratio.loc[index,'Cumulative_Prod']-combined_ratio.loc[counter,'Cumulative_Prod']
#             print(combined_ratio['Monthly_Value'])
#         if (combined_ratio.loc[index,'Cumulative_Prod'] - combined_ratio.loc[counter,'Cumulative_Prod']<0):
#             combined_ratio.loc[index,'Monthly_Value'] = 0
#         else:
#             combined_ratio.loc[index,'Monthly_Value'] = combined_ratio.loc[index,'Cumulative_Prod']-combined_ratio.loc[counter,'Cumulative_Prod']
# =============================================================================
            
    combined_ratio=removing_negative_monthly(combined_ratio)
    return(combined_ratio) 

def calculate_monthly(combined_yearly,combined_ratio,year_max,month_max):
    print(combined_yearly)
    print(combined_ratio)
    
    combined_yearly=combined_yearly.rename(columns={'Estado':'UF'})
    combined_ratio=pd.merge(combined_ratio,combined_yearly,on=['Produto','UF','Type','Years'],how='outer',right_index=False,sort=True,indicator=True).query("_merge != 'right_only'").drop('_merge',axis=1)
    
# =============================================================================
#     for index,row in combined_ratio.iloc[:,:].iterrows():
#         for j,rowj in combined_yearly.iloc[:,:].iterrows():
#             if ((row[0]==rowj[0]) and (row[1]==rowj[1]) and (row[2]==rowj[2])and (row[3]==rowj[3])):
#                 combined_ratio.loc[index,'Yearly_value'] = combined_yearly.loc[j,'Value']
# =============================================================================
    
    combined_ratio=combined_ratio.rename(columns={'Value':'Yearly_value'})
    combined_ratio=combined_ratio.reset_index(drop=True)
    print(combined_ratio)
    
    print(colored('\nCombining Yearly Value with Percentage File Complete!!!...\n','green')) 
            
    combined_ratio["Cumulative_Prod"]=combined_ratio['Yearly_value']*combined_ratio['Percentage']
    combined_ratio['Cumulative_Prod']=combined_ratio['Cumulative_Prod'].fillna(0)
    print(combined_ratio)
    
    combined_ratio['Monthly_Value']=combined_ratio['Yearly_value']*combined_ratio['Percentage']
    
    milho2_cycle_start=combined_ratio[(combined_ratio['Month']<=11)&(combined_ratio['Produto']=='Milho 2 Safra')]
    milho2_cycle_start['Years'] = milho2_cycle_start['Years'].apply(lambda x: x.split('-')[1])
    
    milho2_cycle_end=combined_ratio[(combined_ratio['Month']>11)&(combined_ratio['Produto']=='Milho 2 Safra')]
    milho2_cycle_end['Years'] = milho2_cycle_end['Years'].apply(lambda x: x.split('-')[0])
        
    soya_milho1_cycle_start=combined_ratio[(combined_ratio['Month']<=7)&(combined_ratio['Produto']!='Milho 2 Safra')]
    soya_milho1_cycle_start['Years'] = soya_milho1_cycle_start['Years'].apply(lambda x: x.split('-')[1])
    
    soya_milho1_cycle_end=combined_ratio[(combined_ratio['Month']>7)&(combined_ratio['Produto']!='Milho 2 Safra')]
    soya_milho1_cycle_end['Years'] = soya_milho1_cycle_end['Years'].apply(lambda x: x.split('-')[0])
    
    milho2_combined=pd.concat([milho2_cycle_start.reset_index(drop=True),milho2_cycle_end.reset_index(drop=True)],axis=0)
    soya_milho1_combined=pd.concat([soya_milho1_cycle_start.reset_index(drop=True),soya_milho1_cycle_end.reset_index(drop=True)],axis=0)
    
    combined_ratio=pd.concat([soya_milho1_combined.reset_index(drop=True),milho2_combined.reset_index(drop=True)],axis=0)
    combined_ratio=combined_ratio.reset_index(drop=True)
    combined_ratio=combined_ratio.sort_values(['Produto','Type','UF','Years','Month'])
    
    print(combined_ratio)
    
    print(colored('\nCalculating Cumulative Prod Complete!!!...\n','green'))
    
    print(colored('\nLoops to Calculate Monthly Values Starts...\n','green'))
    
    combined_ratio['Years']=combined_ratio['Years'].astype(int)
    
    combined_ratio=cumulative_to_monthly(combined_ratio,year_max)
    combined_ratio['Years']=combined_ratio['Years'].astype(int)
    combined_ratio=combined_ratio.reset_index(drop=True)
    
# =============================================================================
#     for index,row in combined_ratio.iloc[:,:].iterrows():
#         if(((row[0]=='Soja')&(row[2]=='Area Plantada'))|((row[0]=='Milho 1 Safra')&(row[2]=='Area Plantada'))):
#             combined_ratio.loc[index,'Years']=combined_ratio.loc[index,'Years']-1
#         else:
#             combined_ratio.loc[index,'Years']=combined_ratio.loc[index,'Years']
# =============================================================================
    print(combined_ratio)
    
# =============================================================================
#     for index,row in combined_ratio.iloc[:,:].iterrows():
#         if(((row[0]=='Soja')&(row[2]=='Area Plantada'))|((row[0]=='Milho 1 Safra')&(row[2]=='Area Plantada'))):
#             combined_ratio.loc[index,'Years']=combined_ratio.loc[index,'Years']-1
#         else:
#             combined_ratio.loc[index,'Years']=combined_ratio.loc[index,'Years']
# =============================================================================
    
    for index,row in combined_ratio.iloc[:,:].iterrows():
        if((combined_ratio.loc[index,'Years']==year_max) & (combined_ratio.loc[index,'Month']>month_max)):
            combined_ratio.loc[index,'Monthly_Value']=0
    for index,row in combined_ratio.iloc[:,:].iterrows():
        if(combined_ratio.loc[index,'Years']>year_max):
            combined_ratio.loc[index,'Monthly_Value']=0
                
    print(colored('\nAll Loops Complete !!!\n','green'))
    return(combined_ratio)

def generate_monthly2(combined_ratio,state1,year_list_monthly):
    combined_ratio=combined_ratio[(combined_ratio['Type']=='Producao')]
    combined_ratio=combined_ratio[['Years','Month','Produto','UF','Monthly_Value']]
    for index,row in combined_ratio.iloc[:,:].iterrows():
        for j,rowj in state1.iloc[:,:].iterrows():
            if(row[3]==rowj[0]):
                combined_ratio.loc[index,'Estado']=state1.loc[j,'State_Name'].upper()
    combined_ratio=combined_ratio[['Years','Month','Produto','Estado','Monthly_Value']]
    for index,row in combined_ratio.iloc[:,:].iterrows():
        pre_year=row[0]-1
        combined_ratio.loc[index,'Years'] = str(pre_year)+'-'+str(row[0])
        
    combined_ratio.columns=['Year','Month','Crops','Estado','Production (1000 ton)']
    combined_ratio['Crops'] = combined_ratio['Crops'].map({'Soja':'Soja','Milho 1 Safra': 'Milho 1ª Safra', 'Milho 2 Safra': 'Milho 2ª Safra'})    
    return(combined_ratio)

def month_num(month_name):
    if month_name=='Jan':
        return(1)
    if month_name=='Feb':
        return(2)
    if month_name=='Mar':
        return(3)
    if month_name=='Apr':
        return(4)
    if month_name=='May':
        return(5)
    if month_name=='Jun':
        return(6)
    if month_name=='Jul':
        return(7)
    if month_name=='Aug':
        return(8)
    if month_name=='Sep':
        return(9)
    if month_name=='Oct':
        return(10)
    if month_name=='Nov':
        return(11)
    if month_name=='Dec':
        return(12)

def getting_prod_data(latest_data,pred_year):
    latest_data=latest_data[(latest_data['Type']=='Producao')&(latest_data['Years']<=pred_year)]
    return(latest_data)

def getting_plant_data(latest_data,pred_year,crop):
    latest_data=latest_data[(latest_data['Type']=='Area Plantada')&(latest_data['Years']<=pred_year)]
    if((crop=='Milho 1 Safra')|(crop=='Soja')):
        latest_data=latest_data[(latest_data['Years']<=2018)]
    return(latest_data)
    
def preparing_covariate1(data,pred_data):
    data.insert(0,'Ano',data['DATE'].str.split('/').str[2])
    data.insert(1,'Mes',data['DATE'].str.split('/').str[0])
    del data['DATE']
    data.columns=["Ano","Mes","Value"]
    pred_data=preparing_predicted_data(pred_data)
    data_full=pd.concat([data.reset_index(drop=True),pred_data.reset_index(drop=True)],axis=0)
    data_full=data_full.reset_index(drop=True)
    data_full['Ano'] = data_full['Ano'].astype(int)
    data_full['Mes'] = data_full['Mes'].astype(int)
    return(data_full)

def preparing_predicted_data(pred_data):
    pred_data['Ano'] = pred_data[u'Unnamed: 0'].str.split(' ').str[1]
    pred_data['Mes_Name'] = pred_data[u'Unnamed: 0'].str.split(' ').str[0]
    for index,row in pred_data.iloc[:,:].iterrows():
        pred_data.loc[index,'Mes']=month_num(pred_data.loc[index,'Mes_Name'])
    pred_data=pred_data[['Ano','Mes','Point.Forecast']]
    pred_data.columns=['Ano','Mes','Value']
    return(pred_data)

def preparing_covariate2(data,pred_data):
    pred_data=preparing_predicted_data(pred_data)
    data_full=pd.concat([data.reset_index(drop=True),pred_data.reset_index(drop=True)],axis=0)
    data_full=data_full.reset_index(drop=True)
    data_full['Ano'] = data_full['Ano'].astype(int)
    data_full['Mes'] = data_full['Mes'].astype(int)
    return(data_full)
    
def max_year(data):
    max_year=data['Ano'].max()
    return(max_year)  

def max_month(data):
    year_max=max_year(data)
    max_month=data[data['Ano']==year_max]['Mes'].max()
    return(max_month)
    
def min_year(prod_data):
    min_year=prod_data['Years'].min()
    return(min_year)
    
def min_month(prod_data):
    year_min=min_year(prod_data)
    min_month=prod_data[prod_data['Years']==year_min]['Month'].min()
    return(min_month)

def preparing_covariate(data):
    df = pd.DataFrame(data[0])
    df.columns = df.iloc[0]
    df=df.drop(0)
    df["Year"]= df["Month"].str.split(" ").str[1]
    df['Month_Name']=df['Month'].str.split(" ").str[0]
    for index,row in df.iloc[:,:].iterrows():
        df.loc[index,'Month']=month_num(df.loc[index,'Month_Name'])
    df['Year'] = df['Year'].astype(int)
    df['Month'] = df['Month'].astype(int)   
    df=df[['Year','Month','Price']]
    df.columns=['Ano','Mes','Value']
    df['Value'] = df['Value'].astype(float)
    return(df)

def creating_covariate_lags(covariate,cov_name,lag_no):
    covariate_value=pd.concat([covariate, covariate['Value'].shift(lag_no)], axis=1).dropna()
    covariate_value=covariate_value.reset_index(drop=True)
    covariate_value.columns=['Ano','Mes',cov_name,'Lag'+str(lag_no)+'_'+str(cov_name)]
    covariate_value['Ano']=covariate_value['Ano'].astype(int)
    covariate_value['Mes']=covariate_value['Mes'].astype(int)
    covariate_value=covariate_value.reset_index(drop=True)
    covariate_value=covariate_value[['Ano','Mes','Lag'+str(lag_no)+'_'+str(cov_name)]]
    return(covariate_value)

def creating_plantation_lags(crop,state_code,plantation,lag_no):
    state_plantation=plantation[(plantation['UF']==state_code) & (plantation['Produto']==crop)]
    state_plantation=state_plantation[['Years','Month','Monthly_Value']]
    state_plantation.columns=['Years','Month','Plantation']
    plantation_value=pd.concat([state_plantation, state_plantation['Plantation'].shift(lag_no)], axis=1)
    plantation_value['Years']=plantation_value['Years'].astype(int)
    plantation_value['Month']=plantation_value['Month'].astype(int)
    plantation_value=plantation_value.fillna(0)
    plantation_value=plantation_value.reset_index(drop=True)
    plantation_value.columns=['Ano','Mes','Plantation','Lag'+str(lag_no)+'_Plantation'] 
    return(plantation_value)

def data_preprocessing_prod(crop,state_code,production):
    state_production=production[(production['UF']==state_code) & (production['Produto']==crop)]
    state_production=state_production[['Years','Month','Monthly_Value']]
    state_production['Date']=state_production['Month'].map(str)+"/1/"+state_production['Years'].map(str)
    state_production=state_production[['Date','Years','Month',"Monthly_Value"]]
    state_production['Years']=state_production['Years'].astype(int)
    state_production['Month']=state_production['Month'].astype(int)
    state_production.columns=['Date','Years','Month',"Production"] 
    return(state_production)
    
def data_preprocessing_plant(crop,state_code,plantation):
    state_plantation=plantation[(plantation['UF']==state_code) & (plantation['Produto']==crop)]
    state_plantation=state_plantation[['Years','Month','Monthly_Value']]
    state_plantation.columns=['Ano','Mes','Plantation']
    state_plantation['Ano']=state_plantation['Ano'].astype(int)
    state_plantation['Mes']=state_plantation['Mes'].astype(int)
    return(state_plantation)

def create_all_covariates(prod_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    prod=data_preprocessing_prod('Milho 2 Safra','GO',prod_data)
    del prod['Production']
       
    prod=pd.merge(prod,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    prod=delete_month_year(prod)
    prod.columns=['Date','Years','Month','Exchange_Rate']
    
    prod=pd.merge(prod,interest_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    prod=delete_month_year(prod)
    prod.columns=['Date','Years','Month','Exchange_Rate','Interest_Rate']
    
    lag1_milho=creating_covariate_lags(milho2_price,'Milho_Price',1)
    lag3_milho=creating_covariate_lags(milho2_price,'Milho_Price',3)
    
    prod=pd.merge(prod,milho_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    prod=delete_month_year(prod)
    prod.columns=['Date','Years','Month','Exchange_Rate','Interest_Rate','Milho_Price']
    
    prod=pd.merge(prod,lag1_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    prod=delete_month_year(prod)
    
    prod=pd.merge(prod,lag3_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    prod=delete_month_year(prod)
    
    lag1_soya=creating_covariate_lags(soya2_price,'Soya_Price',1)
    lag3_soya=creating_covariate_lags(soya2_price,'Soya_Price',3)
    lag6_soya=creating_covariate_lags(soya2_price,'Soya_Price',6)

    prod=pd.merge(prod,soya_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    prod=delete_month_year(prod)
    prod.columns=['Date','Years','Month','Exchange_Rate','Interest_Rate','Milho_Price','Lag1_Milho_Price','Lag3_Milho_Price','Soya_Price']
    
    prod=pd.merge(prod,lag1_soya,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    prod=delete_month_year(prod)
    
    prod=pd.merge(prod,lag3_soya,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    prod=delete_month_year(prod)
    
    prod=pd.merge(prod,lag6_soya,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    prod=delete_month_year(prod)
    
    del prod['Date']    
    return(prod)
    
def delete_month_year(data):
    del data['Ano']
    del data['Mes']
    return(data)
    
def delete_month_year2(data):
    del data['Years']
    del data['Month']
    return(data)

def plant_processing_predict(crop,state_code,plantation,year_list):
    state_plantation=plantation[(plantation['UF']==state_code) & (plantation['Produto']==crop)]
    state_plantation=state_plantation[['Years','Month','Monthly_Value']]
    state_plantation['Date']=state_plantation['Month'].map(str)+"/1/"+state_plantation['Years'].map(str)
    state_plantation=state_plantation[['Date','Years','Month',"Monthly_Value"]]
    state_plantation['Years']=state_plantation['Years'].astype(int)
    state_plantation['Month']=state_plantation['Month'].astype(int)
    state_plantation.columns=['Date','Years','Month',"Plantation"] 
    if((crop=='Soja')|(crop=='Milho 1 Safra')):
        df=pd.DataFrame(0,index=np.arange(12),columns=state_plantation.columns)
        df['Years']=2019
        df['Month']=list(range(1,13))
        df['Date']=df['Month'].map(str)+"/1/"+df['Years'].map(str)
        df['Plantation']=0
        state_plantation=pd.concat([state_plantation,df],axis=0)    
    return(state_plantation)

def concat_actual_pred_plant(plantation,pred_plant,year_list,crop):
    max_year=year_list[0]
    max_month=year_list[2]
    actual=plantation[['Ano','Mes','Plantation']]
    plantation1=actual[actual['Ano']<max_year]
    if(actual['Ano'].max()<max_year):
        plantation2=pred_plant[(pred_plant['Ano']==max_year)&(pred_plant['Mes']<=max_month)]
    else:
        plantation2=actual[(actual['Ano']==max_year)&(actual['Mes']<=max_month)]
    actual_plant=pd.concat([plantation1.reset_index(drop=True),plantation2.reset_index(drop=True)],axis=0)     
    actual_plant=actual_plant.reset_index(drop=True)
    pred_plant1=pred_plant[(pred_plant['Ano']==max_year)&(pred_plant['Mes']>max_month)]
    pred_plant2=pred_plant[pred_plant['Ano']>max_year]
    pred=pd.concat([pred_plant1.reset_index(drop=True),pred_plant2.reset_index(drop=True)],axis=0)
    pred=pred.reset_index(drop=True)
    actual_pred_plant=pd.concat([actual_plant.reset_index(drop=True),pred.reset_index(drop=True)],axis=0)
    return(actual_pred_plant)

def new_plant_lags(plantation,pred_plant,lag_no,year_list,crop):
    max_year=year_list[0]
    max_month=year_list[2]
    actual=plantation[['Ano','Mes','Plantation']]
    plantation1=actual[actual['Ano']<max_year]
    if(actual['Ano'].max()<max_year):
        plantation2=pred_plant[(pred_plant['Ano']==max_year)&(pred_plant['Mes']<=max_month)]
    else:
        plantation2=actual[(actual['Ano']==max_year)&(actual['Mes']<=max_month)]
    actual_plant=pd.concat([plantation1.reset_index(drop=True),plantation2.reset_index(drop=True)],axis=0)     
    actual_plant=actual_plant.reset_index(drop=True)
    pred_plant1=pred_plant[(pred_plant['Ano']==max_year)&(pred_plant['Mes']>max_month)]
    pred_plant2=pred_plant[pred_plant['Ano']>max_year]
    pred=pd.concat([pred_plant1.reset_index(drop=True),pred_plant2.reset_index(drop=True)],axis=0)
    pred=pred.reset_index(drop=True)
    actual_pred_plant=pd.concat([actual_plant.reset_index(drop=True),pred.reset_index(drop=True)],axis=0)
    actual_pred_plant=pd.concat([actual_pred_plant, actual_pred_plant['Plantation'].shift(lag_no)], axis=1)
    actual_pred_plant.columns=['Ano','Mes','Plantation','Lag'+str(lag_no)+'_Plantation'] 
    actual_pred_plant=actual_pred_plant.reset_index(drop=True)
    return(actual_pred_plant)

def create_milho2_plant_input_GO(year_list,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    GO_plant=plant_processing_predict('Milho 2 Safra','GO',plant_data,year_list)
    
    GO_plant=pd.merge(GO_plant,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    GO_plant=delete_month_year(GO_plant)
    GO_plant.columns=['Date','Years','Month','Plantation','Exchange_Rate']
    
    lag3_soya=creating_covariate_lags(soya2_price,'Soya_Price',3)
    GO_plant=pd.merge(GO_plant,lag3_soya,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    GO_plant=delete_month_year(GO_plant) 
    
    GO_plant=pd.merge(GO_plant,soya_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    GO_plant=delete_month_year(GO_plant)
    GO_plant.columns=['Date','Years','Month','Plantation','Exchange_Rate','Lag3_Soya_Price','Soya_Price']
    
    GO_plant=delete_month_year2(GO_plant)
    return(GO_plant)
    
def create_milho2_input_GO(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,pred_plant,year_list):
    ## Milho2 - Data Files - GO
    lag3_plantation=creating_plantation_lags('Milho 2 Safra','GO',plant_data,3)
    lag3_plantation=new_plant_lags(lag3_plantation,pred_plant,3,year_list,'Milho2')
    
    GO_prod=data_preprocessing_prod('Milho 2 Safra','GO',prod_data)
    
    GO_prod=pd.merge(GO_prod,lag3_plantation,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    GO_prod=delete_month_year(GO_prod)
    
    GO_prod=pd.merge(GO_prod,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    GO_prod=delete_month_year(GO_prod)
    GO_prod.columns=['Date','Years','Month',"Production",'Plantation','Lag3_Plantation','Exchange_Rate']
     
    GO_prod=pd.merge(GO_prod,soya_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    GO_prod=delete_month_year(GO_prod)
    GO_prod.columns=['Date','Years','Month',"Production",'Plantation','Lag3_Plantation','Exchange_Rate','Soya_Price']
    
    lag3_milho=creating_covariate_lags(milho2_price,'Milho_Price',3)
    GO_prod=pd.merge(GO_prod,lag3_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    GO_prod=delete_month_year(GO_prod)
 
    GO_prod=delete_month_year2(GO_prod)    
    return(GO_prod)

def create_milho2_plant_input_MT(year_list,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    MT_plant=plant_processing_predict('Milho 2 Safra','MT',plant_data,year_list)
    
    MT_plant=pd.merge(MT_plant,milho_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MT_plant=delete_month_year(MT_plant) 
    MT_plant.columns=['Date','Years','Month','Plantation','Milho_Price']
    
    lag3_milho=creating_covariate_lags(milho2_price,'Milho_Price',3)
    MT_plant=pd.merge(MT_plant,lag3_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MT_plant=delete_month_year(MT_plant)
    
    MT_plant=pd.merge(MT_plant,interest_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MT_plant=delete_month_year(MT_plant)
    MT_plant.columns=['Date','Years','Month','Plantation','Milho_Price','Lag3_Milho_Price','Interest_Rate']
    
    MT_plant=delete_month_year2(MT_plant)
    return(MT_plant)

def create_milho2_input_MT(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,pred_plant,year_list):    
    ## Milho2 - Data Files - MT
    lag3_plantation=creating_plantation_lags('Milho 2 Safra','MT',plant_data,3)
    lag3_plantation=new_plant_lags(lag3_plantation,pred_plant,3,year_list,'Milho2')
    
    MT_prod=data_preprocessing_prod('Milho 2 Safra','MT',prod_data)
    MT_prod=pd.merge(MT_prod,lag3_plantation,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MT_prod=delete_month_year(MT_prod)
    
    MT_prod=pd.merge(MT_prod,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MT_prod=delete_month_year(MT_prod)
    MT_prod.columns=['Date','Years','Month',"Production",'Plantation','Lag3_Plantation','Exchange_Rate']
    
    MT_prod=pd.merge(MT_prod,soya_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MT_prod=delete_month_year(MT_prod)
    MT_prod.columns=['Date','Years','Month',"Production",'Plantation','Lag3_Plantation','Exchange_Rate','Soya_Price']
    
    MT_prod=delete_month_year2(MT_prod)
    
    return(MT_prod)

def create_milho2_input_PR(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    ## Milho2 - Data Files - PR
    PR_prod=data_preprocessing_prod('Milho 2 Safra','PR',prod_data)
    
    PR_prod=pd.merge(PR_prod,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    PR_prod=delete_month_year(PR_prod)
    PR_prod.columns=['Date','Years','Month',"Production",'Exchange_Rate']
    
    PR_prod=delete_month_year2(PR_prod)
    return(PR_prod)
    
def create_milho2_input_MS(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    ## Milho2 - Data Files - MS
    MS_prod=data_preprocessing_prod('Milho 2 Safra','MS',prod_data)
    
    MS_prod=pd.merge(MS_prod,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MS_prod=delete_month_year(MS_prod)
    MS_prod.columns=['Date','Years','Month',"Production",'Exchange_Rate']
    
    MS_prod=delete_month_year2(MS_prod)
    return(MS_prod)

def create_milho1_input_BA(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    ## Milho1 - Data Files - BA
    BA_prod=data_preprocessing_prod('Milho 1 Safra','BA',prod_data)
    
    BA_prod=pd.merge(BA_prod,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    BA_prod=delete_month_year(BA_prod)
    BA_prod.columns=['Date','Years','Month',"Production",'Exchange_Rate']
    
    BA_prod=pd.merge(BA_prod,milho_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    BA_prod=delete_month_year(BA_prod)
    BA_prod.columns=['Date','Years','Month','Production','Exchange_Rate','Milho_Price']
    
    BA_prod=delete_month_year2(BA_prod)
    return(BA_prod)

def create_milho1_input_GO(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    ## Milho1 - Data Files - GO
    GO_prod=data_preprocessing_prod('Milho 1 Safra','GO',prod_data)
    
    GO_prod=pd.merge(GO_prod,milho_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    GO_prod=delete_month_year(GO_prod)
    GO_prod.columns=['Date','Years','Month',"Production",'Milho_Price']
    
    GO_prod=delete_month_year2(GO_prod)
    return(GO_prod)

def create_milho1_plant_input_MA(year_list,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    MA_plant=plant_processing_predict('Milho 1 Safra','MA',plant_data,year_list)
    
    MA_plant=pd.merge(MA_plant,interest_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MA_plant=delete_month_year(MA_plant)
    MA_plant.columns=['Date','Years','Month','Plantation','Interest_Rate']
    
    MA_plant=pd.merge(MA_plant,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MA_plant=delete_month_year(MA_plant)
    MA_plant.columns=['Date','Years','Month','Plantation','Interest_Rate','Exchange_Rate']
    
    lag3_milho=creating_covariate_lags(milho2_price,'Milho_Price',3)
    MA_plant=pd.merge(MA_plant,lag3_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MA_plant=delete_month_year(MA_plant)
    
    MA_plant=delete_month_year2(MA_plant)
    return(MA_plant)

def create_milho1_input_MA(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,pred_plant,year_list):
    ## Milho1 - Data Files - MA
    lag3_plantation=creating_plantation_lags('Milho 1 Safra','MA',plant_data,3)
    lag3_plantation=new_plant_lags(lag3_plantation,pred_plant,3,year_list,'Milho1')
    
    MA_prod=data_preprocessing_prod('Milho 1 Safra','MA',prod_data)
    
    MA_prod=pd.merge(MA_prod,lag3_plantation,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MA_prod=delete_month_year(MA_prod)
    
    MA_prod=pd.merge(MA_prod,milho_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MA_prod=delete_month_year(MA_prod)
    MA_prod.columns=['Date','Years','Month',"Production",'Plantation','Lag3_Plantation','Milho_Price']
    
    MA_prod=pd.merge(MA_prod,soya_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MA_prod=delete_month_year(MA_prod)
    MA_prod.columns=['Date','Years','Month',"Production",'Plantation','Lag3_Plantation','Milho_Price','Soya_Price']
    
    MA_prod=pd.merge(MA_prod,interest_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MA_prod=delete_month_year(MA_prod)
    MA_prod.columns=['Date','Years','Month',"Production",'Plantation','Lag3_Plantation','Milho_Price','Soya_Price','Interest_Rate']
    
    MA_prod=delete_month_year2(MA_prod)
    return(MA_prod)

def create_milho1_input_MG(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    ## Milho1 - Data Files - MG
    MG_prod=data_preprocessing_prod('Milho 1 Safra','MG',prod_data)
    
    MG_prod=pd.merge(MG_prod,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MG_prod=delete_month_year(MG_prod)
    MG_prod.columns=['Date','Years','Month',"Production",'Exchange_Rate']
    
    MG_prod=delete_month_year2(MG_prod)   
    return(MG_prod)

def create_milho1_plant_input_PR(year_list,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    PR_plant=plant_processing_predict('Milho 1 Safra','PR',plant_data,year_list)
    
    lag1_soya=creating_covariate_lags(soya2_price,'Soya_Price',1)
    PR_plant=pd.merge(PR_plant,lag1_soya,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    PR_plant=delete_month_year(PR_plant)
    
    PR_plant=pd.merge(PR_plant,soya_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    PR_plant=delete_month_year(PR_plant)
    PR_plant.columns=['Date','Years','Month','Plantation','Lag1_Soya_Price','Soya_Price']
    
    PR_plant=pd.merge(PR_plant,interest_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    PR_plant=delete_month_year(PR_plant)
    PR_plant.columns=['Date','Years','Month','Plantation','Lag1_Soya_Price','Soya_Price','Interest_Rate']
    
    lag3_milho=creating_covariate_lags(milho2_price,'Milho_Price',3)
    PR_plant=pd.merge(PR_plant,lag3_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    PR_plant=delete_month_year(PR_plant)
    
    PR_plant=delete_month_year2(PR_plant)
    return(PR_plant)
    
def create_milho1_input_PR(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,pred_plant,year_list):
    ## Milho1 - Data Files - PR
    PR_plant=data_preprocessing_plant('Milho 1 Safra','PR',plant_data)
    PR_plant=concat_actual_pred_plant(PR_plant,pred_plant,year_list,'Milho1')
    
    PR_prod=data_preprocessing_prod('Milho 1 Safra','PR',prod_data)
    
    PR_prod=pd.merge(PR_prod,PR_plant,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    PR_prod=delete_month_year(PR_prod)
    
    PR_prod=pd.merge(PR_prod,milho_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    PR_prod=delete_month_year(PR_prod)
    PR_prod.columns=['Date','Years','Month',"Production",'Plantation','Milho_Price']
    
    PR_prod=delete_month_year2(PR_prod)  
    return(PR_prod)

def create_milho1_plant_input_RS(year_list,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    RS_plant=plant_processing_predict('Milho 1 Safra','RS',plant_data,year_list)
    
    lag3_soya=creating_covariate_lags(soya2_price,'Soya_Price',3)
    RS_plant=pd.merge(RS_plant,lag3_soya,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    RS_plant=delete_month_year(RS_plant)
    
    lag1_soya=creating_covariate_lags(soya2_price,'Soya_Price',1)
    RS_plant=pd.merge(RS_plant,lag1_soya,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    RS_plant=delete_month_year(RS_plant)
    
    RS_plant=pd.merge(RS_plant,soya_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    RS_plant=delete_month_year(RS_plant)
    RS_plant.columns=['Date','Years','Month','Plantation','Lag3_Soya_Price','Lag1_Soya_Price','Soya_Price']
    
    RS_plant=pd.merge(RS_plant,interest_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    RS_plant=delete_month_year(RS_plant)
    RS_plant.columns=['Date','Years','Month','Plantation','Lag3_Soya_Price','Lag1_Soya_Price','Soya_Price','Interest_Rate']
    
    RS_plant=delete_month_year2(RS_plant)
    return(RS_plant)

def create_milho1_input_RS(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,pred_plant,year_list):
    ## Milho1 - Data Files - RS
    RS_plant=data_preprocessing_plant('Milho 1 Safra','RS',plant_data)
    RS_plant=concat_actual_pred_plant(RS_plant,pred_plant,year_list,'Milho1')
    
    RS_prod=data_preprocessing_prod('Milho 1 Safra','RS',prod_data)
    
    RS_prod=pd.merge(RS_prod,RS_plant,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    RS_prod=delete_month_year(RS_prod)
    
    RS_prod=pd.merge(RS_prod,interest_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    RS_prod=delete_month_year(RS_prod)
    RS_prod.columns=['Date','Years','Month',"Production",'Plantation','Interest_Rate']
    
    RS_prod=delete_month_year2(RS_prod)
    return(RS_prod)

def create_milho1_plant_input_SC(year_list,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    SC_plant=plant_processing_predict('Milho 1 Safra','SC',plant_data,year_list)
    
    lag3_milho=creating_covariate_lags(milho2_price,'Milho_Price',3)
    SC_plant=pd.merge(SC_plant,lag3_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SC_plant=delete_month_year(SC_plant)
    
    SC_plant=pd.merge(SC_plant,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SC_plant=delete_month_year(SC_plant)
    SC_plant.columns=['Date','Years','Month','Plantation','Lag3_Milho_Price','Exchange_Rate']
    
    lag1_milho=creating_covariate_lags(milho2_price,'Milho_Price',1)
    SC_plant=pd.merge(SC_plant,lag1_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SC_plant=delete_month_year(SC_plant)
    
    SC_plant=delete_month_year2(SC_plant)
    return(SC_plant)

def create_milho1_input_SC(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,pred_plant,year_list):
    ## Milho1 - Data Files - SC
    SC_plant=data_preprocessing_plant('Milho 1 Safra','SC',plant_data)
    SC_plant=concat_actual_pred_plant(SC_plant,pred_plant,year_list,'Milho1')
    
    SC_prod=data_preprocessing_prod('Milho 1 Safra','SC',prod_data)
    
    SC_prod=pd.merge(SC_prod,SC_plant,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SC_prod=delete_month_year(SC_prod)
    
    SC_prod=pd.merge(SC_prod,interest_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SC_prod=delete_month_year(SC_prod)
    SC_prod.columns=['Date','Years','Month',"Production",'Plantation','Interest_Rate']
    
    SC_prod=delete_month_year2(SC_prod)
    return(SC_prod)

def create_milho1_plant_input_SP(year_list,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    SP_plant=plant_processing_predict('Milho 1 Safra','SP',plant_data,year_list)
    
    lag3_milho=creating_covariate_lags(milho2_price,'Milho_Price',3)
    SP_plant=pd.merge(SP_plant,lag3_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SP_plant=delete_month_year(SP_plant)
    
    SP_plant=pd.merge(SP_plant,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SP_plant=delete_month_year(SP_plant)
    SP_plant.columns=['Date','Years','Month','Plantation','Lag3_Milho_Price','Exchange_Rate']
    
    lag1_milho=creating_covariate_lags(milho2_price,'Milho_Price',1)
    SP_plant=pd.merge(SP_plant,lag1_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SP_plant=delete_month_year(SP_plant)
    
    SP_plant=delete_month_year2(SP_plant)
    return(SP_plant)

def create_milho1_input_SP(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,pred_plant,year_list):
    ## Milho1 - Data Files - SP
    lag3_plantation=creating_plantation_lags('Milho 1 Safra','SP',plant_data,3)
    lag3_plantation=new_plant_lags(lag3_plantation,pred_plant,3,year_list,'Milho1')
    
    lag4_plantation=creating_plantation_lags('Milho 1 Safra','SP',plant_data,4)
    lag4_plantation=new_plant_lags(lag4_plantation,pred_plant,4,year_list,'Milho1')
    
    SP_prod=data_preprocessing_prod('Milho 1 Safra','SP',prod_data)
    
    SP_prod=pd.merge(SP_prod,lag3_plantation,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SP_prod=delete_month_year(SP_prod)
    del SP_prod['Plantation']
    
    SP_prod=pd.merge(SP_prod,lag4_plantation,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SP_prod=delete_month_year(SP_prod)
    
    SP_prod=pd.merge(SP_prod,soya_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SP_prod=delete_month_year(SP_prod)
    SP_prod.columns=['Date','Years','Month',"Production",'Lag3_Plantation','Plantation','Lag4_Plantation','Soya_Price']
    
    lag1_soya=creating_covariate_lags(soya2_price,'Soya_Price',1)
    lag3_soya=creating_covariate_lags(soya2_price,'Soya_Price',3)
    lag6_soya=creating_covariate_lags(soya2_price,'Soya_Price',6)
    
    SP_prod=pd.merge(SP_prod,lag1_soya,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SP_prod=delete_month_year(SP_prod)
    
    SP_prod=pd.merge(SP_prod,lag3_soya,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SP_prod=delete_month_year(SP_prod)
    
    SP_prod=pd.merge(SP_prod,lag6_soya,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SP_prod=delete_month_year(SP_prod)
    
    SP_prod=delete_month_year2(SP_prod)
    return(SP_prod)

def create_soya_plant_input_BA(year_list,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    BA_plant=plant_processing_predict('Soja','BA',plant_data,year_list)
    
    lag1_milho=creating_covariate_lags(milho2_price,'Milho_Price',1)
    BA_plant=pd.merge(BA_plant,lag1_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    BA_plant=delete_month_year(BA_plant)
    
    BA_plant=pd.merge(BA_plant,interest_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    BA_plant=delete_month_year(BA_plant)
    BA_plant.columns=['Date','Years','Month',"Plantation",'Lag1_Milho_Price','Interest_Rate']
    
    lag3_soya=creating_covariate_lags(soya2_price,'Soya_Price',3)
    BA_plant=pd.merge(BA_plant,lag3_soya,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    BA_plant=delete_month_year(BA_plant)
    
    BA_plant=delete_month_year2(BA_plant)
    return(BA_plant)
    
def create_soya_input_BA(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,pred_plant,year_list):
    ## Soya - Data Files - BA
    lag2_plantation=creating_plantation_lags('Soja','BA',plant_data,2)
    lag2_plantation=new_plant_lags(lag2_plantation,pred_plant,2,year_list,'Soya')
    
    lag3_plantation=creating_plantation_lags('Soja','BA',plant_data,3)
    lag3_plantation=new_plant_lags(lag3_plantation,pred_plant,3,year_list,'Soya')
    BA_prod=data_preprocessing_prod('Soja','BA',prod_data)
  
    BA_prod=pd.merge(BA_prod,lag2_plantation,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    BA_prod=delete_month_year(BA_prod)
    del BA_prod['Plantation']
    
    BA_prod=pd.merge(BA_prod,lag3_plantation,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    BA_prod=delete_month_year(BA_prod)
    
    BA_prod=pd.merge(BA_prod,interest_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    BA_prod=delete_month_year(BA_prod)
    BA_prod.columns=['Date','Years','Month',"Production (in '000 tonnes)",'Lag2_Plantation','Plantation','Lag3_Plantation','Interest_Rate']
    
    BA_prod=pd.merge(BA_prod,milho_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    BA_prod=delete_month_year(BA_prod)
    BA_prod.columns=['Date','Years','Month',"Production",'Lag2_Plantation','Plantation','Lag3_Plantation','Interest_Rate','Milho_Price']
    
    lag1_milho=creating_covariate_lags(milho2_price,'Milho_Price',1)
    lag3_milho=creating_covariate_lags(milho2_price,'Milho_Price',3)
    
    BA_prod=pd.merge(BA_prod,lag1_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    BA_prod=delete_month_year(BA_prod)
    
    BA_prod=pd.merge(BA_prod,lag3_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    BA_prod=delete_month_year(BA_prod)
    
    BA_prod=delete_month_year2(BA_prod) 
    return(BA_prod)

def create_soya_plant_input_GO(year_list,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    GO_plant=plant_processing_predict('Soja','GO',plant_data,year_list)
    
    GO_plant=pd.merge(GO_plant,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    GO_plant=delete_month_year(GO_plant)
    GO_plant.columns=['Date','Years','Month',"Plantation",'Exchange_Rate']
    
    lag3_soya=creating_covariate_lags(soya2_price,'Soya_Price',3)
    GO_plant=pd.merge(GO_plant,lag3_soya,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    GO_plant=delete_month_year(GO_plant)
    
    lag1_soya=creating_covariate_lags(soya2_price,'Soya_Price',1)
    GO_plant=pd.merge(GO_plant,lag1_soya,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    GO_plant=delete_month_year(GO_plant)
    
    GO_plant=pd.merge(GO_plant,soya_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    GO_plant=delete_month_year(GO_plant)
    GO_plant.columns=['Date','Years','Month',"Plantation",'Exchange_Rate','Lag3_Soya_Price','Lag1_Soya_Price','Soya_Price']
    
    GO_plant=delete_month_year2(GO_plant)
    return(GO_plant)

def create_soya_input_GO(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,pred_plant,year_list):
    ## Soya - Data Files - GO
    GO_plant=data_preprocessing_plant('Soja','GO',plant_data)
    GO_plant=concat_actual_pred_plant(GO_plant,pred_plant,year_list,'Soya')
    
    GO_prod=data_preprocessing_prod('Soja','GO',prod_data)
    
    GO_prod=pd.merge(GO_prod,GO_plant,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    GO_prod=delete_month_year(GO_prod)
    
    GO_prod=pd.merge(GO_prod,milho_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    GO_prod=delete_month_year(GO_prod)
    GO_prod.columns=['Date','Years','Month',"Production",'Plantation','Milho_Price']
    
    GO_prod=delete_month_year2(GO_prod)
    return(GO_prod)

def create_soya_plant_input_MA(year_list,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    MA_plant=plant_processing_predict('Soja','MA',plant_data,year_list)
    
    lag3_milho=creating_covariate_lags(milho2_price,'Milho_Price',3)
    MA_plant=pd.merge(MA_plant,lag3_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MA_plant=delete_month_year(MA_plant)
    
    MA_plant=pd.merge(MA_plant,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MA_plant=delete_month_year(MA_plant)
    MA_plant.columns=['Date','Years','Month','Plantation','Lag3_Milho_Price','Exchange_Rate']
    
    lag1_milho=creating_covariate_lags(milho2_price,'Milho_Price',1)
    MA_plant=pd.merge(MA_plant,lag1_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MA_plant=delete_month_year(MA_plant)
    
    MA_plant=delete_month_year2(MA_plant)
    return(MA_plant)

def create_soya_input_MA(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,pred_plant,year_list):
    ## Soya - Data Files - MA
    MA_plant=data_preprocessing_plant('Soja','MA',plant_data)
    MA_plant=concat_actual_pred_plant(MA_plant,pred_plant,year_list,'Soya')
    
    MA_prod=data_preprocessing_prod('Soja','MA',prod_data)
    
    MA_prod=pd.merge(MA_prod,MA_plant,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MA_prod=delete_month_year(MA_prod)
    
    lag3_milho=creating_covariate_lags(milho2_price,'Milho_Price',3)
    MA_prod=pd.merge(MA_prod,lag3_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MA_prod=delete_month_year(MA_prod)
    
    MA_prod=pd.merge(MA_prod,interest_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MA_prod=delete_month_year(MA_prod)
    MA_prod.columns=['Date','Years','Month',"Production",'Plantation','Lag3_Milho_Price','Interest_Rate']
    
    MA_prod=delete_month_year2(MA_prod)   
    return(MA_prod)

def create_soya_plant_input_MG(year_list,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    MG_plant=plant_processing_predict('Soja','MG',plant_data,year_list)
    
    lag3_milho=creating_covariate_lags(milho2_price,'Milho_Price',3)
    MG_plant=pd.merge(MG_plant,lag3_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MG_plant=delete_month_year(MG_plant)
    
    MG_plant=pd.merge(MG_plant,interest_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MG_plant=delete_month_year(MG_plant)
    MG_plant.columns=['Date','Years','Month','Plantation','Lag3_Milho_Price','Interest_Rate']
    
    MG_plant=pd.merge(MG_plant,milho_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MG_plant=delete_month_year(MG_plant)
    MG_plant.columns=['Date','Years','Month','Plantation','Lag3_Milho_Price','Interest_Rate','Milho_Price']
    
    MG_plant=pd.merge(MG_plant,soya_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MG_plant=delete_month_year(MG_plant)
    MG_plant.columns=['Date','Years','Month','Plantation','Lag3_Milho_Price','Interest_Rate','Milho_Price','Soya_Price']
    
    MG_plant=delete_month_year2(MG_plant)
    return(MG_plant)

def create_soya_input_MG(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,pred_plant,year_list):
    ## Soya - Data Files - MG
    lag3_plantation=creating_plantation_lags('Soja','MG',plant_data,3)
    lag3_plantation=new_plant_lags(lag3_plantation,pred_plant,3,year_list,'Soya')
    MG_prod=data_preprocessing_prod('Soja','MG',prod_data)
    
    MG_prod=pd.merge(MG_prod,lag3_plantation,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MG_prod=delete_month_year(MG_prod)
    
    lag3_milho=creating_covariate_lags(milho2_price,'Milho_Price',3)
    MG_prod=pd.merge(MG_prod,lag3_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MG_prod=delete_month_year(MG_prod)
    
    MG_prod=pd.merge(MG_prod,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MG_prod=delete_month_year(MG_prod)
    MG_prod.columns=['Date','Years','Month',"Production",'Plantation','Lag3_Plantation','Lag3_Milho_Price','Exchange_Rate']
    
    MG_prod=delete_month_year2(MG_prod)
    return(MG_prod)
    
def create_soya_plant_input_MS(year_list,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    MS_plant=plant_processing_predict('Soja','MS',plant_data,year_list)
    
    lag1_milho=creating_covariate_lags(milho2_price,'Milho_Price',1)
    MS_plant=pd.merge(MS_plant,lag1_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MS_plant=delete_month_year(MS_plant)
    
    MS_plant=pd.merge(MS_plant,milho_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MS_plant=delete_month_year(MS_plant)
    MS_plant.columns=['Date','Years','Month','Plantation','Lag1_Milho_Price','Milho_Price']
    
    MS_plant=pd.merge(MS_plant,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MS_plant=delete_month_year(MS_plant)
    MS_plant.columns=['Date','Years','Month','Plantation','Lag1_Milho_Price','Milho_Price','Exchange_Rate']
    
    MS_plant=delete_month_year2(MS_plant)
    return(MS_plant)

def create_soya_input_MS(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,pred_plant,year_list):
    ## Soya - Data Files - MS
    lag3_plantation=creating_plantation_lags('Soja','MS',plant_data,3)
    lag3_plantation=new_plant_lags(lag3_plantation,pred_plant,3,year_list,'Soya')
    MS_prod=data_preprocessing_prod('Soja','MS',prod_data)
    
    MS_prod=pd.merge(MS_prod,lag3_plantation,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MS_prod=delete_month_year(MS_prod)
    
    lag3_milho=creating_covariate_lags(milho2_price,'Milho_Price',3)
    MS_prod=pd.merge(MS_prod,lag3_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MS_prod=delete_month_year(MS_prod)
    
    MS_prod=pd.merge(MS_prod,interest_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MS_prod=delete_month_year(MS_prod)
    MS_prod.columns=['Date','Years','Month',"Production",'Plantation','Lag3_Plantation','Lag3_Milho_Price','Interest_Rate']
    
    MS_prod=delete_month_year2(MS_prod)
    return(MS_prod)

def create_soya_plant_input_MT(year_list,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    MT_plant=plant_processing_predict('Soja','MT',plant_data,year_list)
    
    MT_plant=pd.merge(MT_plant,interest_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MT_plant=delete_month_year(MT_plant)
    MT_plant.columns=['Date','Years','Month','Plantation','Interest_Rate']
    
    lag3_soya=creating_covariate_lags(soya2_price,'Soya_Price',3)
    MT_plant=pd.merge(MT_plant,lag3_soya,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MT_plant=delete_month_year(MT_plant)
    
    MT_plant=pd.merge(MT_plant,soya_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MT_plant=delete_month_year(MT_plant)
    MT_plant.columns=['Date','Years','Month','Plantation','Interest_Rate','Lag3_Soya_Price','Soya_Price']
    
    MT_plant=delete_month_year2(MT_plant)
    return(MT_plant)

def create_soya_input_MT(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,pred_plant,year_list):
    ## Soya - Data Files - MT
    lag2_plantation=creating_plantation_lags('Soja','MT',plant_data,2)
    lag2_plantation=new_plant_lags(lag2_plantation,pred_plant,2,year_list,'Soya')
    
    lag3_plantation=creating_plantation_lags('Soja','MT',plant_data,3)
    lag3_plantation=new_plant_lags(lag3_plantation,pred_plant,3,year_list,'Soya')
    
    lag4_plantation=creating_plantation_lags('Soja','MT',plant_data,4)
    lag4_plantation=new_plant_lags(lag4_plantation,pred_plant,4,year_list,'Soya')
    
    MT_prod=data_preprocessing_prod('Soja','MT',prod_data)
    
    MT_prod=pd.merge(MT_prod,lag2_plantation,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MT_prod=delete_month_year(MT_prod)
    del MT_prod['Plantation']
    
    MT_prod=pd.merge(MT_prod,lag3_plantation,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MT_prod=delete_month_year(MT_prod)
    del MT_prod['Plantation']
    
    MT_prod=pd.merge(MT_prod,lag4_plantation,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MT_prod=delete_month_year(MT_prod)
    
    MT_prod=pd.merge(MT_prod,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MT_prod=delete_month_year(MT_prod)
    MT_prod.columns=['Date','Years','Month',"Production",'Lag2_Plantation','Lag3_Plantation','Plantation','Lag4_Plantation','Exchange_Rate']
    
    MT_prod=pd.merge(MT_prod,interest_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    MT_prod=delete_month_year(MT_prod)
    MT_prod.columns=['Date','Years','Month',"Production",'Lag2_Plantation','Lag3_Plantation','Plantation','Lag4_Plantation','Exchange_Rate','Interest_Rate']
    
    MT_prod=delete_month_year2(MT_prod)
    return(MT_prod)

def create_soya_plant_input_PI(year_list,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    PI_plant=plant_processing_predict('Soja','PI',plant_data,year_list)
    
    lag3_milho=creating_covariate_lags(milho2_price,'Milho_Price',3)
    PI_plant=pd.merge(PI_plant,lag3_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    PI_plant=delete_month_year(PI_plant)
    
    PI_plant=pd.merge(PI_plant,milho_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    PI_plant=delete_month_year(PI_plant)
    PI_plant.columns=['Date','Years','Month','Plantation','Lag3_Milho_Price','Milho_Price']
    
    PI_plant=pd.merge(PI_plant,interest_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    PI_plant=delete_month_year(PI_plant)
    PI_plant.columns=['Date','Years','Month','Plantation','Lag3_Milho_Price','Milho_Price','Interest_Rate']
    
    PI_plant=delete_month_year2(PI_plant)
    return(PI_plant)

def create_soya_input_PI(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,pred_plant,year_list):
    ## Soya - Data Files - PI
    lag3_plantation=creating_plantation_lags('Soja','PI',plant_data,3)
    lag3_plantation=new_plant_lags(lag3_plantation,pred_plant,3,year_list,'Soya')
    
    PI_prod=data_preprocessing_prod('Soja','PI',prod_data)
    
    PI_prod=pd.merge(PI_prod,lag3_plantation,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    PI_prod=delete_month_year(PI_prod)
  
    PI_prod=pd.merge(PI_prod,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    PI_prod=delete_month_year(PI_prod)
    PI_prod.columns=['Date','Years','Month',"Production",'Plantation','Lag3_Plantation','Exchange_Rate']
    
    PI_prod=delete_month_year2(PI_prod)
    return(PI_prod)

def create_soya_plant_input_PR(year_list,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    PR_plant=plant_processing_predict('Soja','PR',plant_data,year_list)
        
    PR_plant=pd.merge(PR_plant,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    PR_plant=delete_month_year(PR_plant)
    PR_plant.columns=['Date','Years','Month','Plantation','Exchange_Rate']
    
    lag3_milho=creating_covariate_lags(milho2_price,'Milho_Price',3)
    PR_plant=pd.merge(PR_plant,lag3_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    PR_plant=delete_month_year(PR_plant)
    
    PR_plant=delete_month_year2(PR_plant)
    return(PR_plant)

def create_soya_input_PR(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,pred_plant,year_list):
    ## Soya - Data Files - PR
    lag3_plantation=creating_plantation_lags('Soja','PR',plant_data,3)
    lag3_plantation=new_plant_lags(lag3_plantation,pred_plant,3,year_list,'Soya')
    
    PR_prod=data_preprocessing_prod('Soja','PR',prod_data)
    
    PR_prod=pd.merge(PR_prod,lag3_plantation,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    PR_prod=delete_month_year(PR_prod)
    
    PR_prod=delete_month_year2(PR_prod)
    return(PR_prod)

def create_soya_plant_input_RO(year_list,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    RO_plant=plant_processing_predict('Soja','RO',plant_data,year_list)
    
    lag1_milho=creating_covariate_lags(milho2_price,'Milho_Price',1)    
    RO_plant=pd.merge(RO_plant,lag1_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    RO_plant=delete_month_year(RO_plant)
    
    RO_plant=pd.merge(RO_plant,interest_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    RO_plant=delete_month_year(RO_plant)
    RO_plant.columns=['Date','Years','Month','Plantation','Lag1_Milho_Price','Interest_Rate']
    
    RO_plant=pd.merge(RO_plant,milho_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    RO_plant=delete_month_year(RO_plant)
    RO_plant.columns=['Date','Years','Month','Plantation','Lag1_Milho_Price','Interest_Rate','Milho_Price']
    
    RO_plant=delete_month_year2(RO_plant)
    return(RO_plant)

def create_soya_input_RO(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,pred_plant,year_list):
    ## Soya - Data Files - RO
    RO_plant=data_preprocessing_plant('Soja','RO',plant_data)
    RO_plant=concat_actual_pred_plant(RO_plant,pred_plant,year_list,'Soya')
    
    RO_prod=data_preprocessing_prod('Soja','RO',prod_data)
    
    RO_prod=pd.merge(RO_prod,RO_plant,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    RO_prod=delete_month_year(RO_prod)
    
    RO_prod=pd.merge(RO_prod,interest_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    RO_prod=delete_month_year(RO_prod)
    RO_prod.columns=['Date','Years','Month',"Production",'Plantation','Interest_Rate']
    
    lag3_milho=creating_covariate_lags(milho2_price,'Milho_Price',3)
    RO_prod=pd.merge(RO_prod,lag3_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    RO_prod=delete_month_year(RO_prod)
    
    RO_prod=delete_month_year2(RO_prod)
    return(RO_prod)

def create_soya_plant_input_RS(year_list,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    RS_plant=plant_processing_predict('Soja','RS',plant_data,year_list)
    
    lag3_soya=creating_covariate_lags(soya2_price,'Soya_Price',3)    
    RS_plant=pd.merge(RS_plant,lag3_soya,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    RS_plant=delete_month_year(RS_plant)
    
    lag1_soya=creating_covariate_lags(soya2_price,'Soya_Price',1)   
    RS_plant=pd.merge(RS_plant,lag1_soya,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    RS_plant=delete_month_year(RS_plant)
    
    RS_plant=pd.merge(RS_plant,soya_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    RS_plant=delete_month_year(RS_plant)
    RS_plant.columns=['Date','Years','Month','Plantation','Lag3_Soya_Price','Lag1_Soya_Price','Soya_Price']
    
    RS_plant=pd.merge(RS_plant,interest_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    RS_plant=delete_month_year(RS_plant)
    RS_plant.columns=['Date','Years','Month','Plantation','Lag3_Soya_Price','Lag1_Soya_Price','Soya_Price','Interest_Rate']
    
    RS_plant=delete_month_year2(RS_plant)
    return(RS_plant)

def create_soya_input_RS(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,pred_plant,year_list):
    ## Soya - Data Files - RS
    lag4_plantation=creating_plantation_lags('Soja','RS',plant_data,4)
    lag4_plantation=new_plant_lags(lag4_plantation,pred_plant,4,year_list,'Soya')
    
    RS_prod=data_preprocessing_prod('Soja','RS',prod_data)
    
    RS_prod=pd.merge(RS_prod,lag4_plantation,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    RS_prod=delete_month_year(RS_prod)
    
    RS_prod=pd.merge(RS_prod,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    RS_prod=delete_month_year(RS_prod)
    RS_prod.columns=['Date','Years','Month',"Production",'Plantation','Lag4_Plantation','Exchange_Rate']
    
    lag3_milho=creating_covariate_lags(milho2_price,'Milho_Price',3)
    RS_prod=pd.merge(RS_prod,lag3_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    RS_prod=delete_month_year(RS_prod)
    
    RS_prod=delete_month_year2(RS_prod)
    return(RS_prod)

def create_soya_plant_input_SC(year_list,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    SC_plant=plant_processing_predict('Soja','SC',plant_data,year_list)
    
    SC_plant=pd.merge(SC_plant,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SC_plant=delete_month_year(SC_plant)
    SC_plant.columns=['Date','Years','Month','Plantation','Exchange_Rate']
    
    lag1_milho=creating_covariate_lags(milho2_price,'Milho_Price',1)   
    SC_plant=pd.merge(SC_plant,lag1_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SC_plant=delete_month_year(SC_plant)
    
    SC_plant=delete_month_year2(SC_plant)
    return(SC_plant)

def create_soya_input_SC(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,pred_plant,year_list):
    ## Soya - Data Files - SC
    lag4_plantation=creating_plantation_lags('Soja','SC',plant_data,4)
    lag4_plantation=new_plant_lags(lag4_plantation,pred_plant,4,year_list,'Soya')
    
    SC_prod=data_preprocessing_prod('Soja','SC',prod_data)
    
    SC_prod=pd.merge(SC_prod,lag4_plantation,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SC_prod=delete_month_year(SC_prod)
    
    SC_prod=pd.merge(SC_prod,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SC_prod=delete_month_year(SC_prod)
    SC_prod.columns=['Date','Years','Month',"Production",'Plantation','Lag4_Plantation','Exchange_Rate']
    
    lag3_milho=creating_covariate_lags(milho2_price,'Milho_Price',3)
    SC_prod=pd.merge(SC_prod,lag3_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SC_prod=delete_month_year(SC_prod)
    
    SC_prod=delete_month_year2(SC_prod)
    return(SC_prod)

def create_soya_plant_input_SP(year_list,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    SP_plant=plant_processing_predict('Soja','SP',plant_data,year_list)
    
    lag1_milho=creating_covariate_lags(milho2_price,'Milho_Price',1)  
    SP_plant=pd.merge(SP_plant,lag1_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SP_plant=delete_month_year(SP_plant)
    
    SP_plant=pd.merge(SP_plant,interest_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SP_plant=delete_month_year(SP_plant)
    SP_plant.columns=['Date','Years','Month','Plantation','Lag1_Milho_Price','Interest_Rate']
    
    SP_plant=pd.merge(SP_plant,milho_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SP_plant=delete_month_year(SP_plant)
    SP_plant.columns=['Date','Years','Month','Plantation','Lag1_Milho_Price','Interest_Rate','Milho_Price']
    
    SP_plant=delete_month_year2(SP_plant)
    return(SP_plant)

def create_soya_input_SP(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,pred_plant,year_list):
    ## Soya - Data Files - SP
    lag3_plantation=creating_plantation_lags('Soja','SP',plant_data,3)
    lag3_plantation=new_plant_lags(lag3_plantation,pred_plant,3,year_list,'Soya')
    
    SP_prod=data_preprocessing_prod('Soja','SP',prod_data)
    
    SP_prod=pd.merge(SP_prod,lag3_plantation,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SP_prod=delete_month_year(SP_prod)
    
    SP_prod=pd.merge(SP_prod,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    SP_prod=delete_month_year(SP_prod)
    SP_prod.columns=['Date','Years','Month',"Production",'Plantation','Lag3_Plantation','Exchange_Rate']
    
    SP_prod=delete_month_year2(SP_prod)
    return(SP_prod)

def create_soya_plant_input_TO(year_list,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price):
    TO_plant=plant_processing_predict('Soja','TO',plant_data,year_list)
     
    TO_plant=pd.merge(TO_plant,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    TO_plant=delete_month_year(TO_plant)
    TO_plant.columns=['Date','Years','Month','Plantation','Exchange_Rate']
    
    lag3_milho=creating_covariate_lags(milho2_price,'Milho_Price',2)  
    TO_plant=pd.merge(TO_plant,lag3_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    TO_plant=delete_month_year(TO_plant)
    
    TO_plant=delete_month_year2(TO_plant)
    return(TO_plant)

def create_soya_input_TO(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,pred_plant,year_list):
    ## Soya - Data Files - TO
    lag2_plantation=creating_plantation_lags('Soja','TO',plant_data,2)
    lag2_plantation=new_plant_lags(lag2_plantation,pred_plant,2,year_list,'Soya')
    
    lag3_plantation=creating_plantation_lags('Soja','TO',plant_data,3)
    lag3_plantation=new_plant_lags(lag3_plantation,pred_plant,3,year_list,'Soya')
    
    lag4_plantation=creating_plantation_lags('Soja','TO',plant_data,4)
    lag4_plantation=new_plant_lags(lag4_plantation,pred_plant,4,year_list,'Soya')
    
    TO_prod=data_preprocessing_prod('Soja','TO',prod_data)
    
    TO_prod=pd.merge(TO_prod,lag2_plantation,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    TO_prod=delete_month_year(TO_prod)
    del TO_prod['Plantation']
    
    TO_prod=pd.merge(TO_prod,lag3_plantation,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    TO_prod=delete_month_year(TO_prod)
    del TO_prod['Plantation']
    
    TO_prod=pd.merge(TO_prod,lag4_plantation,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    TO_prod=delete_month_year(TO_prod)
    
    TO_prod=pd.merge(TO_prod,exchange_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    TO_prod=delete_month_year(TO_prod)
    TO_prod.columns=['Date','Years','Month',"Production",'Lag2_Plantation','Lag3_Plantation','Plantation','Lag4_Plantation','Exchange_Rate']
    
    TO_prod=pd.merge(TO_prod,interest_rate,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    TO_prod=delete_month_year(TO_prod)
    TO_prod.columns=['Date','Years','Month',"Production",'Lag2_Plantation','Lag3_Plantation','Plantation','Lag4_Plantation','Exchange_Rate','Interest_Rate']
    
    TO_prod=pd.merge(TO_prod,milho_price,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    TO_prod=delete_month_year(TO_prod)
    TO_prod.columns=['Date','Years','Month',"Production",'Lag2_Plantation','Lag3_Plantation','Plantation','Lag4_Plantation','Exchange_Rate','Interest_Rate','Milho_Price']
    
    lag3_milho=creating_covariate_lags(milho2_price,'Milho_Price',3)
    TO_prod=pd.merge(TO_prod,lag3_milho,left_on=["Years","Month"],right_on=["Ano","Mes"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    TO_prod=delete_month_year(TO_prod)
    
    TO_prod=delete_month_year2(TO_prod)
    return(TO_prod)

def processing_plantation_output(pred_plant):
    pred_plant.insert(0,'Year',pred_plant['Date'].str.split('/').str[2])
    pred_plant.insert(1,'Month',pred_plant['Date'].str.split('/').str[0])   
    pred_plant['Year']=pred_plant['Year'].astype(int)
    pred_plant['Month']=pred_plant['Month'].astype(int)
    del pred_plant['Date']
    pred_plant.columns=['Ano','Mes','Plantation']
    return(pred_plant)

def main():
    
    year_max=results.year_max
    month_max=results.month_max
    pred_year=int(input('Enter Prediction Year: '))
    
    init()
    print(colored('\nProduction Forecasting Starts...\n', 'green'))
    print(colored('\nRetrieving Agroconsult Input Files...\n','green'))
    
    ## Initialize state file 
    state1=get_state_file('../Input Files/')
    
    ## Reading harvest plantation monthly division file
    harvest_plantation=get_harvest_plantation_file('../Input Files/',state1)
 
    ## Years & Months Pre-processing (monthly file)
    years=harvest_plantation.iloc[:,5:]
    years=year_preprocessing(years)
    year_list_monthly=years.columns
    
    new_harvest_plantation=pd.concat([harvest_plantation[['UF','Cultura','Fase Safra','Quinzena']].reset_index(drop=True),years.reset_index(drop=True)],axis=1)
    new_harvest_plantation=month_preprocessing(new_harvest_plantation)
# =============================================================================
#     for index,row in new_harvest_plantation.iloc[:,:].iterrows():
#         if((row[2]=='Milho 1ª Safra')&(row[3]=='Colheita')):
#             new_harvest_plantation.loc[index,'2019']=0
#         if((row[2]=='Soja')&(row[3]=='Colheita')):
#             new_harvest_plantation.loc[index,'2019']=0
#         if((row[2]=='Milho 2ª Safra')&(row[3]=='Plantio')):
#             new_harvest_plantation.loc[index,'2019']=0
# =============================================================================
    
    ## Reading harvest plantation yearly division file
    harvest_plantation_yearly=pd.read_excel("../Input Files/Detalhe_Safra.xls",encoding=encoding)
    combined_yearly=data_preprocessing_yearly(harvest_plantation_yearly,year_list_monthly)
    combined_ratio=combine_plantation_harvest(new_harvest_plantation,year_list_monthly)
    
    print(colored('\nConverting Agroconsult Yearly Plantation and Production Actuals into Monthly Data (be patient... it will take 10 min to execute)...\n','green'))
    
    monthly_value=calculate_monthly(combined_yearly,combined_ratio,year_max,month_max)
    monthly_value.to_csv("../Data Directory/Monthly_Final.csv",encoding=encoding,index=False)
    
    print(colored('\nConversion of Agroconsult Yearly Plantation and Production Actual into Monthly Data Completed...\n','green'))
    #monthly_value=pd.read_csv("../Data Directory/Monthly_Final.csv",encoding=encoding)
    
    print(monthly_value)
    ## Adjusting the Production and Plantation Monthly Values to Yearly
    
# =============================================================================
#     grouped_monthly=monthly_value.groupby(['Produto','Type','UF','Yearly_value'])['Monthly_Value'].sum()
#     grouped_monthly=grouped_monthly.reset_index()
#     grouped_monthly['Diff_per']=grouped_monthly['Monthly_Value']/grouped_monthly['Yearly_value']
#     
#     for index,row in grouped_monthly.iloc[:,:].iterrows():
#         if((grouped_monthly.loc[index,'Diff_per']<1)&(grouped_monthly.loc[index,'Diff_per']>0)):
#                 produto=grouped_monthly.loc[index,'Produto']
#                 crop_type=grouped_monthly.loc[index,'Type']
#                 estado=grouped_monthly.loc[index,'UF']
#                 yearly=grouped_monthly.loc[index,'Yearly_value']
#                 subset=monthly_value[(monthly_value['Produto']==produto)|(monthly_value['Type']==crop_type)|(monthly_value['UF']==estado)|(monthly_value['Yearly_value']==yearly)]['Percentage']
#                 print(subset)
#                 row_index=subset.index.values+1
#                 monthly_value.loc[row_index, 'Percentage'] = subset['Percentage']
#                 monthly_value.loc[row_index, 'Monthly_Value'] = monthly_value.loc[row_index, 'Percentage']*monthly_value.loc[row_index, 'Yearly_value']
#     
# =============================================================================
   # print(grouped_monthly)
    monthly_value.to_csv("../Data Directory/Monthly_Final.csv",encoding=encoding,index=False)
# =============================================================================
#     combined_ratio2=combined_ratio2.reset_index()
#     combined_ratio2['Adjusted_Ratio']=combined_ratio2['Yearly_value']/combined_ratio2['Monthly_Value']
#     combined_ratio2=combined_ratio2.replace([np.inf, -np.inf], np.nan)
#     combined_ratio2=combined_ratio2.fillna(0)
#     combined_ratio3=pd.merge(monthly_value,combined_ratio2[[u'Produto','UF','Type','Years','Adjusted_Ratio']],on=[u'Produto','UF','Type','Years'],how='outer',right_index=False,sort=True,indicator=True).query("_merge == 'both'").drop('_merge',axis=1)
#     combined_ratio3['Monthly.Prod2']=combined_ratio3['Monthly_Value']*combined_ratio3['Adjusted_Ratio']
#     combined_ratio3['Cumulative.Prod2']=combined_ratio3['Cumulative_Prod']*combined_ratio3['Adjusted_Ratio']
#     combined_ratio3=combined_ratio3[[u'Produto','UF','Type','Years','Month','Percentage','Yearly_value','Cumulative.Prod2','Monthly.Prod2']]
#     combined_ratio3.columns=[u'Produto','UF','Type','Years','Month','Percentage','Yearly_value','Cumulative_Prod','Monthly_Value']
#     monthly_value=pd.concat([combined_ratio3.reset_index(drop=True),data_max_year.reset_index(drop=True)],axis=0)
#     monthly_value.to_csv('../Data Directory/Monthly_Final.csv',index=False,encoding=encoding)
# =============================================================================
    
    ## Get Production Data Monthly
    prod_data=getting_prod_data(monthly_value,pred_year)
    
    year_min=min_year(prod_data)
    month_min=min_month(prod_data)
    
    list1=[]
    list1.append(year_max)
    list1.append(year_min)
    list1.append(month_max)
    list1.append(month_min)
    list1.append(pred_year)
    
    df=pd.DataFrame(list1)
    df.index=['Year_Max','Year_Min','Month_Max','Month_Min','Year_Pred']
    df.to_csv("../Data Directory/year_variables.csv",header=False)
    
    print(colored('\nForecasting Covariates...\n','green'))
    
    ## Retrieving Exchange Rate
    exchange_rate=pd.read_csv("../Input Files/Cambio_Real.csv",encoding=encoding)
    exchange_rate["Year"]= exchange_rate["DATE"].str.split("-").str[0]
    exchange_rate['Month']=exchange_rate['DATE'].str.split("-").str[1]
    del exchange_rate['DATE']
    exchange_rate.insert(0,'DATE',exchange_rate['Month'].map(str)+"/1/"+exchange_rate['Year'].map(str))
    del exchange_rate['Year']
    del exchange_rate['Month']
    rstring="""
    function(data5){
    library(forecast)
    library(imputeTS)
    list1=strsplit(as.character(data5$DATE),"/")
    length=nrow(data5)
    min_year=list1[[1]][3]
    max_year=list1[[length]][3]
    max_month=list1[[length]][1]
    max_month=as.numeric(max_month)
    max_year=as.numeric(max_year)
    min_year=as.numeric(min_year)
    data_points=12-max_month
    if(max_year==2018)
    {
      max_len=12+data_points
    }
    if(max_year==2019)
    {
      max_len=data_points
    }  
    real_ts <- ts(data5[,c(2)],start=c(min_year,1),frequency=12)
    ARIMAfit <- arima(real_ts,order=c(1,1,1),seasonal=list(order=c(1,1,1),period=12))
    pred <- forecast(ARIMAfit,h=max_len)
    pred2<-as.data.frame(pred)
    write.csv(pred,"../Data Directory/Prediction - Covariates/Prediction - Exchange Rate.csv")
    return(pred2)
     }
    """
    
    rfunc=robjects.r(rstring)
    r_df=rfunc(exchange_rate)
    #pred_exchange=pandas2ri.ri2py(r_df)
    pred_exchange=pd.read_csv('../Data Directory/Prediction - Covariates/Prediction - Exchange Rate.csv',encoding=encoding)
    exchange_rate=preparing_covariate1(exchange_rate,pred_exchange)
    
    ## Retrieving Interest Rate
    interest_rate=pd.read_csv("../Input Files/Taxa_juros.csv",encoding=encoding)
    
    rstring="""
    function(data6){
    library(forecast)
    library(imputeTS)
    list1=strsplit(data6$DATE,"/")
    length=nrow(data6)
    min_year=list1[[1]][3]
    max_year=list1[[length]][3]
    max_month=list1[[length]][1]
    max_month=as.numeric(max_month)
    max_year=as.numeric(max_year)
    min_year=as.numeric(min_year)
    data_points=12-max_month
    if(max_year==2018)
    {
      max_len=12+data_points
    }
    if(max_year==2019)
    {
      max_len=data_points
    }  
    data6$Interest_Rate<-na.interpolation(data6$Interest_Rate, option = "spline")
    interest_ts <- ts(data6[,c(2)],start=c(min_year,1),frequency=12)
    ARIMAfit <- arima(interest_ts,order=c(1,1,1),seasonal=list(order=c(1,1,1),period=12))
    pred <- forecast(ARIMAfit,h=max_len)
    write.csv(pred,"../Data Directory/Prediction - Covariates/Prediction - Interest Rate.csv")
    return(pred)
     }
    """
    
    rfunc=robjects.r(rstring)
    r_df=rfunc(interest_rate)
    #pred_interest=pandas2ri.ri2py(r_df)
    pred_interest=pd.read_csv('../Data Directory/Prediction - Covariates/Prediction - Interest Rate.csv',encoding=encoding)
    interest_rate=preparing_covariate1(interest_rate,pred_interest)
    
    ## Retrieving Milho & Soya Price and Pre-Processing
    milho_price = pd.read_html(r'../Input Files/Preco_Milho_Int.xls', encoding='utf-8')
    milho_price=preparing_covariate(milho_price)
    milho_price.to_csv("../Data Directory/Milho_International_Price.csv",index=False,encoding=encoding)
    rstring="""
    function(data80){
    library(forecast)
    library(imputeTS)
    length=nrow(data80)
    min_year=data80[1,1]
    max_year=data80[length,1]
    max_month=data80[length,2]
    min_month=data80[1,2]
    min_month=as.numeric(min_month)
    max_month=as.numeric(max_month)
    max_year=as.numeric(max_year)
    min_year=as.numeric(min_year)
    data_points=12-max_month  
    if(max_year==2018)
    {
      max_len=12+data_points
    }
    if(max_year==2019)
    {
      max_len=data_points
    }  
    start=(12-min_month)+1
    data80=tail(data80,-start)
    data80$Value <-  na.interpolation(data80$Value, option = "spline")
    milho_price_ts <- ts(data80[,c(3)],start=c(data80[1,1],1),frequency=12)
    ARIMAfit <- arima(milho_price_ts,order=c(0,1,1),seasonal=list(order=c(1,1,1),period=12))
    pred <- forecast(ARIMAfit,h=max_len)
    write.csv(pred,"../Data Directory/Prediction - Covariates/Prediction - Milho Price.csv")
    return(pred)
     }
    """
    
    rfunc=robjects.r(rstring)
    r_df=rfunc(milho_price)
    #pred_milho=pandas2ri.ri2py(r_df)
    pred_milho=pd.read_csv('../Data Directory/Prediction - Covariates/Prediction - Milho Price.csv',encoding=encoding)
    
    soya_price = pd.read_html(r'../Input Files/Preco_Soja_Int.xls', encoding='utf-8')
    soya_price=preparing_covariate(soya_price)
    soya_price.to_csv("../Data Directory/Soya_International_Price.csv",index=False,encoding=encoding)
    #r['source']('Covariate_Forecasting2.R')
    
    rstring="""
    function(data81){
    library(forecast)
    library(imputeTS)
    length=nrow(data81)
    min_year=data81[1,1]
    max_year=data81[length,1]
    max_month=data81[length,2]
    min_month=data81[1,2]
    min_month=as.numeric(min_month)
    max_month=as.numeric(max_month)
    max_year=as.numeric(max_year)
    min_year=as.numeric(min_year)
    data_points=12-max_month  
    if(max_year==2018)
    {
      max_len=12+data_points
    }
    if(max_year==2019)
    {
      max_len=data_points
    }  
    start=(12-min_month)+1
    data81=tail(data81,-start)
    data81$Value <-  na.interpolation(data81$Value, option = "spline")
    soya_price_ts <- ts(data81[,c(3)],start=c(data81[1,1],1),frequency=12)
    ARIMAfit <- arima(soya_price_ts,order=c(0,1,1),seasonal=list(order=c(1,1,1),period=12))
    pred <- forecast(ARIMAfit,h=max_len)
    write.csv(pred,"../Data Directory/Prediction - Covariates/Prediction - Soya Price.csv")
    return(pred)
     }
    """

    rfunc=robjects.r(rstring)
    r_df=rfunc(soya_price)
    #pred_soya=pandas2ri.ri2py(r_df)
    pred_soya=pd.read_csv('../Data Directory/Prediction - Covariates/Prediction - Soya Price.csv',encoding=encoding)
    
    ## Retrieving Milho and Soya Predictions   
    milho_price=preparing_covariate2(milho_price,pred_milho)
    soya_price=preparing_covariate2(soya_price,pred_soya)
 
    ## Retrieving Milho Price (more no. of predictions)
    milho2_price = pd.read_html(r'../Input Files/Preco_Milho_Int.xls', encoding='utf-8')
    milho2_price=preparing_covariate(milho2_price)
    
    rstring="""
    function(data80){
    library(forecast)
    library(imputeTS)
    length=nrow(data80)
    min_year=data80[1,1]
    max_year=data80[length,1]
    max_month=data80[length,2]
    min_month=data80[1,2]
    min_month=as.numeric(min_month)
    max_month=as.numeric(max_month)
    max_year=as.numeric(max_year)
    min_year=as.numeric(min_year)
    data_points=12-max_month  
    if(max_year==2018)
    {
      max_len=12+data_points
    }
    if(max_year==2019)
    {
      max_len=data_points
    } 
    start=(12-min_month)+1
    data80=tail(data80,-start)
    milho_price_ts <- ts(data80[,c(3)],start=c(data80[1,1],1),frequency=12)
    ARIMAfit <- arima(milho_price_ts,order=c(0,1,1),seasonal=list(order=c(1,1,1),period=12))
    pred <- forecast(ARIMAfit,h=max_len+10)
    write.csv(pred,"../Data Directory/Prediction - Covariates/Prediction - Milho Price (2).csv")
    return(pred)
     }
    """
    
    rfunc=robjects.r(rstring)
    r_df=rfunc(milho2_price)
    pred_milho2=pd.read_csv('../Data Directory/Prediction - Covariates/Prediction - Milho Price (2).csv',encoding=encoding)
    milho2_price=preparing_covariate2(milho2_price,pred_milho2)
    
    ## Retrieving Milho Price (more no. of predictions)
    soya2_price = pd.read_html(r'../Input Files/Preco_Soja_Int.xls', encoding='utf-8')
    soya2_price=preparing_covariate(soya2_price)
    
    rstring="""
    function(data81){
    library(forecast)
    library(imputeTS)
    length=nrow(data81)
      max_year=data81[length,1]
      max_month=data81[length,2]
      min_month=data81[1,2]
      min_month=as.numeric(min_month)
      max_month=as.numeric(max_month)
      max_year=as.numeric(max_year)
      data_points=12-max_month  
      if(max_year==2018)
      {
        max_len=12+data_points
      }
      if(max_year==2019)
      {
        max_len=data_points
      }  
      start=(12-min_month)+1
      data81=tail(data81,-start)
      min_year=data81[1,1]
      min_year=as.numeric(min_year)
      soya_price_ts <- ts(data81[,c(3)],start=c(min_year,1),frequency=12)
      ARIMAfit <- arima(soya_price_ts,order=c(0,1,1),seasonal=list(order=c(1,1,1),period=12))
      pred <- forecast(ARIMAfit,h=max_len+10)
      write.csv(pred,"../Data Directory/Prediction - Covariates/Prediction - Soya Price (2).csv")
      return(pred)
     }
    """
    
    rfunc=robjects.r(rstring)
    r_df=rfunc(soya2_price)
    pred_soya2=pd.read_csv('../Data Directory/Prediction - Covariates/Prediction - Soya Price (2).csv',encoding=encoding)
    soya2_price=preparing_covariate2(soya2_price,pred_soya2)
    
    print(colored('\nForecasting Covariates Completed...\n','green'))
    
    all_covariates=create_all_covariates(prod_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    all_covariates.to_csv('../Data Directory/covariates_all_crops.csv',index=False,encoding=encoding)
    
    ## Milho 1 - Getting Plantation Data Monthly
    plant_data=getting_plant_data(monthly_value,pred_year,'Milho 1 Safra')
    
    ## Creating Plantation input files for Milho1 states
    MA_plant=create_milho1_plant_input_MA(list1,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    MA_plant.to_csv(str(path)+'/Production/Data Directory/Milho1 Plantation Input Files/Milho1_MA.csv',index=False,encoding=encoding)
    
    PR_plant=create_milho1_plant_input_PR(list1,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    PR_plant.to_csv(str(path)+'/Production/Data Directory/Milho1 Plantation Input Files/Milho1_PR.csv',index=False,encoding=encoding)
    
    RS_plant=create_milho1_plant_input_RS(list1,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    RS_plant.to_csv(str(path)+'/Production/Data Directory/Milho1 Plantation Input Files/Milho1_RS.csv',index=False,encoding=encoding)
    
    SC_plant=create_milho1_plant_input_SC(list1,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    SC_plant.to_csv(str(path)+'/Production/Data Directory/Milho1 Plantation Input Files/Milho1_SC.csv',index=False,encoding=encoding)
    
    SP_plant=create_milho1_plant_input_SP(list1,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    SP_plant.to_csv(str(path)+'/Production/Data Directory/Milho1 Plantation Input Files/Milho1_SP.csv',index=False,encoding=encoding)
    
    ## Milho 2 - Getting Plantation Data Monthly
    plant_data=getting_plant_data(monthly_value,pred_year,'Milho 2 Safra')
    
    ## Creating Plantation input files for Milho2 states
    GO_plant=create_milho2_plant_input_GO(list1,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    GO_plant.to_csv(str(path)+'/Production/Data Directory/Milho2 Plantation Input Files/Milho2_GO.csv',index=False,encoding=encoding)
    
    MT_plant=create_milho2_plant_input_MT(list1,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    MT_plant.to_csv(str(path)+'/Production/Data Directory/Milho2 Plantation Input Files/Milho2_MT.csv',index=False,encoding=encoding)
    
    ## Soya - Getting Plantation Data Monthly
    plant_data=getting_plant_data(monthly_value,pred_year,'Soja')
    
    ## Creating Plantation input files for Soya states
    BA_plant=create_soya_plant_input_BA(list1,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    BA_plant.to_csv(str(path)+'/Production/Data Directory/Soya Plantation Input Files/Soya_BA.csv',index=False,encoding=encoding)
    
    GO_plant=create_soya_plant_input_GO(list1,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    GO_plant.to_csv(str(path)+'/Production/Data Directory/Soya Plantation Input Files/Soya_GO.csv',index=False,encoding=encoding)
    
    MA_plant=create_soya_plant_input_MA(list1,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    MA_plant.to_csv(str(path)+'/Production/Data Directory/Soya Plantation Input Files/Soya_MA.csv',index=False,encoding=encoding)
    
    MG_plant=create_soya_plant_input_MG(list1,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    MG_plant.to_csv(str(path)+'/Production/Data Directory/Soya Plantation Input Files/Soya_MG.csv',index=False,encoding=encoding)
    
    MS_plant=create_soya_plant_input_MS(list1,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    MS_plant.to_csv(str(path)+'/Production/Data Directory/Soya Plantation Input Files/Soya_MS.csv',index=False,encoding=encoding)
    
    MT_plant=create_soya_plant_input_MT(list1,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    MT_plant.to_csv(str(path)+'/Production/Data Directory/Soya Plantation Input Files/Soya_MT.csv',index=False,encoding=encoding)
    
    PI_plant=create_soya_plant_input_PI(list1,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    PI_plant.to_csv(str(path)+'/Production/Data Directory/Soya Plantation Input Files/Soya_PI.csv',index=False,encoding=encoding)
    
    PR_plant=create_soya_plant_input_PR(list1,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    PR_plant.to_csv(str(path)+'/Production/Data Directory/Soya Plantation Input Files/Soya_PR.csv',index=False,encoding=encoding)
    
    RO_plant=create_soya_plant_input_RO(list1,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    RO_plant.to_csv(str(path)+'/Production/Data Directory/Soya Plantation Input Files/Soya_RO.csv',index=False,encoding=encoding)
    
    RS_plant=create_soya_plant_input_RS(list1,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    RS_plant.to_csv(str(path)+'/Production/Data Directory/Soya Plantation Input Files/Soya_RS.csv',index=False,encoding=encoding)
    
    SC_plant=create_soya_plant_input_SC(list1,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    SC_plant.to_csv(str(path)+'/Production/Data Directory/Soya Plantation Input Files/Soya_SC.csv',index=False,encoding=encoding)
    
    SP_plant=create_soya_plant_input_SP(list1,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    SP_plant.to_csv(str(path)+'/Production/Data Directory/Soya Plantation Input Files/Soya_SP.csv',index=False,encoding=encoding)
    
    TO_plant=create_soya_plant_input_TO(list1,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    TO_plant.to_csv(str(path)+'/Production/Data Directory/Soya Plantation Input Files/Soya_TO.csv',index=False,encoding=encoding)
    
    print(colored('\nForecasting Plantation - Milho 2ª Safra...\n','green'))
    
    ## Forecasting all Crops
    
    r['source'](str(path)+'/Production/R Codes/Forecasting Plantation - Milho2 (GO).R')
    r['source'](str(path)+'/Production/R Codes/Forecasting Plantation - Milho2 (MT).R')
    
    print(colored('\nForecasting Plantation - Milho 1ª Safra...\n','green'))
    
    r['source'](str(path)+'/Production/R Codes/Forecasting Plantation - Milho1 (MA).R')
    r['source'](str(path)+'/Production/R Codes/Forecasting Plantation - Milho1 (PR).R')
    r['source'](str(path)+'/Production/R Codes/Forecasting Plantation - Milho1 (RS).R')
    r['source'](str(path)+'/Production/R Codes/Forecasting Plantation - Milho1 (SC).R')
    r['source'](str(path)+'/Production/R Codes/Forecasting Plantation - Milho1 (SP).R')
    
    print(colored('\nForecasting Plantation - Soya...\n','green'))
    
    r['source'](str(path)+'/Production/R Codes/Forecasting Plantation - Soya (BA).R')
    r['source'](str(path)+'/Production/R Codes/Forecasting Plantation - Soya (GO).R')
    r['source'](str(path)+'/Production/R Codes/Forecasting Plantation - Soya (MA).R')
    r['source'](str(path)+'/Production/R Codes/Forecasting Plantation - Soya (MG).R')
    r['source'](str(path)+'/Production/R Codes/Forecasting Plantation - Soya (MS).R')
    r['source'](str(path)+'/Production/R Codes/Forecasting Plantation - Soya (MT).R')
    r['source'](str(path)+'/Production/R Codes/Forecasting Plantation - Soya (PI).R')
    r['source'](str(path)+'/Production/R Codes/Forecasting Plantation - Soya (PR).R')
    r['source'](str(path)+'/Production/R Codes/Forecasting Plantation - Soya (RO).R')
    r['source'](str(path)+'/Production/R Codes/Forecasting Plantation - Soya (RS).R')
    r['source'](str(path)+'/Production/R Codes/Forecasting Plantation - Soya (SC).R')
    r['source'](str(path)+'/Production/R Codes/Forecasting Plantation - Soya (SP).R')
    r['source'](str(path)+'/Production/R Codes/Forecasting Plantation - Soya (TO).R')
    
    print(colored('\nRetrieving Plantation Predictions - Milho 2ª Safra...\n','green'))
    
    ## Getting all the plantation prediction files
    ## Plantation Outputs for Milho2
    milho2_plantation_GO=pd.read_csv(str(path)+'/Production/Data Directory/Milho2 Plantation Output/Predicted-Plantation_GO.csv',encoding=encoding)
    milho2_pred_plantation_GO=processing_plantation_output(milho2_plantation_GO)
    
    milho2_plantation_MT=pd.read_csv(str(path)+'/Production/Data Directory/Milho2 Plantation Output/Predicted-Plantation_MT.csv',encoding=encoding)
    milho2_pred_plantation_MT=processing_plantation_output(milho2_plantation_MT)
    
    print(colored('\nRetrieving  Plantation Predictions - Milho 1ª Safra...\n','green'))
    
    ## Plantation Outputs for Milho1
    milho1_plantation_MA=pd.read_csv(str(path)+'/Production/Data Directory/Milho1 Plantation Output/Predicted-Plantation_MA.csv',encoding=encoding)
    milho1_pred_plantation_MA=processing_plantation_output(milho1_plantation_MA)
    
    milho1_plantation_PR=pd.read_csv(str(path)+'/Production/Data Directory/Milho1 Plantation Output/Predicted-Plantation_PR.csv',encoding=encoding)
    milho1_pred_plantation_PR=processing_plantation_output(milho1_plantation_PR)
    
    milho1_plantation_RS=pd.read_csv(str(path)+'/Production/Data Directory/Milho1 Plantation Output/Predicted-Plantation_RS.csv',encoding=encoding)
    milho1_pred_plantation_RS=processing_plantation_output(milho1_plantation_RS)
    
    milho1_plantation_SC=pd.read_csv(str(path)+'/Production/Data Directory/Milho1 Plantation Output/Predicted-Plantation_SC.csv',encoding=encoding)
    milho1_pred_plantation_SC=processing_plantation_output(milho1_plantation_SC)
    
    milho1_plantation_SP=pd.read_csv(str(path)+'/Production/Data Directory/Milho1 Plantation Output/Predicted-Plantation_SP.csv',encoding=encoding)
    milho1_pred_plantation_SP=processing_plantation_output(milho1_plantation_SP)
    
    print(colored('\nRetrieving Plantation Predictions - Soya...\n','green'))
    
    ## Plantation Outputs for Soya
    soya_plantation_BA=pd.read_csv(str(path)+'/Production/Data Directory/Soya Plantation Output/Predicted-Plantation_BA.csv',encoding=encoding)
    soya_pred_plantation_BA=processing_plantation_output(soya_plantation_BA)
    
    soya_plantation_GO=pd.read_csv(str(path)+'/Production/Data Directory/Soya Plantation Output/Predicted-Plantation_GO.csv',encoding=encoding)
    soya_pred_plantation_GO=processing_plantation_output(soya_plantation_GO)
    
    soya_plantation_MA=pd.read_csv(str(path)+'/Production/Data Directory/Soya Plantation Output/Predicted-Plantation_MA.csv',encoding=encoding)
    soya_pred_plantation_MA=processing_plantation_output(soya_plantation_MA)
    
    soya_plantation_MG=pd.read_csv(str(path)+'/Production/Data Directory/Soya Plantation Output/Predicted-Plantation_MG.csv',encoding=encoding)
    soya_pred_plantation_MG=processing_plantation_output(soya_plantation_MG)
    
    soya_plantation_MS=pd.read_csv(str(path)+'/Production/Data Directory/Soya Plantation Output/Predicted-Plantation_MS.csv',encoding=encoding)
    soya_pred_plantation_MS=processing_plantation_output(soya_plantation_MS)
    
    soya_plantation_MT=pd.read_csv(str(path)+'/Production/Data Directory/Soya Plantation Output/Predicted-Plantation_MT.csv',encoding=encoding)
    soya_pred_plantation_MT=processing_plantation_output(soya_plantation_MT)
    
    soya_plantation_PI=pd.read_csv(str(path)+'/Production/Data Directory/Soya Plantation Output/Predicted-Plantation_PI.csv',encoding=encoding)
    soya_pred_plantation_PI=processing_plantation_output(soya_plantation_PI)
    
    soya_plantation_PR=pd.read_csv(str(path)+'/Production/Data Directory/Soya Plantation Output/Predicted-Plantation_PR.csv',encoding=encoding)
    soya_pred_plantation_PR=processing_plantation_output(soya_plantation_PR)
    
    soya_plantation_RO=pd.read_csv(str(path)+'/Production/Data Directory/Soya Plantation Output/Predicted-Plantation_RO.csv',encoding=encoding)
    soya_pred_plantation_RO=processing_plantation_output(soya_plantation_RO)
    
    soya_plantation_RS=pd.read_csv(str(path)+'/Production/Data Directory/Soya Plantation Output/Predicted-Plantation_RS.csv',encoding=encoding)
    soya_pred_plantation_RS=processing_plantation_output(soya_plantation_RS)
    
    soya_plantation_SC=pd.read_csv(str(path)+'/Production/Data Directory/Soya Plantation Output/Predicted-Plantation_SC.csv',encoding=encoding)
    soya_pred_plantation_SC=processing_plantation_output(soya_plantation_SC)
    
    soya_plantation_SP=pd.read_csv(str(path)+'/Production/Data Directory/Soya Plantation Output/Predicted-Plantation_SP.csv',encoding=encoding)
    soya_pred_plantation_SP=processing_plantation_output(soya_plantation_SP)
    
    soya_plantation_TO=pd.read_csv(str(path)+'/Production/Data Directory/Soya Plantation Output/Predicted-Plantation_TO.csv',encoding=encoding)
    soya_pred_plantation_TO=processing_plantation_output(soya_plantation_TO)
    
    print(colored('\nCreating Input Files for Production Using Plantation Predictions - Milho 2ª Safra...\n','green'))
    
    ## Milho 2 - Getting Plantation Data Monthly
    plant_data=getting_plant_data(monthly_value,pred_year,'Milho 2 Safra')
    
    ## Creating input files for Milho2 states
    GO_prod=create_milho2_input_GO(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,milho2_pred_plantation_GO,list1)
    GO_prod.to_csv(str(path)+'/Production/Data Directory/Milho2 Input Files/Milho2_GO.csv',index=False,encoding=encoding)
    
    MT_prod=create_milho2_input_MT(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,milho2_pred_plantation_MT,list1)
    MT_prod.to_csv(str(path)+'/Production/Data Directory/Milho2 Input Files/Milho2_MT.csv',index=False,encoding=encoding)
    
    MS_prod=create_milho2_input_MS(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    MS_prod.to_csv(str(path)+'/Production/Data Directory/Milho2 Input Files/Milho2_MS.csv',index=False,encoding=encoding)
    
    PR_prod=create_milho2_input_PR(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    PR_prod.to_csv(str(path)+'/Production/Data Directory/Milho2 Input Files/Milho2_PR.csv',index=False,encoding=encoding)
    
    print(colored('\nCreating Input Files for Production Using Plantation Predictions - Milho 1ª Safra...\n','green'))
    
    ## Milho 1 - Getting Plantation Data Monthly
    plant_data=getting_plant_data(monthly_value,pred_year,'Milho 1 Safra')
    
    ## Creating input files for Milho1 states
    BA_prod=create_milho1_input_BA(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    BA_prod.to_csv(str(path)+'/Production/Data Directory/Milho1 Input Files/Milho1_BA.csv',index=False,encoding=encoding)
    
    GO_prod=create_milho1_input_GO(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    GO_prod.to_csv(str(path)+'/Production/Data Directory/Milho1 Input Files/Milho1_GO.csv',index=False,encoding=encoding)
    
    MA_prod=create_milho1_input_MA(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,milho1_pred_plantation_MA,list1)
    MA_prod.to_csv(str(path)+'/Production/Data Directory/Milho1 Input Files/Milho1_MA.csv',index=False,encoding=encoding)
    
    MG_prod=create_milho1_input_MG(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price)
    MG_prod.to_csv(str(path)+'/Production/Data Directory/Milho1 Input Files/Milho1_MG.csv',index=False,encoding=encoding)
    
    PR_prod=create_milho1_input_PR(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,milho1_pred_plantation_PR,list1)
    PR_prod.to_csv(str(path)+'/Production/Data Directory/Milho1 Input Files/Milho1_PR.csv',index=False,encoding=encoding)
    
    RS_prod=create_milho1_input_RS(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,milho1_pred_plantation_RS,list1)
    RS_prod.to_csv(str(path)+'/Production/Data Directory/Milho1 Input Files/Milho1_RS.csv',index=False,encoding=encoding)
    
    SC_prod=create_milho1_input_SC(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,milho1_pred_plantation_SC,list1)
    SC_prod.to_csv(str(path)+'/Production/Data Directory/Milho1 Input Files/Milho1_SC.csv',index=False,encoding=encoding)
    
    SP_prod=create_milho1_input_SP(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,milho1_pred_plantation_SP,list1)
    SP_prod.to_csv(str(path)+'/Production/Data Directory/Milho1 Input Files/Milho1_SP.csv',index=False,encoding=encoding)
    
    print(colored('\nCreating Input Files for Production Using Plantation Predictions - Soya...\n','green'))
    
    ## Soya - Getting Plantation Data Monthly
    plant_data=getting_plant_data(monthly_value,pred_year,'Soja')
    
    ## Creating input files for Soya states
    BA_prod=create_soya_input_BA(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,soya_pred_plantation_BA,list1)
    BA_prod.to_csv(str(path)+'/Production/Data Directory/Soya Input Files/Soya_BA.csv',index=False,encoding=encoding)
    
    GO_prod=create_soya_input_GO(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,soya_pred_plantation_GO,list1)
    GO_prod.to_csv(str(path)+'/Production/Data Directory/Soya Input Files/Soya_GO.csv',index=False,encoding=encoding)
    
    MA_prod=create_soya_input_MA(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,soya_pred_plantation_MA,list1)
    MA_prod.to_csv(str(path)+'/Production/Data Directory/Soya Input Files/Soya_MA.csv',index=False,encoding=encoding)
     
    MG_prod=create_soya_input_MG(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,soya_pred_plantation_MG,list1)
    MG_prod.to_csv(str(path)+'/Production/Data Directory/Soya Input Files/Soya_MG.csv',index=False,encoding=encoding)
    
    MS_prod=create_soya_input_MS(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,soya_pred_plantation_MS,list1)
    MS_prod.to_csv(str(path)+'/Production/Data Directory/Soya Input Files/Soya_MS.csv',index=False,encoding=encoding)
    
    MT_prod=create_soya_input_MT(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,soya_pred_plantation_MT,list1)
    MT_prod.to_csv(str(path)+'/Production/Data Directory/Soya Input Files/Soya_MT.csv',index=False,encoding=encoding)
    
    PI_prod=create_soya_input_PI(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,soya_pred_plantation_PI,list1)
    PI_prod.to_csv(str(path)+'/Production/Data Directory/Soya Input Files/Soya_PI.csv',index=False,encoding=encoding)
    
    PR_prod=create_soya_input_PR(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,soya_pred_plantation_PR,list1)
    PR_prod.to_csv(str(path)+'/Production/Data Directory/Soya Input Files/Soya_PR.csv',index=False,encoding=encoding)
    
    RO_prod=create_soya_input_RO(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,soya_pred_plantation_RO,list1)
    RO_prod.to_csv(str(path)+'/Production/Data Directory/Soya Input Files/Soya_RO.csv',index=False,encoding=encoding)
    
    RS_prod=create_soya_input_RS(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,soya_pred_plantation_RS,list1)
    RS_prod.to_csv(str(path)+'/Production/Data Directory/Soya Input Files/Soya_RS.csv',index=False,encoding=encoding)
    
    SC_prod=create_soya_input_SC(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,soya_pred_plantation_SC,list1)
    SC_prod.to_csv(str(path)+'/Production/Data Directory/Soya Input Files/Soya_SC.csv',index=False,encoding=encoding)
    
    SP_prod=create_soya_input_SP(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,soya_pred_plantation_SP,list1)
    SP_prod.to_csv(str(path)+'/Production/Data Directory/Soya Input Files/Soya_SP.csv',index=False,encoding=encoding)
    
    TO_prod=create_soya_input_TO(prod_data,plant_data,exchange_rate,interest_rate,milho_price,soya_price,milho2_price,soya2_price,soya_pred_plantation_TO,list1)
    TO_prod.to_csv(str(path)+'/Production/Data Directory/Soya Input Files/Soya_TO.csv',index=False,encoding=encoding)
    
    ## Forecasting all Crops
    
    print(colored('\nForecasting Production - Milho 2ª Safra...\n','green'))
    
    r['source'](str(path)+'/Production/R Codes/Forecasting Production - GO.R')
    r['source'](str(path)+'/Production/R Codes/Forecasting Production - PR.R')
    r['source'](str(path)+'/Production/R Codes/Forecasting Production - MT.R')
    r['source'](str(path)+'/Production/R Codes/Forecasting Production - MS.R')
    
    print(colored('\nForecasting Production - Milho 1ª Safra...\n','green'))
    
    r['source'](str(path)+'/Production/R Codes/Production Forecasting - Milho1 (BA).R')
    r['source'](str(path)+'/Production/R Codes/Production Forecasting - Milho1 (GO).R')
    r['source'](str(path)+'/Production/R Codes/Production Forecasting - Milho1 (MA).R')
    r['source'](str(path)+'/Production/R Codes/Production Forecasting - Milho1 (MG).R')
    r['source'](str(path)+'/Production/R Codes/Production Forecasting - Milho1 (PR).R')
    r['source'](str(path)+'/Production/R Codes/Production Forecasting - Milho1 (RS).R')
    r['source'](str(path)+'/Production/R Codes/Production Forecasting - Milho1 (SC).R')
    r['source'](str(path)+'/Production/R Codes/Production Forecasting - Milho1 (SP).R')
    
    print(colored('\nForecasting Production - Soya...\n','green'))
    
    r['source'](str(path)+'/Production/R Codes/Production Forecasting - Soya (BA).R')
    r['source'](str(path)+'/Production/R Codes/Production Forecasting - Soya (GO).R')
    r['source'](str(path)+'/Production/R Codes/Production Forecasting - Soya (MA).R')
    r['source'](str(path)+'/Production/R Codes/Production Forecasting - Soya (MG).R')
    r['source'](str(path)+'/Production/R Codes/Production Forecasting - Soya (MS).R')
    r['source'](str(path)+'/Production/R Codes/Production Forecasting - Soya (MT).R')
    r['source'](str(path)+'/Production/R Codes/Production Forecasting - Soya (PI).R')
    r['source'](str(path)+'/Production/R Codes/Production Forecasting - Soya (PR).R')
    r['source'](str(path)+'/Production/R Codes/Production Forecasting - Soya (RO).R')
    r['source'](str(path)+'/Production/R Codes/Production Forecasting - Soya (RS).R')
    r['source'](str(path)+'/Production/R Codes/Production Forecasting - Soya (SC).R')
    r['source'](str(path)+'/Production/R Codes/Production Forecasting - Soya (SP).R')
    r['source'](str(path)+'/Production/R Codes/Production Forecasting - Soya (TO).R')
    
    print(colored('\nForecasting Production - Completed!!!\n','green'))
#     ## End of Code
#     
# =============================================================================
if __name__ == '__main__':
    main()
