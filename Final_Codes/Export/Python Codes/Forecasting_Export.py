# -*- coding: utf-8 -*-
"""
Created on Mon Mar  4 22:27:44 2019

@author: agupta466
"""
import pandas as pd
import numpy as np
import fnmatch
import datetime
from pathlib import Path
import sys
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
# os.environ['R_HOME'] = 'C:\Program Files\R\R-3.5.1' 
# os.environ['R_USER'] = 'agupta466' 
# 
# =============================================================================
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

#path = Path(__file__).parents[2]
#sys.path.append(str(p)+"/Production/Input Files")
#sys.path.append(str(p)+"/Production/Output Files")
sys.path.append(str(path)+"/Production/Python Codes")
#sys.path.append(str(path)+"/Production/Data Directory")
#sys.path.append(str(p)+"/Export/Data Directory/Soya_Bran")
import Forecasting_Production as production
from Forecasting_Production import get_state_file,preparing_covariate1,preparing_predicted_data,year_preprocessing,month_preprocessing,get_harvest_plantation_file,get_maxima,data_preprocessing_maxima,year_left_calculate,calculate_maxima_other,removing_negative_monthly

encoding='ISO-8859-15'

def get_ratio_file():
    vli_agro=pd.read_excel(str(path)+"/Production/Input Files/Prod_longo_prazo.xlsx")
    return(vli_agro)

def valid_years(year_var):
    year=list(range(2015,(int(year_var[1][4])+1)))
    return(year)

def getting_all_zones(vli_agro,year_var):
    year=valid_years(year_var)
    len_year=len(year)
    vli_agro=vli_agro.iloc[91:154]
    vli_agro=vli_agro.iloc[0:57]
    vli_agro_list=[42,43,44,46,48,49,50,51]
    vli_agro_list=vli_agro_list[:(len_year)]
    vli_agro=vli_agro.iloc[:, lambda vli_agro: vli_agro_list]
    vli_agro.columns = year
    vli_agro=vli_agro.reset_index(level=1, drop=True)
    vli_agro.index.name='Zonas'
    return(vli_agro)

def retrieving_zones(vli_agro):
    vli_agro.insert(0,'Estado',vli_agro.index.get_level_values('Zonas').str.split('_').str[0])
    vli_agro.insert(1,'Region',vli_agro.index.get_level_values('Zonas').str.split('_').str[1])
    vli_agro['Region'] = vli_agro['Region'].fillna(vli_agro['Estado'])
    return(vli_agro)

def calculating_total_zones(vli_agro,year_var):
    year_list=valid_years(year_var)
    for index,row in vli_agro.iloc[:,:].iterrows():
        for i in range(len(year_list)):
            if(row[1]==row[0]):
                vli_agro.loc[index,'Total-'+str(year_list[i])] = row[2+i]
            else:
                vli_agro.loc[index,'Total-'+str(year_list[i])] = 0
    vli_agro=vli_agro.reset_index()
    
    for index,row in vli_agro.iloc[:,0:(3+2*len(year_list))].iterrows():
        for i in range(len(year_list)):
            counter=index-1
            if(row[1]!=row[2]):
                vli_agro.loc[index,'Total-'+str(year_list[i])] = vli_agro.loc[counter,'Total-'+str(year_list[i])]
            else:
                continue
    
    counter_no=[]
    for index,row in vli_agro.iloc[:,:].iterrows():
        if index>0:
            counter=index-1
            if(vli_agro.loc[index,"Estado"]==vli_agro.loc[counter,"Estado"]):
                if(vli_agro.loc[counter,"Region"]==vli_agro.loc[counter,"Estado"]):
                    counter_no.append(counter)
            else:
                continue
        else:
            continue
     
    list1=list(range(0,57))
    zones_index=(set(list1).difference(counter_no))
    Zones_final=vli_agro.loc[zones_index]
    
    return(Zones_final)

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

def prev_years_ratios(vli_agro,year_var):
    year_list=valid_years(year_var)
    len_year_list=len(year_list)
    total_column_len=len(vli_agro.columns)
    total_year_list=list(range(year_var[1][1],year_var[1][0]))
    left_year_list= [i for i in total_year_list if i not in year_list]
    len_left_year=len(left_year_list)
    left_column_start=total_column_len-len_year_list
    base_ratio=vli_agro.iloc[:,left_column_start]
    if(len_left_year>0):       
        for index,row in vli_agro.iloc[:,:].iterrows():
            for i in range(len(left_year_list)):
                if(index==0):
                    vli_agro.insert(i+left_column_start,'Ratio-'+str(left_year_list[i]),base_ratio[index])
                else:
                    vli_agro.loc[index,'Ratio-'+str(left_year_list[i])] = base_ratio[index]
        
    vli_agro = vli_agro.fillna(0)
    return(vli_agro)

def calculating_country_to_zone_ratio(vli_agro,year_var): 
    year_list=valid_years(year_var) 
    len_year_list=len(year_list)
    total_columns=len(vli_agro.columns)
    start_total_column=total_columns-len(year_list)
    vli_agro=vli_agro.iloc[:,0:start_total_column]
    df1=pd.melt(vli_agro, id_vars=['Estado', 'Region'], var_name='Year', value_name='Export')
    total_years=df1.groupby(['Year'])['Export'].agg('sum')
    for i in range(len(year_list)):
        vli_agro['Total-'+str(year_list[i])] = total_years[i]
    
    vli_agro.iloc[:,3:] = vli_agro.iloc[:,3:].apply(pd.to_numeric, errors = 'coerce')
    
    for index,row in vli_agro.iloc[:,:].iterrows():
        for i in range(len(year_list)):
            if(row[3+len_year_list+i]!=0):
                vli_agro.loc[index,'Ratio-'+str(year_list[i])] = row[3+i]/row[3+len_year_list+i]
            else:
                vli_agro.loc[index,'Ratio-'+str(year_list[i])] = 0
    
    vli_agro=prev_years_ratios(vli_agro,year_var)
    return(vli_agro)

def get_state_code(state_name):
    if state_name=='Bahia':
        return('BA')
    if state_name[:2]=="Go":
        return('GO')
    if state_name=="Mato Grosso":
        return('MT')
    if state_name=="Mato Grosso do Sul":
        return('MS')
    if state_name=="Minas Gerais":
        return('MG')
    if state_name[:5]=="Paran":
        return('PR')
    if state_name=="Rio Grande do Sul":
        return('RS')
    if state_name[4:10]=="Paulo":
        return('SP')
    if state_name=="Santa Catarina":
        return('SC')
    if state_name[:4]=="Piau":
        return('PI')
    if state_name=="Tocantins":
        return('TO')
    if state_name[:3]=="Par":
        return('PA')
    if state_name=="Pernambuco":
        return('PE')
    if state_name[:4]=="Cear":
        return('CE')
    if state_name=="Acre":
        return('AC')
    if state_name=="Alagoas":
        return("AL")
    if state_name=="Amazonas":
        return("AM")
    if state_name[:4]=="Mara":
        return("MA")
    if state_name=="Rio de Janeiro":
        return("RJ")
    if state_name[:4]=="Rond":
        return("RO")
    else:
        return("ND")

def get_state_name(state1,state_code):
    for index,row in state1.iloc[:,:].iterrows():
        if(state_code == row[0]):
            return(row[1])  

def get_month_num(month_name):
    if month_name=="Jan":
        return(1)
    if month_name=="Feb":
        return(2)
    if month_name=="Mar":
        return(3)
    if month_name=="Apr":
        return(4)
    if month_name=="May":
        return(5)
    if month_name=="Jun":
        return(6)
    if month_name=="Jul":
        return(7)
    if month_name=="Aug":
        return(8)
    if month_name=="Sep":
        return(9)
    if month_name=="Oct":
        return(10)
    if month_name=="Nov":
        return(11)
    if month_name=="Dec":
        return(12)

def processing_export(hist_export):
    ## Converting into tonnes
    hist_export["Tonnes"]=hist_export[u'Quilograma Líquido']/1000
    hist_export[u'Código NCM']=hist_export[u'Código NCM'].astype(str)
    hist_export=hist_export[(hist_export[u'Código NCM']=='23040010') | (hist_export[u'Código NCM']=='23040090')]
    grouped_hist_export=hist_export.groupby([u'Ano', u'Mês', u'UF do Produto'], as_index=False).sum()
    grouped_hist_export=grouped_hist_export.reset_index(drop=True)
    grouped_hist_export=grouped_hist_export[[u'Ano', u'Mês', u'UF do Produto','Valor FOB (US$)','Quilograma Líquido','Tonnes']]
    return(grouped_hist_export)

def remove_columns_export(grouped_full_data):   
    print(grouped_full_data)
    for index,row in grouped_full_data.iloc[:,:].iterrows():
        grouped_full_data.loc[index,'UF'] = get_state_code(row[2])  
    
    grouped_full_data.columns=['Year', 'Month', 'UF do Produto', 'Valor FOB (US$)', 'Quilograma Líquido','Tonnes', 'UF']
    del grouped_full_data['Valor FOB (US$)']
    del grouped_full_data['Quilograma Líquido']
    grouped_full_data=grouped_full_data[['Year', 'Month', 'UF do Produto','UF','Tonnes']]           
    grouped_full_data.columns=['Year', 'Month','Estado','UF','Tonnes']
    for index,row in grouped_full_data.iloc[:,:].iterrows():
        grouped_full_data.loc[index,'Estado']=grouped_full_data.loc[index,'Estado'].upper()
    return(grouped_full_data)
  
def repeat_fun(value,length):
    a=[]
    [a.append(value) for i in range(0,length)]
    return(a)
    
def year_left_calculate(dataframe,year_ND):
    total_months=list(range(1,13))
    df=pd.DataFrame(columns=dataframe.columns)
    for i in range(len(year_ND)):
        month=list(dataframe[dataframe['Year']==year_ND[i]]['Month'])
        month_left=set(total_months).difference(month)
        code='ND'
        df1=pd.DataFrame(0,index=np.arange(len(month_left)),columns=dataframe.columns)
        df1['Month']=month_left
        df1['UF']=code
        df1['Year']=year_ND[i]
        df=pd.concat([df.reset_index(drop=True), df1.reset_index(drop=True)], axis=0)
    df=df.reset_index(drop=True)
    return(df)
    
def creating_ND(grouped_full_data,year_var):
    nd_data=grouped_full_data[grouped_full_data['UF']=='ND']
    months=list(range(1,13))
    year=list(range(year_var[1][1],(int(year_var[1][0])+1)))
    df_year_ND=list(nd_data.iloc[:,0])
    year_left_ND=set(year).difference(df_year_ND)
    year_list=[]
    month_list=[]
    [year_list.extend(repeat_fun(item,12)) for item in year_left_ND]
    year_list.sort()
    month_list.extend(repeat_fun(months,len(year_left_ND)))
    flat_month_list=[]
    for sublist in month_list:
        for item in sublist:
            flat_month_list.append(item)
    df1=pd.DataFrame(0,index=np.arange(len(months)),columns=nd_data.columns)
    df=pd.DataFrame(columns=nd_data.columns)
    for item in range(0,len(year_left_ND)):
        df=df.append(df1)
    df['Year']=year_list
    df['Month']=flat_month_list
    year_ND=list(set(year).difference(year_left_ND))
    ND_left=year_left_calculate(nd_data,year_ND)
    full_zero=pd.concat([ND_left.reset_index(drop=True),df.reset_index(drop=True)],axis=0)
    full_ND=pd.concat([nd_data.reset_index(drop=True),full_zero.reset_index(drop=True)],axis=0)
    full_ND=full_ND.sort_values(['Year','Month'])
    full_ND=full_ND.reset_index(drop=True)
    full_ND['UF']="ND"
    full_ND['Estado']='Zona Nao Declarada'
    return(full_ND)
    
def ND_split_zonewise(nd_data,year_var,zones_ratio,state1):
    nd_data=nd_data[(nd_data['Year']>=year_var[1][1])]
    df=pd.DataFrame(columns=nd_data.columns)
    all_months=len(list(range(year_var[1][1],(int(year_var[1][0])+1))))*12
    df1=pd.DataFrame(0,index=np.arange(all_months),columns=nd_data.columns)
    zones_ratio=zones_ratio.groupby(['Estado'], as_index=False).sum()
    for index,row in zones_ratio.iloc[:,:].iterrows():
        for j,rowj in nd_data.iloc[:,:].iterrows():
            df1.loc[j,"Year"]=rowj[0]
            df1.loc[j,"Month"]=rowj[1]
            df1.loc[j,"Estado"]=get_state_name(state1,row[0]).upper()
            df1.loc[j,"UF"]=row[0]
            df1.loc[j,'Tonnes'] = rowj[4]*zones_ratio.loc[index,'Ratio-'+str(rowj[0])]
        df=pd.concat([df.reset_index(drop=True), df1.reset_index(drop=True)], axis=0)
    df=df.reset_index(drop=True)
    df.iloc[:,0:2] = df.iloc[:,0:2].apply(pd.to_numeric, errors = 'coerce')
    return(df)

def add_ND_to_states(data,nd_data,year_var):
    final_data=data[(data['UF']!='ND')&(data['Year']>=year_var[1][1])]
    final_data.iloc[:,0:2] = final_data.iloc[:,0:2].apply(pd.to_numeric, errors = 'coerce')
    nd_data.iloc[:,0:2] = nd_data.iloc[:,0:2].apply(pd.to_numeric, errors = 'coerce')
    final_data.columns=['Year','Month','Estado','UF','Tonnes.1']
    nd_data=pd.merge(nd_data,final_data,on=["Year",'Month','UF','Estado'],how='outer',right_index=False,sort=True,indicator=True).query("_merge != 'right_only'").drop('_merge',axis=1)
    nd_data=nd_data.fillna(0)
    nd_data['Export']=nd_data['Tonnes']+nd_data['Tonnes.1']
    del nd_data['Tonnes']
    del nd_data['Tonnes.1']
    return(nd_data)

def calculating_argentina_prod(state1):
    argentina_prod=pd.read_excel(str(path)+"/Export/Input Files/Resumo_Pais_mundo.xls",encoding=encoding)
    argentina_harvest=pd.read_excel(str(path)+"/Export/Input Files/Colheita_Argentina.xls",encoding=encoding)
    
    argentina_prod=argentina_prod[argentina_prod["Cultura"]=="Soja"]
    argentina_harvest=argentina_harvest[(argentina_harvest["Estado"]=="Total")&(argentina_harvest["Cultura"]=="Soja")&(argentina_harvest["Fase Safra"]=="Colheita")]
    
    argentina_prod.columns=["Cultura","Country","Year","Consumo (1000 ton)","Estoques Finais (1000 ton)","Export (1000 ton)","Import (1000 ton)","Production (1000 ton)","Area Harvest (1000 ha)","Productivity (ton/ha)"]
    argentina_prod=argentina_prod[["Country","Year","Production (1000 ton)"]]
    
    pivoted_argentina=argentina_prod.pivot(index='Country',columns='Year',values="Production (1000 ton)")    
    harvest_file=get_harvest_plantation_file(str(path)+"/Production/Input Files/",state1)
    del harvest_file['19/20']
    argentina_harvest=argentina_harvest.rename(columns={'Data':'Quinzena'})
    
    for index,row in argentina_harvest.iloc[:,:].iterrows():
        argentina_harvest.loc[index,'UF'] = get_state_code(row[0])
    
    years1=year_preprocessing(pivoted_argentina.iloc[:,:])
    years_argentina=years1.columns
    
    pivoted_argentina.columns=years_argentina
    
    unpivoted_argentina=pd.melt(pivoted_argentina, var_name='Years', value_name='Value')
    unpivoted_argentina=unpivoted_argentina.reset_index(drop=True)
    unpivoted_argentina["Years"] = unpivoted_argentina["Years"].astype(int)
    unpivoted_argentina["Value"] = unpivoted_argentina["Value"].astype(float)
    
    col_list2=harvest_file.columns
    argentina_harvest=argentina_harvest[col_list2]    
    
    years2=year_preprocessing(argentina_harvest.iloc[:,5:])
    year_list_monthly=years2.columns
    
    combined_harvest = pd.concat([argentina_harvest[['UF','Cultura','Fase Safra','Quinzena']].reset_index(drop=True),years2.reset_index(drop=True)],axis=1)
    combined_harvest = month_preprocessing(combined_harvest)
    
    soya_harvest=combined_harvest[(combined_harvest[u'Cultura']=='Soja') & (combined_harvest['Fase Safra']=='Colheita')]
    soya_prod_maxima=get_maxima(soya_harvest,year_list_monthly)
    soya_prod_maxima=data_preprocessing_maxima(soya_prod_maxima)
    
    soya_other=production.year_left_calculate(soya_prod_maxima,year_list_monthly)
    soya_full=calculate_maxima_other(soya_prod_maxima,soya_other)
    soya_full.insert(0,'Produto','Soja')
    
    soya_full['Years']=soya_full['Years'].astype(int)
    soya_full['Month']=soya_full['Month'].astype(int)
    soya_full['Percentage']=soya_full['Percentage'].astype(float)
    
    merged_data=pd.merge(soya_full,unpivoted_argentina,on=['Years'],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    merged_data['Cumulative_Prod']=merged_data['Value']*merged_data['Percentage']
    merged_data=merged_data.reset_index(drop=True)
    
    ## Calculating Monthly Production from Cumulative Production
    for index,row in merged_data.iloc[:,:].iterrows():
        counter=index-1
        if (counter<0):
            merged_data.loc[index,'Monthly_Value'] = merged_data.loc[index,'Cumulative_Prod']
        if (counter>=0):
            merged_data.loc[index,'Monthly_Value'] = merged_data.loc[index,'Cumulative_Prod']-merged_data.loc[counter,'Cumulative_Prod']
        
    merged_data = removing_negative_monthly(merged_data)
    merged_data = merged_data[['Years','Month','Monthly_Value']]
    merged_data.columns = ['Ano','Mes','Argentina_Production']
    return(merged_data)
    
def create_argentina_input(prod_data,milho_price):    
    ## Argentina Prod - Data Files
    argentina_prod=pd.merge(prod_data,milho_price,on=["Ano","Mes"],how='outer',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    argentina_prod=argentina_prod.fillna(0)   
    return(argentina_prod)
    
def argentina_processing(argentina_prod):
    pred_argentina=pd.read_csv(str(path)+"/Export/Data Directory/Soya_Bran/Prediction - Covariates/Predicted-Production_Argentina.csv",encoding=encoding)
    pred_argentina.columns=['Ano','Mes','Argentina_Production']
    pred_argentina=pred_argentina[pred_argentina['Ano']==2019]
    argentina_prod=argentina_prod[(argentina_prod['Ano']<2019)]
    argentina_full=pd.concat([argentina_prod.reset_index(drop=True),pred_argentina.reset_index(drop=True)],axis=0)
    argentina_full=argentina_full.reset_index(drop=True)
    return(argentina_full)

def Replace(str1): 
    str1 = str1.replace(',','third') 
    str1 = str1.replace('.',',') 
    str1 = str1.replace('third','.') 
    return(str1)

def Replace_comma(str1):
    return(float(str1.replace(',','')))

def try_cutoff(x):
    try:
        a=round(float(x), 3)
        return format(a, '.3f')
    except Exception:
        return x

def soya_prod_processing(soya_prod):    
    
    soya_prod_actual=soya_prod[(soya_prod['Tipo']=='Real') & (soya_prod['Produto']=='Soja')]
    soya_prod_actual=soya_prod_actual.reset_index(drop=True)
    grouped_soya_actual=soya_prod_actual.groupby([u'Ano', u'Mês', u'UF','Estado'], as_index=False).sum()
    grouped_soya_actual=grouped_soya_actual[['Ano',u'Mês','Estado','UF',u'Produção Med (Kt)']]
    
    soya_prod_pred=soya_prod[(soya_prod['Tipo']=='Prevista') & (soya_prod['Produto']=='Soja') & (soya_prod[u'% Confiança']==70)]
    grouped_soya_pred=soya_prod_pred.groupby(['Ano',u'Mês','Estado','UF'], as_index=False).sum()
    grouped_soya_pred=grouped_soya_pred[['Ano',u'Mês','Estado','UF',u'Produção Med (Kt)']]
    
    soya_full=pd.concat([grouped_soya_actual.reset_index(drop=True),grouped_soya_pred.reset_index(drop=True)],axis=0)
    soya_full.columns=['Ano',u'Mes','Estado','UF.1',u'Produção Med (Kt)']
    
    return(soya_full)

def concat_actual_pred(data,pred_data):
    df=pd.concat([data.reset_index(drop=True),pred_data.reset_index(drop=True)],axis=0)
    df=df.reset_index(drop=True)
    return(df)

def read_all_files(files,path):
    df = pd.DataFrame()
    df = pd.concat([pd.read_csv(str(path)+str(f)) for f in files])
    df = df.reset_index(drop=True)
    return(df)

def add_state_code(df,files,code_type):
    if(code_type=='biodiesel'):
        state_code_list=[str(name)[-7:-5] for name in files]
    else:
        state_code_list=[str(name)[-6:-4] for name in files]
        
    len_one_file=len(df)/len(state_code_list)    
    complete_list_code=[state_code for state_code in state_code_list for i in list(range(int(len_one_file)))]
    return(complete_list_code)
    
def biodiesel_processing(biodiesel_type):
    p=str(path)+'/Export/Data Directory/Soya_Bran/Prediction - Covariates/'
    all_files=os.listdir(p)
    if(biodiesel_type=='prod'):
        biodiesel=pd.read_csv(str(path)+'/Export/Input Files/Producao_Biodiesel.csv',encoding=encoding)
        biodiesel_files=fnmatch.filter(all_files, "*Prediction - Biodiesel_Production*")
        
    if(biodiesel_type=='consump'):
        biodiesel=pd.read_csv(str(path)+'/Export/Input Files/Consumo_Biodiesel.csv',encoding=encoding)
        biodiesel_files=fnmatch.filter(all_files, "*Prediction - Biodiesel_Consumption*")
        
    biodiesel.iloc[:,0:2] = biodiesel.iloc[:,0:2].apply(pd.to_numeric, errors = 'coerce')    
    biodiesel.columns=['Ano','Mes','UF','Value']    
    df = read_all_files(biodiesel_files,p)
    
    df = preparing_predicted_data(df)
    df['Mes']=df['Mes'].astype(int)
    df['Ano']=df['Ano'].astype(int)
    
    complete_list_code=add_state_code(df,biodiesel_files,'biodiesel')
    df.insert(2,'UF',complete_list_code)
    biodiesel=concat_actual_pred(biodiesel,df) 
  
    return(biodiesel)
    
def CBOT_processing(CBOT_type):
    if(CBOT_type=='soya'):
        CBOT_price=pd.read_csv(str(path)+'/Export/Input Files/CBOT_Soja.csv',error_bad_lines=False,skiprows=12)
        CBOT_price['Ano'] = CBOT_price['date'].str.split('-').str[0]
        CBOT_price['Mes'] = CBOT_price['date'].str.split('-').str[1]
        print(CBOT_price)
    if(CBOT_type=='soyameal'):
        CBOT_price=pd.read_csv(str(path)+'/Export/Input Files/CBOT_Farelo.csv',error_bad_lines=False,skiprows=15)
        CBOT_price['Ano'] = CBOT_price['date'].str.split('-').str[0]
        CBOT_price['Mes'] = CBOT_price['date'].str.split('-').str[1]
    if(CBOT_type=='milho'):
        CBOT_price=pd.read_csv(str(path)+'/Export/Input Files/CBOT_Milho.csv',error_bad_lines=False,skiprows=14)
        CBOT_price['Ano'] = CBOT_price['date'].str.split('-').str[0]
        CBOT_price['Mes'] = CBOT_price['date'].str.split('-').str[1]
        
    CBOT_price['Ano']=CBOT_price['Ano'].astype(int)
    CBOT_price['Mes']=CBOT_price['Mes'].astype(int)
    
    CBOT_price=CBOT_price[['Ano','Mes',u' value']]
    CBOT_price_grouped=CBOT_price.groupby([u'Ano','Mes'], as_index=False).mean()
    CBOT_price_grouped.columns=['Ano','Mes','Value']
    
    return(CBOT_price_grouped)
    
def CBOT_pred_processing(CBOT_price_grouped,CBOT_type):
    #max_month=year_var[1][2]
    if(CBOT_type=='soya'):               
# =============================================================================
#         rstring="""
#         function(data,max_month,path){
#         library(forecast)
#         library(imputeTS)
#         path3<-paste0(path,"/Export/Data Directory/Soya_Bran/Prediction - Covariates/")
#         max_len=12-max_month
#         data$Value <-  na.interpolation(data$Value, option = "spline")
#         del_list=c()
#         if(data[1,2]>1)
#         {
#           no_to_del=(12-data[1,2])+1
#           del_list=seq(1,no_to_del)
#           data<-data[-del_list,]
#         }
#         rownames(data) <- seq(length=nrow(data)) 
#         count_ts <- ts(data[,c(3)],start=c(data[1,1],1),frequency=12)
#         ARIMAfit <- arima(count_ts,order=c(0,1,1),seasonal=list(order=c(1,1,1),period=12))
#         pred <- forecast(ARIMAfit,h=max_len)  
#         setwd(path3)
#         write.csv(pred,"Prediction - Soya futures.csv")
#          }
#         """       
# =============================================================================
        rstring="""
        function(data80,path){
        library(forecast)
        library(imputeTS)
        path3<-paste0(path,"/Export/Data Directory/Soya_Bran/Prediction - Covariates/")
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
        soya_future_ts <- ts(data80[,c(3)],start=c(data80[1,1],1),frequency=12)
        ARIMAfit <- arima(soya_future_ts,order=c(0,1,1),seasonal=list(order=c(1,1,1),period=12))
        pred <- forecast(ARIMAfit,h=max_len)
        setwd(path3)
        write.csv(pred,"Prediction - Soya futures.csv")
        return(pred)
         }
        """
        rfunc=robjects.r(rstring) 
        r_df=rfunc(CBOT_price_grouped,str(path))
        #r_df=rfunc(CBOT_price_grouped,max_month,str(path))
        pred_CBOT=pd.read_csv(str(path)+'/Export/Data Directory/Soya_Bran/Prediction - Covariates/Prediction - Soya futures.csv',encoding=encoding)
       
    if(CBOT_type=='soyameal'):
# =============================================================================
#         rstring="""
#         function(data,max_month,path){
#         library(forecast)
#         library(imputeTS)
#         path3<-paste0(path,"/Export/Data Directory/Soya_Bran/Prediction - Covariates/")
#         max_len=12-max_month
#         data$Value <-  na.interpolation(data$Value, option = "spline")
#         del_list=c()
#         if(data[1,2]>1)
#         {
#           no_to_del=(12-data[1,2])+1
#           del_list=seq(1,no_to_del)
#           data<-data[-del_list,]
#         }
#         rownames(data) <- seq(length=nrow(data)) 
#         count_ts <- ts(data[,c(3)],start=c(data[1,1],1),frequency=12)
#         ARIMAfit <- arima(count_ts,order=c(0,1,1),seasonal=list(order=c(1,1,1),period=12))
#         pred <- forecast(ARIMAfit,h=max_len)  
#         setwd(path3)
#         write.csv(pred,"Prediction - Soya Meal futures.csv")
#          }
#         """ 
# =============================================================================
        rstring="""
        function(data80,path){
        library(forecast)
        library(imputeTS)
        path3<-paste0(path,"/Export/Data Directory/Soya_Bran/Prediction - Covariates/")
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
        soyameal_future_ts <- ts(data80[,c(3)],start=c(data80[1,1],1),frequency=12)
        ARIMAfit <- arima(soyameal_future_ts,order=c(0,1,1),seasonal=list(order=c(1,1,1),period=12))
        pred <- forecast(ARIMAfit,h=max_len)
        setwd(path3)
        write.csv(pred,"Prediction - Soya Meal futures.csv")
        return(pred)
         }
        """
        rfunc=robjects.r(rstring) 
        r_df=rfunc(CBOT_price_grouped,str(path))
        #r_df=rfunc(CBOT_price_grouped,max_month,str(path))
        pred_CBOT=pd.read_csv(str(path)+'/Export/Data Directory/Soya_Bran/Prediction - Covariates/Prediction - Soya Meal futures.csv',encoding=encoding)
    
    if(CBOT_type=='milho'):               
# =============================================================================
#         rstring="""
#         function(data,max_month,path){
#         library(forecast)
#         library(imputeTS)
#         path3<-paste0(path,"/Export/Data Directory/Soya_Bran/Prediction - Covariates/")
#         max_len=12-max_month
#         data$Value <-  na.interpolation(data$Value, option = "spline")
#         del_list=c()
#         if(data[1,2]>1)
#         {
#           no_to_del=(12-data[1,2])+1
#           del_list=seq(1,no_to_del)
#           data<-data[-del_list,]
#         }
#         rownames(data) <- seq(length=nrow(data)) 
#         count_ts <- ts(data[,c(3)],start=c(data[1,1],1),frequency=12)
#         ARIMAfit <- arima(count_ts,order=c(1,1,0),seasonal=list(order=c(1,1,1),period=12))
#         pred <- forecast(ARIMAfit,h=max_len)  
#         setwd(path3)
#         write.csv(pred,"Prediction - Milho futures.csv")
#          }
#         """
# =============================================================================
        rstring="""
        function(data80,path){
        library(forecast)
        library(imputeTS)
        path3<-paste0(path,"/Export/Data Directory/Soya_Bran/Prediction - Covariates/")
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
        milho_future_ts <- ts(data80[,c(3)],start=c(data80[1,1],1),frequency=12)
        ARIMAfit <- arima(milho_future_ts,order=c(0,1,1),seasonal=list(order=c(1,1,1),period=12))
        pred <- forecast(ARIMAfit,h=max_len)
        setwd(path3)
        write.csv(pred,"Prediction - Milho futures.csv")
        return(pred)
         }
        """
        rfunc=robjects.r(rstring) 
        r_df=rfunc(CBOT_price_grouped,str(path))
        #r_df=rfunc(CBOT_price_grouped,max_month,str(path))
        pred_CBOT=pd.read_csv(str(path)+'/Export/Data Directory/Soya_Bran/Prediction - Covariates/Prediction - Milho futures.csv',encoding=encoding)
        
    pred_CBOT = preparing_predicted_data(pred_CBOT)
    CBOT_price=concat_actual_pred(CBOT_price_grouped,pred_CBOT)
    CBOT_price=CBOT_price.reset_index(drop=True)
    
    CBOT_price['Ano']=CBOT_price['Ano'].astype(int)
    CBOT_price['Mes']=CBOT_price['Mes'].astype(int)
    
    return(CBOT_price)
     
def merge_covariate(final_data,cov_data):
    final_data=pd.merge(final_data,cov_data,left_on=["Year",u'Month'],right_on=["Ano","Mes"],how='outer',right_index=False,sort=True,indicator=True).query("_merge != 'right_only'").drop('_merge',axis=1)
    del final_data['Ano']
    del final_data['Mes']
    return(final_data)
    
def merge_covariate2(final_data,cov_data):
    final_data=pd.merge(final_data,cov_data,left_on=["Year",u'Month','UF'],right_on=["Ano","Mes",'UF.1'],how='outer',right_index=False,sort=True,indicator=True).query("_merge != 'right_only'").drop('_merge',axis=1)
    del final_data['Ano']
    del final_data['Mes']
    del final_data['UF.1']
    return(final_data)
 
def change_date_to_month(df):
    df.insert(0,'Year',df['Date'].str.split('/').str[2])
    df.insert(1,'Month',df['Date'].str.split('/').str[0])   
    df['Year']=df['Year'].astype(int)
    df['Month']=df['Month'].astype(int)
    del df['Date']
    return(df)
    
def retrieving_export_predictions():   
    # Getting Results from Export Predictions
    p=str(path)+'/Export/Data Directory/Soya_Bran/Predicted-Export/'
    all_files=os.listdir(p)
    export_predict_files = fnmatch.filter(all_files, "*Predicted-Export*")
    df = read_all_files(export_predict_files,p)
    
    df=change_date_to_month(df)    
    complete_list_code=add_state_code(df,export_predict_files,'predicted_export')
    df.insert(2,'UF',complete_list_code)
    return(df)

def retrieving_actual_export(actual_data,year_var):
    argentina_prod=pd.read_csv(str(path)+'/Export/Data Directory/Soya_Bran/covariates_results/Argentina_Prod.csv',encoding=encoding)   
    max_year=year_var[1][0]
    min_year=argentina_prod['Ano'][0]
    max_month=year_var[1][2]
    actual_data1 = actual_data[(actual_data['Year']>=min_year)&(actual_data['Year']<max_year)]
    actual_data2 = actual_data[(actual_data['Year']==max_year)&(actual_data['Month']<=max_month)]
    final_actual = pd.concat([actual_data1.reset_index(drop=True),actual_data2.reset_index(drop=True)],axis=0)
    final_actual = final_actual.reset_index(drop=True)
    return(final_actual)
    
def retrieving_predicted_export(pred_data,year_var):
    max_year=year_var[1][0]
    max_month=year_var[1][2]
    pred_data1 = pred_data[(pred_data['Year']==max_year)&(pred_data['Month']>max_month)]
    pred_data2 = pred_data[(pred_data['Year']>max_year)]
    pred_full = pd.concat([pred_data1.reset_index(drop=True),pred_data2.reset_index(drop=True)],axis=0)
    pred_full = pred_full.reset_index(drop=True)
    return(pred_full)

def preprocessing_state_to_zone(state_to_zone):
    #state_to_zone = pd.read_csv(str(path)+'/Export/Data Directory/Soya_Bran/State_to_Zone_Ratio.csv',encoding=encoding)
    ratio_columns = [col for col in state_to_zone.columns if 'Ratio' in col]
    new_columns = [names.split('-')[1] for names in ratio_columns]
    all_ratios = state_to_zone[ratio_columns]
    all_ratios.columns=new_columns
    zones_data = state_to_zone.iloc[:,0:2]
    zones_data['Zonas']=zones_data['Zonas'].str.upper()
    complete_data = pd.concat([zones_data,all_ratios],axis=1)
    unpivoted_complete_data=pd.melt(complete_data, id_vars=['Zonas','Estado'], var_name='Year', value_name='Ratio') 
    unpivoted_complete_data['Year']=unpivoted_complete_data['Year'].astype(int)
    unpivoted_complete_data.columns=['Zonas','UF','Year','Ratio']
    return(unpivoted_complete_data)

def calculate_state_to_zone(data,zone_ratio,calculation_type):
    if(calculation_type=='pred'):
        data.columns=['Year','Month','UF','Export']
        data=pd.merge(data,zone_ratio,on=["UF","Year"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
        data['Zones_Export']=data['Export']*data['Ratio']
        data=data[['Year','Month','UF','Zonas','Zones_Export']]
        return(data)
    if(calculation_type=='confidence'):
        data.columns=['Year','Month','UF','Lo.70','Hi.70','Lo.75','Hi.75','Lo.80','Hi.80','Lo.85','Hi.85','Lo.90','Hi.90','Lo.95','Hi.95']
        data=pd.merge(data,zone_ratio,on=["UF","Year"],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
        data1=data[['Year','Month','UF','Zonas','Ratio','Lo.70','Lo.75','Lo.80','Lo.85','Lo.90','Lo.95']]
        data1.columns=['Year','Month','UF','Zonas','Ratio','70','75','80','85','90','95']
        data2=data[['Year','Month','UF','Zonas','Ratio','Hi.70','Hi.75','Hi.80','Hi.85','Hi.90','Hi.95']]
        data2.columns=['Year','Month','UF','Zonas','Ratio','70','75','80','85','90','95']
        unpivoted_data1=pd.melt(data1, id_vars=['Year','Month','UF','Zonas','Ratio'], var_name='Conf', value_name='Low Conf')
        unpivoted_data2=pd.melt(data2, id_vars=['Year','Month','UF','Zonas','Ratio'], var_name='Conf', value_name='High Conf')
        unpivoted_data=pd.merge(unpivoted_data1,unpivoted_data2,on=['Year','Month','UF','Zonas','Ratio','Conf'],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
        unpivoted_data['Low']=unpivoted_data['Low Conf']*unpivoted_data['Ratio']
        unpivoted_data['High']=unpivoted_data['High Conf']*unpivoted_data['Ratio']
        del unpivoted_data['Low Conf']
        del unpivoted_data['High Conf']
        del unpivoted_data['Ratio']
        unpivoted_data=unpivoted_data[['Year','Month','UF','Zonas','Conf','Low','High']]
        unpivoted_data=unpivoted_data.reset_index(drop=True)        
        return(unpivoted_data)

def non_predicted_states(actual_data,pred_data,year_var):
    all_states=actual_data['UF'].unique()
    model_states=pred_data['UF'].unique()
    non_model_state_list=list(set(all_states).difference(model_states))
    max_year=year_var[1][0]
    max_month=year_var[1][2]
    non_model_states=actual_data[actual_data['UF'].isin(non_model_state_list)]
    non_model_states1 = non_model_states[(non_model_states['Year']==max_year)&(non_model_states['Month']>max_month)]
    non_model_states2 = non_model_states[(non_model_states['Year']>max_year)]
    non_model_states=pd.concat([non_model_states1.reset_index(drop=True),non_model_states2.reset_index(drop=True)],axis=0)
    non_model_states=non_model_states.reset_index(drop=True)
    non_model_states['Year']=non_model_states['Year'].astype(int)
    non_model_states['Month']=non_model_states['Month'].astype(int)
    return(non_model_states)

def merge_all_states(actual_zonewise,fitted_zonewise,predicted_zonewise,zonewise_non_model,fitted_non_model_zonewise):
    final_export = pd.concat([actual_zonewise.reset_index(drop=True),fitted_zonewise.reset_index(drop=True)],axis=0)
    final_export = pd.concat([final_export.reset_index(drop=True),fitted_non_model_zonewise.reset_index(drop=True)],axis=0)
    final_export = pd.concat([final_export.reset_index(drop=True),predicted_zonewise.reset_index(drop=True)],axis=0)
    final_export = pd.concat([final_export.reset_index(drop=True),zonewise_non_model.reset_index(drop=True)],axis=0)
    final_export=final_export.reset_index(drop=True)
    return(final_export)

def retrieving_confidence(p,forecast_type):
    # Getting Results from Export Predictions
    #p=str(path)+'/Export/Data Directory/Soya_Bran/Confidence-Results/'
    all_files=os.listdir(p)
    confidence_predict_files = fnmatch.filter(all_files, "*Confidence*")
    df = read_all_files(confidence_predict_files,p)

    df.insert(0,'Ano',df['Year'].str.split(' ').str[1])
    df.insert(1,'Mes',df['Year'].str.split(' ').str[0])  
    df.insert(2,'Month_Num',df['Mes'].apply(lambda x: get_month_num(x.strip())))
    del df['Mes']
    del df['Year']
    if(forecast_type=='exp'):
        del df['Point.Forecast']
    df['Ano']=df['Ano'].astype(int)
    df['Month_Num']=df['Month_Num'].astype(int)
    complete_list_code=add_state_code(df,confidence_predict_files,'confidence_export')
    df.insert(2,'UF',complete_list_code)   
    return(df)

def add_confidence(zonewise_data):
    zonewise_data['Conf']=0
    zonewise_data['Low']=0
    zonewise_data['High']=0
    return(zonewise_data)

def add_confidence2(zonewise_data):
    zonewise_data['70'],zonewise_data['75'],zonewise_data['80'],zonewise_data['85'],zonewise_data['90'],zonewise_data['95']=0,0,0,0,0,0
    unpivoted_zonewise=pd.melt(zonewise_data, id_vars=['Year','Month','UF','Zonas','Zones_Export'], var_name='Conf', value_name='Low')
    unpivoted_zonewise['High']=0
    return(unpivoted_zonewise)

def convert_thousand_tonnes(zonewise_data):
    zonewise_data['Low']=zonewise_data['Low']/1000
    zonewise_data['Zones_Export']=zonewise_data['Zones_Export']/1000
    zonewise_data['High']=zonewise_data['High']/1000
    return(zonewise_data)

def get_date():
    now = datetime.datetime.now()
    if len(str(now.month))==1:
        date=str(now.year)+"0"+str(now.month)+""+str(now.day)
    else:
        date=str(now.year)+""+str(now.month)+""+str(now.day)
    
    return(date)
  
def main():
    
    init()
    print(colored('\nSoya Bran Export Forecasting Starts...\n', 'green'))
    
    year_var=pd.read_csv(str(path)+'/Production/Data Directory/year_variables.csv',encoding=encoding,header=None)
    
    actual_year=year_var[1][0]
    year_var[1][0]=year_var[1][4]
    print(year_var[1][2])
    
    print(colored('\nRetrieving ComexStat Input Files...\n','green'))
    
    state1=get_state_file(str(path)+"/Production/Input Files/")    
    hist_export=pd.ExcelFile("../Input Files/COMEX_2010_2018_20190211.xlsx",encoding=encoding)
    new3_export=pd.ExcelFile("../Input Files/EXP_2019_2019_20190414_v4.xlsx",encoding=encoding)
    
    hist_export=pd.read_excel(hist_export,"Resultado")
    new3_export=pd.read_excel(new3_export,'Resultado')
    
    print(colored('\nPreprocessing of ComexStat Input Files...\n','green'))
    
    grouped_hist_export=processing_export(hist_export)
    print(grouped_hist_export)
    grouped_new3_export=processing_export(new3_export)
    print(grouped_new3_export)
    
    grouped_new3_export=grouped_new3_export[(grouped_new3_export['Ano']==year_var[1][0])&(grouped_new3_export[u'Mês']<=year_var[1][2])]
    print(grouped_new3_export)
    
    grouped_full_data=pd.concat([grouped_hist_export.reset_index(drop=True), grouped_new3_export.reset_index(drop=True)], axis=0)
    grouped_full_data=grouped_full_data.reset_index(drop=True)
    
    print(colored('\nPreprocessing of ComexStat Input Files Completed!...\n','green'))
    
    final_data=remove_columns_export(grouped_full_data)
    final_data.to_csv("../Data Directory/Soya_Bran/Final_Comex_Stat.csv",encoding=encoding,index=False)
    
    print(colored('\nSoya Bran - Reading Zone Wise Export File...\n','green'))
    
    ratio_file=get_ratio_file()
    zones=getting_all_zones(ratio_file,year_var)
    all_zones=retrieving_zones(zones)
       
    final_zones=calculating_total_zones(all_zones,year_var)
    final_zones.to_csv("../Data Directory/Soya_Bran/Zones_values.csv",encoding=encoding,index=False)
    
    country_ratio=calculating_country_to_zone_ratio(final_zones,year_var)
    country_ratio.to_csv('../Data Directory/Soya_Bran/Country_Zone_Ratio.csv',encoding=encoding,index=False)
    
    print(colored('\nSoya Bran - Calculating Brazil State to Zone Ratios...\n','green'))
    
    zones_ratio=calculating_zones_ratio(final_zones,year_var)
    zones_ratio.to_csv('../Data Directory/Soya_Bran/State_to_Zone_Ratio.csv',encoding=encoding,index=False)
    
    print(colored('\nSoya Bran - Conversion of Brazil State to Zone Ratios Completed!...\n','green'))
    print(colored('\nSoya Bran - Distributing Não Declarada (ND) Values into Zones Using Export Volumns v/s Total Brazil Export - Started... \n','green'))
    
    nd_data=creating_ND(final_data,year_var)
    final_nd_converted=ND_split_zonewise(nd_data,year_var,country_ratio,state1)
    
    print(colored('\nSoya Bran - Distribution of Não Declarada (ND) into Zones Completed!!! \n','green'))
    
    final_nd_converted.to_csv('../Data Directory/Soya_Bran/nd_zone_division.csv',encoding=encoding,index=False)
    final_data=add_ND_to_states(final_data,final_nd_converted,year_var)
    
    print(colored('\nSoya Bran - Adding Não Declarada (ND) Export Zone-wise Values to Actual Export Zone-wise Values Completed!!! \n','green'))
    print(colored('\nForecasting - Milho Futures... \n','green'))
    
    ## Predicting Milho Future - Argentina Prediction
    milho_CBOT=CBOT_processing('milho')
    milho_CBOT=CBOT_pred_processing(milho_CBOT,'milho')
    milho_CBOT.columns=['Ano','Mes','CBOT_Milho']
    milho_CBOT.to_csv(str(path)+'/Export/Data Directory/Soya_Bran/covariates_results/milho_futures.csv',encoding=encoding,index=False)
    
    print(colored('\nForecasting - Milho Futures Completed!... \n','green'))
    print(colored('\nForecasting - Argentina Soya Production Using Milho Futures as Covariate... \n','green'))
    
    ## Creating Argentina Soya Production - Monthly
    argentina_prod=calculating_argentina_prod(state1)
    argentina_prod.to_csv(str(path)+'/Export/Data Directory/Soya_Bran/argentina_prod_value.csv',index=False,encoding=encoding)
    
    ## Creating Input for Argentina Soya Production 
    argentina_prod=pd.read_csv(str(path)+'/Export/Data Directory/Soya_Bran/argentina_prod_value.csv',encoding=encoding)
    argentina_input=create_argentina_input(argentina_prod,milho_CBOT)
    argentina_input.to_csv(str(path)+'/Export/Data Directory/Soya_Bran/Argentina_Prod_Input.csv',encoding=encoding,index=False)
    
    ##Forecasting Argentina Soya Production
    r['source'](str(path)+'/Export/R Codes/Forecasting - Argentina Soya Production.R') 
    
    print(colored('\nForecasting - Argentina Soya Production Completed!... \n','green'))
    
    ## Merging Argentina Production to final input data
    argentina_prod=argentina_processing(argentina_prod)
    argentina_prod.to_csv(str(path)+'/Export/Data Directory/Soya_Bran/covariates_results/Argentina_Prod.csv',encoding=encoding,index=False)
    final_data=merge_covariate(final_data,argentina_prod)
    final_data.columns=['Year', 'Month', 'Estado','UF','Tonnes','Argentina_Production']
    
    print(colored('\nRetrieving Actual & Forecasted Exchange Rate... \n','green'))
    
    ## Merging Exchange Rate to final input data
    exchange_rate=pd.read_csv(str(path)+"/Production/Input Files/Cambio_Real.csv",encoding=encoding)
    exchange_rate["Year"]= exchange_rate["DATE"].str.split("-").str[0]
    exchange_rate['Month']=exchange_rate['DATE'].str.split("-").str[1]
    del exchange_rate['DATE']
    exchange_rate.insert(0,'DATE',exchange_rate['Month'].map(str)+"/1/"+exchange_rate['Year'].map(str))
    del exchange_rate['Year']
    del exchange_rate['Month']
    pred_exchange=pd.read_csv(str(path)+'/Production/Data Directory/Prediction - Covariates/Prediction - Exchange Rate.csv',encoding=encoding)
    exchange_rate=preparing_covariate1(exchange_rate,pred_exchange)
    final_data=merge_covariate(final_data,exchange_rate)
    final_data.columns=['Year', 'Month', 'Estado','UF','Tonnes','Argentina_Production', 'Exchange_Rate']
    
    print(colored('\nRetrieving Actual & Forecasted Interest Rate... \n','green'))
    
    ## Merging Interest Rate to final input data
    interest_rate=pd.read_csv(str(path)+"/Production/Input Files/Taxa_juros.csv",encoding=encoding)
    pred_interest=pd.read_csv(str(path)+'/Production/Data Directory/Prediction - Covariates/Prediction - Interest Rate.csv',encoding=encoding)
    interest_rate=preparing_covariate1(interest_rate,pred_interest)
    final_data=merge_covariate(final_data,interest_rate)
    final_data.columns=['Year', 'Month', 'Estado','UF','Tonnes','Argentina_Production', 'Exchange_Rate','Interest_Rate']
    
    print(colored('\nRetrieving Actual & Forecasted Soya Production State-Wise... \n','green'))
    
    ## Merging Soya Production to final input data
    soya_prod=pd.read_csv(str(path)+'/Production/Output Files/'+u'Produção.csv',encoding=encoding,engine='python')
    soya_prod=soya_prod_processing(soya_prod)
    soya_prod.to_csv(str(path)+'/Export/Data Directory/Soya_Bran/covariates_results/Soya_Statewise_Prod.csv',encoding=encoding,index=False)
    soya_prod.iloc[:,0:2] = soya_prod.iloc[:,0:2].apply(pd.to_numeric, errors = 'coerce')
    soya_prod['UF.1']=soya_prod['UF.1'].astype(str)
    final_data.iloc[:,0:2] = final_data.iloc[:,0:2].apply(pd.to_numeric, errors = 'coerce')
    final_data['UF']=final_data['UF'].astype(str)
    del soya_prod['Estado']
    final_data=merge_covariate2(final_data,soya_prod)
    final_data.columns=['Year', 'Month', 'Estado','UF','Tonnes','Argentina_Production', 'Exchange_Rate','Interest_Rate','Soya_Prod']
    
    print(colored('\nForecasting - Biodiesel Production & Consumption... \n','green'))
    
    ##Forecasting Biodiesel Production & Consumption
    r['source'](str(path)+'/Export/R Codes/Covariate Forecasting - Biodiesel.R') 
    
    print(colored('\nForecasting - Biodiesel Production & Consumption Completed!... \n','green'))
    
    ## Merging Biodiesel Production to final input data
    biodiesel_prod=biodiesel_processing("prod")
    biodiesel_prod.columns=['Ano','Mes','UF.1','Biodiesel_Prod']
    biodiesel_prod['UF.1']=biodiesel_prod['UF.1'].astype(str)    
    final_data=merge_covariate2(final_data,biodiesel_prod)
    final_data.columns=['Year', 'Month', 'Estado','UF','Tonnes','Argentina_Production', 'Exchange_Rate','Interest_Rate','Soya_Prod','Biodiesel_Prod']
    
    ## Merging Biodiesel Consumption to final input data
    biodiesel_consump=biodiesel_processing("consump")
    biodiesel_consump.columns=['Ano','Mes','UF.1','Biodiesel_Consump']
    biodiesel_consump['UF.1']=biodiesel_consump['UF.1'].astype(str)    
    final_data=merge_covariate2(final_data,biodiesel_consump)
    final_data.columns=['Year', 'Month', 'Estado','UF','Tonnes','Argentina_Production', 'Exchange_Rate','Interest_Rate','Soya_Prod','Biodiesel_Prod','Biodiesel_Consump']
    
    print(colored('\nForecasting - Soya Futures... \n','green'))
    
    ## Merging Soya CBOT to final input data
    soya_CBOT=CBOT_processing('soya')
    soya_CBOT=CBOT_pred_processing(soya_CBOT,'soya')
    soya_CBOT.columns=['Ano','Mes','CBOT_Soya']
    soya_CBOT.to_csv(str(path)+'/Export/Data Directory/Soya_Bran/covariates_results/soya_futures.csv',encoding=encoding,index=False)
    final_data=merge_covariate(final_data,soya_CBOT)
    final_data.columns=['Year', 'Month', 'Estado','UF','Tonnes','Argentina_Production', 'Exchange_Rate','Interest_Rate','Soya_Prod','Biodiesel_Prod','Biodiesel_Consump','CBOT_Soya']
    
    print(colored('\nForecasting - Soya Futures Completed!... \n','green'))
    print(colored('\nForecasting - Soya Meal Futures... \n','green'))
    
    ## Merging Soyameal CBOT to final input data
    soyameal_CBOT=CBOT_processing('soyameal') 
    soyameal_CBOT=CBOT_pred_processing(soyameal_CBOT,'soya')
    soyameal_CBOT.columns=['Ano','Mes','CBOT_Soyameal']
    soyameal_CBOT.to_csv(str(path)+'/Export/Data Directory/Soya_Bran/covariates_results/soyameal_futures.csv',encoding=encoding,index=False)
    final_data=merge_covariate(final_data,soyameal_CBOT)
    final_data.columns=['Year', 'Month', 'Estado','UF','Tonnes','Argentina_Production', 'Exchange_Rate','Interest_Rate','Soya_Prod','Biodiesel_Prod','Biodiesel_Consump','CBOT_Soya','CBOT_Soyameal']
    final_data=final_data.fillna(0)
    
    print(colored('\nForecasting - Soya Meal Futures Completed... \n','green'))
    
    year_var[1][0]=actual_year
    for index,row in final_data.iloc[:,:].iterrows():
        if((row[0]==year_var[1][0])&(row[1]>year_var[1][2])):
            final_data.loc[index,'Tonnes']=0
        if(row[0]>year_var[1][0]):
            final_data.loc[index,'Tonnes']=0

    final_data.to_csv(str(path)+'/Export/Data Directory/Soya_Bran/final_input_data.csv',encoding=encoding,index=False)
    
    print(colored('\nSoya Bran - Final Export Input Data Prepared for Forecasting... \n','green'))
    print(colored('\nSoya Bran - Export Forecasting Starts... \n','green'))

    ##Forecasting Soya Bran
    r['source'](str(path)+'/Export/R Codes/Soya Bran Export Forecasting.R')   
    
    print(colored('\nSoya Bran - Export Forecasting Completed!!!... \n','green'))
    print(colored('\nSoya Bran - Format Building Starts... \n','green'))
    print(colored('\nRetrieving Export Predictions for Soya Bran...\n','green'))
    
    ## Getting all Predicted Values - Zones
    export_predictions=retrieving_export_predictions()
    state_to_zone=pd.read_csv(str(path)+'/Export/Data Directory/Soya_Bran/State_to_Zone_Ratio.csv',encoding=encoding)
    state_to_zone=preprocessing_state_to_zone(state_to_zone)
    predicted_export=retrieving_predicted_export(export_predictions,year_var)
    
    print(colored('\nSoya Bran - Converting State-wise Export Predictions into Zone-wise Predictions for Model States...\n','green'))
    
    predicted_zonewise=calculate_state_to_zone(predicted_export,state_to_zone,'pred') 
    
    ## Getting Confidence Results for Predicted States
    p=str(path)+'/Export/Data Directory/Soya_Bran/Confidence-Results/'
    confidence_result=retrieving_confidence(p,'exp')
    confidence_result=calculate_state_to_zone(confidence_result,state_to_zone,'confidence')
    confidence_result.to_csv(str(path)+'/Export/Data Directory/Soya_Bran/Confidence_Export.csv',encoding=encoding,index=False)
    
    ## Merging the Confidence Interval Results with Export Predictions
    zonewise_predicted=pd.merge(predicted_zonewise,confidence_result,on=['Year','Month','UF','Zonas'],how='outer',right_index=False,sort=True,indicator=True).query("_merge != 'right_only'").drop('_merge',axis=1)
    zonewise_predicted['Tipo']='Prevista'
    zonewise_predicted['Tipo Ajustado']='Prevista'
    zonewise_predicted.to_csv(str(path)+'/Export/Data Directory/Soya_Bran/Predicted_Export.csv',encoding=encoding,index=False)
    
    print(colored('\nSoya Bran - Converting State-wise Export Predictions into Zone-wise Predictions for Model States Completed!...\n','green'))
    print(colored('\nSoya Bran - Converting State-wise Actual Export into Zone-wise Export Values for all States...\n','green'))
    
    ## Getting all Actual Values - Zones
    actual_data = final_data[['Year','Month','UF','Tonnes']]
    actual_export=retrieving_actual_export(actual_data,year_var)
    zonewise_actual=calculate_state_to_zone(actual_export,state_to_zone,'pred')
    zonewise_actual=add_confidence(zonewise_actual)
    zonewise_actual['Tipo']='Real'
    zonewise_actual['Tipo Ajustado']='Real / Calculada'
    zonewise_actual.to_csv(str(path)+'/Export/Data Directory/Soya_Bran/Actual_Export.csv',encoding=encoding,index=False)
    
    print(colored('\nSoya Bran - Converting State-wise Actual Export into Zone-wise Export Values Completed!...\n','green'))
    print(colored('\nSoya Bran - Retrieving Fitted Export Values for Model States...\n','green'))
    
    ## Getting all Fitted Values - Zones
    fitted_export=retrieving_actual_export(export_predictions,year_var)
    zonewise_fitted=calculate_state_to_zone(fitted_export,state_to_zone,'pred')
    zonewise_fitted=add_confidence(zonewise_fitted)
    zonewise_fitted['Tipo']='Calculada'
    zonewise_fitted['Tipo Ajustado']='Real / Calculada'
    
    print(colored('\nSoya Bran - Retrieving Fitted Export Values for Model States Completed!...\n','green'))
    print(colored('\nSoya Bran - Retrieving Non - Model States Export Values...\n','green'))
    
    ## Getting Non Model Predicted Values - Zones
    non_model_states=non_predicted_states(actual_data,export_predictions,year_var)
    print(non_model_states)
    zonewise_non_model=calculate_state_to_zone(non_model_states,state_to_zone,'pred')
    zonewise_non_model=add_confidence2(zonewise_non_model)
    zonewise_non_model['Tipo']='Prevista'
    zonewise_non_model['Tipo Ajustado']='Prevista'
    
    print(colored('\nSoya Bran - Converting Non - Model States Export Values into Zone-wise...\n','green'))
    
    ## Getting Non Model Fitted Values - Zones
    all_states=actual_data['UF'].unique()
    model_states=export_predictions['UF'].unique()
    non_model_state_fitted=list(set(all_states).difference(model_states))
    fitted_non_model=actual_export[actual_export['UF'].isin(non_model_state_fitted)]
    fitted_non_model_zonewise=calculate_state_to_zone(fitted_non_model,state_to_zone,'pred')    
    fitted_non_model_zonewise['Zones_Export']=0
    fitted_non_model_zonewise=add_confidence(fitted_non_model_zonewise)
    fitted_non_model_zonewise['Tipo']='Calculada'
    fitted_non_model_zonewise['Tipo Ajustado']='Real / Calculada'
    
    print(colored('\nSoya Bran - Combining Zone-wise Model and Non-Model States Actual, Predicted & Fitted Export Values...\n','green'))
    
    soyabran_export=merge_all_states(zonewise_actual,zonewise_fitted,zonewise_predicted,zonewise_non_model,fitted_non_model_zonewise)  
    soyabran_export=pd.merge(soyabran_export,state1,left_on=['UF'],right_on=['State_Code'],how='left',right_index=False,sort=True,indicator=True).query("_merge != 'left_only'").drop('_merge',axis=1)
    del soyabran_export['State_Code']
    
    soyabran_export['State_Name']=soyabran_export['State_Name'].str.upper() 
    soyabran_export.insert(0,'Produto','Farelo')
    soyabran_export=convert_thousand_tonnes(soyabran_export)
    soyabran_export=soyabran_export[['Produto','Month','Year','State_Name','UF','Zonas','Low','Zones_Export','High','Tipo','Tipo Ajustado','Conf']]
    soyabran_export.columns=['Produto',u'Mês','Ano','Estado','UF',u'Microrregião',u'Exportação Mín (Kt)',u'Exportação Med (Kt)',u'Exportação Max (Kt)','Tipo','Tipo Ajustado',u'% Confiança']
    soyabran_export.iloc[:,11] = soyabran_export.iloc[:,11].apply(pd.to_numeric, errors = 'coerce')
    
    print(colored('\nSoya Bran - Combining Zone-wise Model and Non-Model States Actual, Predicted & Fitted Export Values Completed!!!...\n','green'))
    
    for field in soyabran_export.iloc[:,6:9]:        
        soyabran_export[field] = soyabran_export[field].map(try_cutoff)

    ## Saving in Indian Settings
    date=get_date()
    soyabran_export.to_csv(str(path)+'/Export/Output Files/'+date+u'_Exportação.csv',index=False,encoding=encoding)
    soyabran_export.to_csv(str(path)+'/Export/Input Files/'+date+u'_Exportação_meal.csv',index=False,encoding=encoding)
    
    print(colored('\nSoya Bran - Zone-Wise Export Prediction Results Saving in Database !!!...\n','green'))
    
    ## Saving in Brazilian Settings
    soyabran_export[[u'Exportação Mín (Kt)',u'Exportação Med (Kt)',u'Exportação Max (Kt)']] = soyabran_export[[u'Exportação Mín (Kt)',u'Exportação Med (Kt)',u'Exportação Max (Kt)']].astype(str)
    for field in soyabran_export.iloc[:,6:9]:
        soyabran_export[field] = soyabran_export[field].map(Replace)
    
    soyabran_export.to_excel(str(path)+'/Export/Output Files/'+date+u'_Exportação.xlsx',index=False,encoding=encoding)
    
    print(colored('\nSoya Bran - Export Forecasting & Output Results Process Completed !!!...\n','green'))

if __name__ == '__main__':
    main()
