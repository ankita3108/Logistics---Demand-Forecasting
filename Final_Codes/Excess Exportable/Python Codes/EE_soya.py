#!/usr/bin/env python
# coding: utf-8

# In[69]:


import pandas as pd
import numpy as np
import time

# In[221]:


final_soya_production = pd.read_excel("../Input Files/20190221_Produção.xlsx",sheet_name=0)


final_soya_production =  final_soya_production[final_soya_production["Produto"] == "Soja"]


final_soya_production=final_soya_production[final_soya_production.Ano > 2017]

soya_production_real=final_soya_production[final_soya_production['Tipo'] == 'Real']


soya_production_prevista = final_soya_production[final_soya_production['%Confiança'] == 70 ]


soya_production = soya_production_real.append(soya_production_prevista)

soya_production=soya_production[['Mês','Ano','Microrregião','Produção (Kt)']]


def Replace(str1):

    str1 = str1.replace(',','third')

    str1 = str1.replace('.',',')

    str1 = str1.replace('third','.')
    num = float(str1.replace(",",""))
    return num

soya_production["Produção (Kt)"]=[Replace(soya_production["Produção (Kt)"].values[i]) for i in range(len(soya_production))]


raw_consum_soya = pd.read_excel('../Input Files/20181211_Consumo.xlsx',sheet_name=0)

soya_consumption = raw_consum_soya[raw_consum_soya.Produto == 'Soja']



soya_consumption = soya_consumption[(soya_consumption.Ano> 2017) & (soya_consumption.Ano<2020)]



soya_consumption = soya_consumption[['Produto','Mês', 'Ano','Estado','UF','Microrregião', 'Consumo (Kt)']]

soya_prod_cosum= pd.merge(soya_consumption,soya_production,how="left",on=["Microrregião",'Mês', 'Ano'])

#laoding Export data
raw_soya_export = pd.read_excel("../Input Files/20190222_Exportação.xlsx",encoding='utf-8-sig',sheet_name="Exportação")

soya_export = raw_soya_export[raw_soya_export.Ano > 2017]

soya_export =  soya_export[soya_export.Produto == "Soja"]


# In[252]:


soya_export_real=soya_export[soya_export['Tipo']=="Real"]


# In[253]:


soya_export_prevista=soya_export[soya_export["%Confiança"] == 70]



# In[255]:


soya_export = soya_export_real.append(soya_export_prevista)




soya_export=soya_export[['Mês', 'Ano','Microrregião','Exportação Med (Kt)']]



soya_prod_consum_export = pd.merge(soya_prod_cosum,soya_export, how="left",on=["Microrregião",'Mês', 'Ano'])


# # Stocks

# In[266]:


raw_stocks_soya =  pd.read_excel("../Input Files/20181210_Estoque.xlsx",encoding='utf-8-sig')



# In[267]:


soya_stocks = raw_stocks_soya[(raw_stocks_soya['Microrregião'] != 'MG' ) & (raw_stocks_soya['Microrregião'] != 'MS' ) & (raw_stocks_soya['Microrregião'] != 'MT' ) &
               (raw_stocks_soya['Microrregião'] != 'GO' ) & (raw_stocks_soya['Microrregião'] != 'BA' ) & (raw_stocks_soya['Microrregião'] != 'PI' ) &
               (raw_stocks_soya['Microrregião'] != 'MA' ) & (raw_stocks_soya['Microrregião'] != 'TO' ) & (raw_stocks_soya['Microrregião'] != 'PA' )]
soya_stocks= soya_stocks[soya_stocks.Produto == "Soja"]



soya_stocks.reset_index(inplace=True)
soya_stocks.drop("index",axis=1,inplace=True)



initial_stock_dict={}
for i in range(soya_stocks.shape[0]):
    initial_stock_dict[soya_stocks["Microrregião"][i]] = soya_stocks["Estoque (Kt)"][i]



soya_prod_consum_export["Intial_stock"] =  0.0
soya_prod_consum_export["Excess_Exportable"] = 0.0



# In[279]:


for i in range(soya_prod_consum_export.shape[0]):
    if i < soya_prod_consum_export.shape[0]:
        if soya_prod_consum_export.Ano[i] == 2018 and soya_prod_consum_export.Mês[i] == 1:
#             print(soya_prod_consum_export["Microrregião"][i])
            soya_prod_consum_export["Intial_stock"][i] = initial_stock_dict[soya_prod_consum_export["Microrregião"][i]]
            print("Inital Stock:",soya_prod_consum_export["Intial_stock"])
            soya_prod_consum_export["Excess_Exportable"][i] = soya_prod_consum_export["Intial_stock"][i]+soya_prod_consum_export["Produção (Kt)"][i]-soya_prod_consum_export["Consumo (Kt)"][i]-soya_prod_consum_export["Exportação Med (Kt)"][i]
            print("EE:",soya_prod_consum_export["Excess_Exportable"])
            if soya_prod_consum_export["Excess_Exportable"][i] < 0:
                soya_prod_consum_export["Excess_Exportable"][i] = 0
            soya_prod_consum_export["Intial_stock"][i+1] = soya_prod_consum_export["Excess_Exportable"][i]      
        else:
            soya_prod_consum_export["Excess_Exportable"][i] = soya_prod_consum_export["Intial_stock"][i]+soya_prod_consum_export["Produção (Kt)"][i]-soya_prod_consum_export["Consumo (Kt)"][i]-soya_prod_consum_export["Exportação Med (Kt)"][i]
            if soya_prod_consum_export["Excess_Exportable"][i] < 0:
                soya_prod_consum_export["Excess_Exportable"][i] = 0
            soya_prod_consum_export["Intial_stock"][i+1] = soya_prod_consum_export["Excess_Exportable"][i]



# In[283]:


soya_prod_consum_export.rename(columns={'Produção (Kt)':'Produção Med (Kt)','Intial_stock':'Estoque (Kt)','Excess_Exportable':'Excedente Exportável (Kt)',
                                       'Consumo (Kt)':'Consumo Med (Kt)'},inplace=True)



soya_prod_consum_export = soya_prod_consum_export[['Produto', 'Mês', 'Ano', 'Estado' , 'UF', 'Microrregião', 'Produção Med (Kt)',
       'Consumo Med (Kt)', 'Exportação Med (Kt)',
       'Estoque (Kt)', 'Excedente Exportável (Kt)']]
      




# In[284]:


soya_excess_export =  soya_prod_consum_export.copy()
soya_excess_export.drop_duplicates(inplace=True)



soya_ee = soya_excess_export.copy()

# # cosmetic changes

# In[286]:




# In[287]:


corn_ee =  pd.read_excel("../Data Directory/20190226_Excedente Exportável_Milho.xlsx",sheet_name=0)


final_ee =  soya_ee.append(corn_ee)



# In[289]:


final_ee["Tipo"] = "Calculada"
final_ee["Tipo Ajustado"] = "Real / Calculada"
final_ee["% Confiança"] = 0



# final_ee.to_excel("./20190222_Excedente Exportável_indian_format.xlsx",index=False)


# #  Brazilain Format

# In[292]:


def comma1(num):
    return '{:,.3f}'.format(num) 
def Replace(str1):

    str1 = str1.replace(',','third')

    str1 = str1.replace('.',',')

    str1 = str1.replace('third','.')

    return str1 


# In[297]:


# format into comma as thousand operator
final_ee["Produção Med (Kt)"]=[comma1(final_ee["Produção Med (Kt)"].values[i]) for i in range(len(final_ee))]
final_ee["Consumo Med (Kt)"]=[comma1(final_ee["Consumo Med (Kt)"].values[i]) for i in range(len(final_ee))]
final_ee["Exportação Med (Kt)"]=[comma1(final_ee["Exportação Med (Kt)"].values[i]) for i in range(len(final_ee))]
final_ee["Estoque (Kt)"]=[comma1(final_ee["Estoque (Kt)"].values[i]) for i in range(len(final_ee))]
final_ee["Excedente Exportável (Kt)"]=[comma1(final_ee["Excedente Exportável (Kt)"].values[i]) for i in range(len(final_ee))]


# In[299]:


#Brazilian format
final_ee["Produção Med (Kt)"]=[Replace(final_ee["Produção Med (Kt)"].values[i]) for i in range(len(final_ee))]
final_ee["Consumo Med (Kt)"]=[Replace(final_ee["Consumo Med (Kt)"].values[i]) for i in range(len(final_ee))]
final_ee["Exportação Med (Kt)"]=[Replace(final_ee["Exportação Med (Kt)"].values[i]) for i in range(len(final_ee))]
final_ee["Estoque (Kt)"]=[Replace(final_ee["Estoque (Kt)"].values[i]) for i in range(len(final_ee))]
final_ee["Excedente Exportável (Kt)"]=[Replace(final_ee["Excedente Exportável (Kt)"].values[i]) for i in range(len(final_ee))]


ttime =  time.strftime("%Y%m%d")
# writing out the file
final_ee.to_excel("../Output Files/"+ttime+"_Excedente Exportável.xlsx",index=False,encoding = "utf-8-sig")







