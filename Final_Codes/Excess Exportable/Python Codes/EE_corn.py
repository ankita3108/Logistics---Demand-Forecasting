#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np


# In[11]:


final_corn_production = pd.read_excel("../Input Files/20190221_Produção.xlsx",sheet_name=0)


final_corn_production =  final_corn_production[final_corn_production["Produto"] != "Soja"]

final_corn_production=final_corn_production[final_corn_production.Ano > 2017]



corn_production_real=final_corn_production[final_corn_production['Tipo'] == 'Real']


corn_production_prevista = final_corn_production[final_corn_production['%Confiança'] == 70 ]

corn_production = corn_production_real.append(corn_production_prevista)




corn_production=corn_production[['Mês','Ano','Microrregião','Produção (Kt)']]


# In[16]:


def Replace(str1):

    str1 = str1.replace(',','third')

    str1 = str1.replace('.',',')

    str1 = str1.replace('third','.')
    num = float(str1.replace(",",""))
    return num

corn_production["Produção (Kt)"]=[Replace(corn_production["Produção (Kt)"].values[i]) for i in range(len(corn_production))]



corn_production=corn_production.groupby(['Mês','Ano','Microrregião'],as_index=False).sum()



# # Consumption

# In[28]:


raw_consum_corn = pd.read_excel('../Input Files/20181211_Consumo.xlsx',sheet_name=0)


corn_consumption = raw_consum_corn[raw_consum_corn.Produto == 'Milho']



corn_consumption = corn_consumption[(corn_consumption.Ano> 2017) & (corn_consumption.Ano<2020)]



corn_consumption = corn_consumption[['Produto','Mês', 'Ano','Estado','UF','Microrregião', 'Consumo (Kt)']]


corn_prod_cosum= pd.merge(corn_consumption,corn_production,how="left",on=["Microrregião",'Mês', 'Ano'])#left_on='Zona/UF',right_on='Zona/UF')



# # Corn_Export 

# In[38]:


#laoding Export data
raw_corn_export = pd.read_excel("../Input Files/20190222_Exportação.xlsx",encoding='utf-8-sig',sheet_name="Exportação")



# In[40]:


corn_export = raw_corn_export[raw_corn_export.Ano > 2017]

corn_export =  corn_export[corn_export.Produto == "Milho"]

corn_export_real=corn_export[corn_export['Tipo']=="Real"]

corn_export_prevista=corn_export[corn_export["%Confiança"] == 70]




corn_export = corn_export_real.append(corn_export_prevista)





corn_export=corn_export[['Mês', 'Ano','Microrregião','Exportação Med (Kt)']]



# In[48]:


corn_prod_consum_export = pd.merge(corn_prod_cosum,corn_export, how="left",on=["Microrregião",'Mês', 'Ano'])

# # Stocks

# In[53]:


raw_stocks_corn =  pd.read_excel("../Input Files/20181210_Estoque.xlsx",encoding='utf-8-sig')



# In[54]:


corn_stocks = raw_stocks_corn[(raw_stocks_corn['Microrregião'] != 'MG' ) & (raw_stocks_corn['Microrregião'] != 'MS' ) & (raw_stocks_corn['Microrregião'] != 'MT' ) &
               (raw_stocks_corn['Microrregião'] != 'GO' ) & (raw_stocks_corn['Microrregião'] != 'BA' ) & (raw_stocks_corn['Microrregião'] != 'PI' ) &
               (raw_stocks_corn['Microrregião'] != 'MA' ) & (raw_stocks_corn['Microrregião'] != 'TO' ) & (raw_stocks_corn['Microrregião'] != 'PA' )]
corn_stocks= corn_stocks[corn_stocks.Produto == "Milho"]



corn_stocks.reset_index(inplace=True)
corn_stocks.drop("index",axis=1,inplace=True)


initial_stock_dict={}
for i in range(corn_stocks.shape[0]):
    initial_stock_dict[corn_stocks["Microrregião"][i]] = corn_stocks["Estoque (Kt)"][i]



corn_prod_consum_export["Intial_stock"] =  0.0
corn_prod_consum_export["Excess_Exportable"] = 0.0

corn_prod_consum_export=corn_prod_consum_export.drop(corn_prod_consum_export.loc[(corn_prod_consum_export.Ano == 2018) & (corn_prod_consum_export.Mês == 1),:].index)
corn_prod_consum_export


# In[79]:


corn_prod_consum_export.reset_index(inplace=True)
corn_prod_consum_export.drop("index",axis=1,inplace=True)


for i in range(corn_prod_consum_export.shape[0]):
    if i < corn_prod_consum_export.shape[0]:
        if corn_prod_consum_export.Ano[i] == 2018 and corn_prod_consum_export.Mês[i] == 2:
            print(corn_prod_consum_export["Microrregião"][i])
            corn_prod_consum_export["Intial_stock"][i] = initial_stock_dict[corn_prod_consum_export["Microrregião"][i]]
            print("initial_stock",initial_stock_dict[corn_prod_consum_export["Microrregião"][i]]," ",corn_prod_consum_export["Intial_stock"][i])
            corn_prod_consum_export["Excess_Exportable"][i] = corn_prod_consum_export["Intial_stock"][i]+corn_prod_consum_export["Produção (Kt)"][i]-corn_prod_consum_export["Consumo (Kt)"][i]-corn_prod_consum_export["Exportação Med (Kt)"][i]
            print( corn_prod_consum_export["Excess_Exportable"][i])
            if corn_prod_consum_export["Excess_Exportable"][i] < 0:
                corn_prod_consum_export["Excess_Exportable"][i] = 0
            corn_prod_consum_export["Intial_stock"][i+1] = corn_prod_consum_export["Excess_Exportable"][i]      
        else:
            corn_prod_consum_export["Excess_Exportable"][i] = corn_prod_consum_export["Intial_stock"][i]+corn_prod_consum_export["Produção (Kt)"][i]-corn_prod_consum_export["Consumo (Kt)"][i]-corn_prod_consum_export["Exportação Med (Kt)"][i]
#             print("initial_stock",corn_prod_consum_export["Intial_stock"][i])
            print( corn_prod_consum_export["Excess_Exportable"][i])
            if corn_prod_consum_export["Excess_Exportable"][i] < 0:
                corn_prod_consum_export["Excess_Exportable"][i] = 0
            corn_prod_consum_export["Intial_stock"][i+1] = corn_prod_consum_export["Excess_Exportable"][i]




corn_prod_consum_export.rename(columns={'Produção (Kt)':'Produção Med (Kt)','Intial_stock':'Estoque (Kt)','Excess_Exportable':'Excedente Exportável (Kt)',
                                       'Consumo (Kt)':'Consumo Med (Kt)'},inplace=True)



corn_prod_consum_export = corn_prod_consum_export[['Produto', 'Mês', 'Ano', 'Estado' , 'UF', 'Microrregião', 'Produção Med (Kt)',
       'Consumo Med (Kt)', 'Exportação Med (Kt)',
       'Estoque (Kt)', 'Excedente Exportável (Kt)']]
      




# In[85]:


corn_excess_export =  corn_prod_consum_export.copy()
corn_excess_export.drop_duplicates(inplace=True)


# In[86]:


corn_excess_export.to_excel("../Data Directory/20190226_Excedente Exportável_Milho.xlsx",index=False)






