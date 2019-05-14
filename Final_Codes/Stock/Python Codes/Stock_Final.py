
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np
import time

# In[5]:


stock_ibge = pd.read_excel('../Input Files/Tabela 254-1.xlsx' , sheet_name='Tabela 1')


# In[7]:


stock_ibge['Milho '] = stock_ibge['Milho '].replace(['X', '-'], 0 )
stock_ibge['Soja'] = stock_ibge['Soja'].replace(['X', '-'], 0)


# In[8]:


stock_ibge['Fraction_Milho'] = (stock_ibge['Milho ']/stock_ibge['Milho '][0])
stock_ibge['Fraction_Soya'] = (stock_ibge['Soja']/stock_ibge['Soja'][0])


# In[12]:


ibge_fractions = stock_ibge[1:]


# In[14]:


agro_consult_soja = pd.read_excel('../Input Files/2018_12_03_Balanco_OD.xlsx', sheet_name='Soja')


# In[15]:


agro_consult_milho = pd.read_excel('../Input Files/2018_12_03_Balanco_OD.xlsx', sheet_name='Milho')


# In[17]:


stock_final_soja = agro_consult_soja[2017][agro_consult_soja['Values'] == 'Estoque final'].iloc[0]
stock_final_milho = agro_consult_milho[2017][agro_consult_milho['Values'] == 'Estoque final'].iloc[0]


# In[20]:


ibge_fractions['stock_soya'] = (ibge_fractions['Fraction_Soya'] * stock_final_soja)
ibge_fractions['stock_milho'] = (ibge_fractions['Fraction_Milho'] * stock_final_milho)


# In[22]:


state_name = pd.read_excel("../Input Files/State_codes.xlsx",encoding='latin1')


# In[23]:


state_name['Proper_State'] = state_name['Proper_State'].str.title()
ibge_fractions['Estado'] = ibge_fractions['Estado'].str.title()


# In[25]:


stocks = pd.merge(ibge_fractions, state_name[['Proper_State','State Abbreviation']], left_on='Estado',right_on='Proper_State')


# In[28]:


stock_soya_17 = stocks.drop(['Estado','Milho ','Soja','Fraction_Milho','Fraction_Soya','stock_milho'],axis = 1)
stock_milho_17 = stocks.drop(['Estado','Milho ','Soja','Fraction_Milho','Fraction_Soya','stock_soya'],axis = 1)


# In[29]:


zones = pd.read_csv('../Input Files/zones.csv',  encoding ='latin1')

# In[30]:
# In[31]:


zones = (zones.drop_duplicates())


# In[33]:


stock_soya_17_f = pd.merge(zones, stock_soya_17, left_on='Estado/UF',right_on='State Abbreviation')
stock_milho_17_f = pd.merge(zones, stock_milho_17, left_on='Estado/UF',right_on='State Abbreviation')


# In[35]:


stock_soya_17_f.drop(['State Abbreviation'],axis=1,inplace=True)
stock_milho_17_f.drop(['State Abbreviation'],axis=1,inplace=True)


# # DIVIDING THE VALUE IN ZONES FOR SOYA ##############

# In[37]:


zone_count = (stock_soya_17_f['Estado/UF'].value_counts())
#zone_count.columns = ['Estado/UF', 'Count']
zone_val_dic = zone_count.to_dict()


# In[38]:


for i in range(len(stock_soya_17_f)):
    if (stock_soya_17_f['Estado/UF'][i] != stock_soya_17_f['Zona/UF'][i]):
        stock_soya_17_f['stock_soya'][i] = stock_soya_17_f['stock_soya'][i]/(zone_val_dic[stock_soya_17_f['Estado/UF'][i]] - 1)
 


# In[39]:


stock_soya_17_f['Mês'] = 12
stock_soya_17_f['Ano'] = 2017
stock_soya_17_f['Produto'] = 'Soja'


# # DIVIDING THE VALUE IN ZONES FOR MILHO ##############

# In[40]:


zone_count1 = (stock_milho_17_f['Estado/UF'].value_counts())
#zone_count.columns = ['Estado/UF', 'Count']
zone_val_dic1 = zone_count1.to_dict()


# In[41]:


for i in range(len(stock_soya_17_f)):
    if (stock_milho_17_f['Estado/UF'][i] != stock_milho_17_f['Zona/UF'][i]):
        stock_milho_17_f['stock_milho'][i] = stock_milho_17_f['stock_milho'][i]/(zone_val_dic1[stock_milho_17_f['Estado/UF'][i]] - 1)
 


# In[42]:


stock_milho_17_f['Mês'] = 1
stock_milho_17_f['Ano'] = 2018
stock_milho_17_f['Produto'] = 'Milho'


# In[42]:


stock_milho_17_f.rename(columns={'stock_milho':'Estoque','Proper_State':'Estado', 'Estado/UF':'UF'},inplace=True)
stock_soya_17_f.rename(columns={'stock_soya':'Estoque','Proper_State':'Estado','Estado/UF':'UF'},inplace=True)


# In[44]:


stock_final = stock_milho_17_f.append(stock_soya_17_f)


# In[47]:


stock_final = stock_final[['Produto', 'Mês', 'Ano','Estado','UF', 'Zona/UF', 'Estoque']] 


# # Cosmetic changes

# In[4]:


stock_final["Tipo"] =  "Calculada"
stock_final["Tipo Ajustado"] = "Real / Calculada"
stock_final["% Confiança"] = 0


# In[5]:


stock_final.rename(columns={'Zona/UF':'Microrregião','Estoque':'Estoque (Kt)'},inplace=True)


# In[5]:


stock_final =stock_final[(stock_final['Microrregião'] != 'MG' ) & (stock_final['Microrregião'] != 'MS' ) & (stock_final['Microrregião'] != 'MT' ) &
               (stock_final['Microrregião'] != 'GO' ) & (stock_final['Microrregião'] != 'BA' ) & (stock_final['Microrregião'] != 'PI' ) &
               (stock_final['Microrregião'] != 'MA' ) & (stock_final['Microrregião'] != 'TO' ) & (stock_final['Microrregião'] != 'PA' )]


# In[7]:
ttime = time.strftime("%Y%m%d")
stock_final.to_excel("../Output Files/"+ ttime + "_Estoque.xlsx", encoding='utf-8-sig',index=False)

