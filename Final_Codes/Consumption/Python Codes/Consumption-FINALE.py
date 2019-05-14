#!/usr/bin/env python
# coding: utf-8

# In[15]:


import pandas as pd
import numpy as np
import time

# # CORN

# In[16]:


df1 =  pd.read_csv('../Data Directory/corn_state_yearwise_consumption.csv')


# In[17]:


states = df1.States.unique()





final_df = pd.DataFrame()


# In[20]:


for state in states:
    print(state)
    
    temp = df1[df1.States == state].reset_index()
    temp = temp.drop("index", axis=1)
    temp['sub-factor'] = np.nan
    temp['mon_value'] = temp['value'] / 12
    
    temp['date'] = '20'+temp['Year'].str.split('/').str[1] + '-' +'06'
    temp['date'] = pd.to_datetime(temp['date'])
    
    for i in range(len(temp) - 1):
        temp['sub-factor'][i] = temp['mon_value'][i+1] - temp['mon_value'][i]
    
    temp['sub-factor'].iloc[0:8:] = temp['sub-factor'].iloc[0:8:]/12
    temp['sub-factor'].iloc[-4:] = temp['sub-factor'].iloc[-4:]/48
    
    temp1 = temp.set_index('date').groupby('States', group_keys=False).resample('M').ffill().reset_index()
    
    temp1['monthly_value'] = np.nan
    
    for i in range(1,len(temp1)):
        temp1['monthly_value'][0] = temp1['mon_value'][0]
        temp1['monthly_value'][i] = temp1.loc[i-1,'monthly_value'] + temp1.loc[i-1,'sub-factor']

    final_df = final_df.append(temp1)


# In[8]:


final_df.drop(['Year','value','sub-factor','mon_value'], axis=1, inplace=True)






final_df.date = pd.to_datetime(final_df.date)
final_df['date'] = final_df['date'].dt.strftime('%Y-%m')


# In[11]:


final_df[['Ano','Mês']] = final_df['date'].str.split('-',expand=True)
final_df.drop('date',axis = 1,inplace=True)


# In[12]:


final_df['Zona/UF'] = final_df['States']


# In[13]:


final_df['States'] = final_df['States'].str.split('_').str[0]


# In[14]:


final_df['Produto'] = 'Milho'


# In[15]:


final_df = final_df[['Produto', 'Mês', 'Ano', 'States', 'Zona/UF',  'monthly_value']]


# In[16]:


final_df =final_df.rename(columns={'States': 'Estado/UF', 'monthly_value': 'Vol. Consumido (k Ton)'})



state_codes = pd.read_excel("../Input Files/State_codes.xlsx")





final_df_milho = pd.merge(final_df, state_codes[['Proper_State', 'State Abbreviation']], left_on= 'Estado/UF',right_on='State Abbreviation')

final_df_milho = final_df_milho[['Produto', 'Mês', 'Ano', 'Proper_State' ,'Estado/UF' , 'Zona/UF', 'Vol. Consumido (k Ton)']]


final_df_milho =final_df_milho.rename(columns={'Proper_State': 'Estado','Estado/UF': 'UF'})




# # SOJA ####################################

# In[27]:


df1 =  pd.read_csv('../Data Directory/soya_state_yearwise_consumption.csv')


# In[28]:


states = df1.States.unique()


# In[29]:


final_df = pd.DataFrame()


# In[30]:


for state in states:
    print(state)
    
    temp = df1[df1.States == state].reset_index()
    temp = temp.drop("index", axis=1)
    temp['sub-factor'] = np.nan
    temp['mon_value'] = temp['value'] / 12
    
    temp['date'] = '20'+temp['Year'].str.split('/').str[1] + '-' +'06'
    temp['date'] = pd.to_datetime(temp['date'])
    
    for i in range(len(temp) - 1):
        temp['sub-factor'][i] = temp['mon_value'][i+1] - temp['mon_value'][i]
    
    temp['sub-factor'].iloc[0:8:] = temp['sub-factor'].iloc[0:8:]/12
    temp['sub-factor'].iloc[-4:] = temp['sub-factor'].iloc[-4:]/48
    
    temp1 = temp.set_index('date').groupby('States', group_keys=False).resample('M').ffill().reset_index()
    
    temp1['monthly_value'] = np.nan
    
    for i in range(1,len(temp1)):
        temp1['monthly_value'][0] = temp1['mon_value'][0]
        temp1['monthly_value'][i] = temp1.loc[i-1,'monthly_value'] + temp1.loc[i-1,'sub-factor']

    final_df = final_df.append(temp1)


# In[31]:


final_df.drop(['Year','value','sub-factor','mon_value'], axis=1, inplace=True)





final_df.date = pd.to_datetime(final_df.date)
final_df['date'] = final_df['date'].dt.strftime('%Y-%m')


# In[34]:


final_df[['Ano','Mês']] = final_df['date'].str.split('-',expand=True)
final_df.drop('date',axis = 1,inplace=True)


# In[35]:


final_df['Zona/UF'] = final_df['States']


# In[36]:


final_df['States'] = final_df['States'].str.split('_').str[0]


# In[37]:


final_df['Produto'] = 'Soja'


# In[38]:


final_df = final_df[['Produto', 'Mês', 'Ano', 'States', 'Zona/UF',  'monthly_value']]


# In[39]:


final_df =final_df.rename(columns={'States': 'Estado/UF', 'monthly_value': 'Consumption Média (k Ton)'})


final_df_soya = pd.merge(final_df, state_codes[['Proper_State', 'State Abbreviation']], left_on= 'Estado/UF',right_on='State Abbreviation')


# In[42]:


final_df_soya = final_df_soya[['Produto', 'Mês', 'Ano', 'Proper_State' ,'Estado/UF' , 'Zona/UF',  'Consumption Média (k Ton)']]




final_df_soya =final_df_soya.rename(columns={'Proper_State': 'Estado','Estado/UF': 'UF' ,'Consumption Média (k Ton)': 'Vol. Consumido (k Ton)'})






final_df = final_df_soya.append(final_df_milho, ignore_index=True)




# # cosmetic changes



final_df =final_df[(final_df['Zona/UF'] != 'MG' ) & (final_df['Zona/UF'] != 'MS' ) & (final_df['Zona/UF'] != 'MT' ) &
               (final_df['Zona/UF'] != 'GO' ) & (final_df['Zona/UF'] != 'BA' ) & (final_df['Zona/UF'] != 'PI' ) &
               (final_df['Zona/UF'] != 'MA' ) & (final_df['Zona/UF'] != 'TO' ) & (final_df['Zona/UF'] != 'PA' )]








final_df["Tipo"] = "Calculada"
final_df["Tipo Ajustado"] = "Real / Calculada"
final_df["% Confiança"] = 0


# In[12]:


final_df.rename(columns={'Zona/UF':'Microrregião','Vol. Consumido (k Ton)':'Consumo (Kt)'},inplace=True)

final_df.loc[final_df["Consumo (Kt)"]<0,'Consumo (Kt)'] = 0.0


ttime =  time.strftime("%Y%m%d")
final_df.to_excel("../Output Files/"+ttime+"_Consumo.xlsx",index=False)


