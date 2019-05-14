#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np


# In[4]:

#reading the  data
soya  = pd.read_excel("../Input Files/soya_state_yearwise_consumption.xlsx",sheet_name="soya_crush")



soya.fillna(value=0,inplace=True)



# In[10]:


x = pd.melt(soya,id_vars= ['States'],var_name="Year")


# In[11]:


x.to_csv("../Data Directory/soya_state_yearwise_consumption.csv",index=False)


# In[18]:


corn  = pd.read_excel("../Input Files/corn_state_yearwise_consumption.xlsx",sheet_name="Sheet1")



# In[18]:


x = soya.States.unique()


# In[21]:


len([x[i] for i in range(len(x)) if len(x[i])>2])


# In[19]:


corn.drop(['Unnamed: 5','Unnamed: 7'],axis=1,inplace=True)


# In[20]:


y = pd.melt(corn,id_vars=['States'],var_name="Year")


# In[21]:


y.to_csv("../Data Directory/corn_state_yearwise_consumption.csv",index=False)







