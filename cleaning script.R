library(tidyverse)
library(magrittr)
library(SASxport)

# =============================================================

# 2011-2012 data

#download.file('https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/TST_G.XPT','NHANES2011_T.xpt',mod='wb')
data_T= read.xport('NHANES2011_T.xpt') %>% select(SEQN, LBXTST)

#download.file('https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/MGX_G.XPT','NHANES2011_grip.xpt',mod='wb')
data_handedness= read.xport('NHANES2011_grip.xpt') %>% select(SEQN,MGD130)

#download.file('https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.XPT','NHANES2011_demographics.xpt',mod='wb')
data_demographics= read.xport('NHANES2011_demographics.xpt') %>% select(SEQN,RIAGENDR,RIDAGEYR) # https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_H.htm#INDHHIN2

#download.file('https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/FASTQX_G.XPT','NHANES2011_session.xpt',mod='wb')
data_session= read.xport('NHANES2011_session.xpt') %>% select(SEQN,PHDSESN) %>% rename(session_time=PHDSESN)

#download.file('https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/RHQ_G.XPT','NHANES2011_period.xpt',mod='wb')
data_periods = read.xport('NHANES2011_period.xpt') %>% select(SEQN,RHQ031) %>% rename(regular_periods=RHQ031)

data1 = data_demographics %>% inner_join(data_handedness,by='SEQN')%>% inner_join(data_T,by='SEQN') %>% inner_join(data_session,by='SEQN') %>% left_join(data_periods,by='SEQN')
data1$year = 2011
data1$LBXEST = -1
data1$regular_periods[data1$RIAGENDR==1]= -1
data1$regular_periods[is.na(data1$regular_periods)|data1$regular_periods==9]= -1
data1 = data1[data1$RIDAGEYR>= 18,]
data1 %>% nrow # 5615

# and 2013-2014 data
#download.file('https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/TST_H.XPT','NHANES2013_T.xpt',mod='wb')
data_T2= read.xport('NHANES2013_T.xpt') %>% select(SEQN, LBXTST,LBXEST)

#download.file('https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/MGX_H.XPT','NHANES2013_grip.xpt',mod='wb')
data_handedness2= read.xport('NHANES2013_grip.xpt') %>% select(SEQN,MGD130)

#download.file('https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.XPT','NHANES2013_demographics.xpt',mod='wb')
data_demographics2= read.xport('NHANES2013_demographics.xpt') %>% select(SEQN,RIAGENDR,RIDAGEYR) # https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.htm#INDHHIN2

#download.file('https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/FASTQX_H.XPT','NHANES2013_session.xpt',mod='wb')
data_session2= read.xport('NHANES2013_session.xpt') %>% select(SEQN,PHDSESN) %>% rename(session_time=PHDSESN)

#download.file('https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/RHQ_H.XPT','NHANES2013_period.xpt',mod='wb')
data_periods2 = read.xport('NHANES2013_period.xpt') %>% select(SEQN,RHQ031) %>% rename(regular_periods=RHQ031)

data2 = data_demographics2 %>% inner_join(data_handedness2,by='SEQN')%>% inner_join(data_T2,by='SEQN') %>% inner_join(data_session2,by='SEQN') %>% left_join(data_periods2,by='SEQN')

data2$year = 2013
data2$regular_periods[data2$RIAGENDR==1]= -1
data2$regular_periods[is.na(data1$regular_periods)|data2$regular_periods==9]= -1
data2 = data2[data2$RIDAGEYR>= 18,]
data2 %>% nrow # 5924

# now bring them together

data = rbind(data1,data2) %>% na.omit 
colnames(data) = c('id','sex','age','handedness','T','session_time','regular_periods','year','E')
data %>% nrow # how much data do we have? 9538

data$sex %<>% factor
levels(data$sex)= c('male','female')
data$E[data$E<0]=NaN

data$handedness %<>%  factor()
levels(data$handedness) = c('Right-handed','Left-handed','Mixed-handed')

data %>% nrow

write.csv(data, 'handedness_hormone_data_cleaned.csv',row.names = F)
