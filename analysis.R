library(tidyverse)
library(magrittr)
library(rstan)
library(rstanarm)
library(bayesplot)
library(bayestestR)
library(cowplot)
library(e1071) 
library(modelr)

set.seed(140293)

data = read.csv('handedness_hormone_data_cleaned.csv') %>% filter(regular_periods<2) %>% select(-regular_periods) # filter women who do not have regular periods

data$sex %>% table
round(c(mean(data$age),sd(data$age)),2)
data$handedness %>% table
round(data$handedness %>% table()*100/nrow(data),2)

men_T = data %>% filter(sex=='male')
women_T = data %>% filter(sex=='female')

men_T$handedness %<>%  factor(levels = c("Right-handed","Left-handed","Mixed-handed"))
women_T$handedness %<>%  factor(levels = c("Right-handed","Left-handed","Mixed-handed"))

c(men_T$T %>% kurtosis(),men_T$T %>% log %>%  kurtosis(),men_T$T %>% sqrt %>%  kurtosis())
c(men_T$T %>% skewness(),men_T$T %>% log %>% skewness(),men_T$T %>% sqrt %>% skewness())

c(women_T$T %>% skewness(),women_T$T %>% log %>% skewness(),women_T$T %>% sqrt %>% skewness())
c(women_T$T %>% kurtosis(),women_T$T %>% log %>%  kurtosis(),women_T$T %>% sqrt %>%  kurtosis() )

resid_men_T = lm(sqrt(T)~age+factor(year)+session_time,men_T )
men_T %<>% add_residuals(resid_men_T)
men_T_plot=ggplot(men_T,aes(x=handedness,y=resid,fill=handedness))+geom_violin()+ theme(legend.position="none")+ labs(y = 'Residual square root transformed Testosterone', x=  'Handedness')

resid_women_T = lm(log(T)~age+factor(year)+session_time,women_T )
women_T %<>% add_residuals(resid_women_T)
women_T_plot=ggplot(women_T,aes(x=handedness,y=resid,fill=handedness))+geom_violin()+ theme(legend.position="none")+ labs(y = 'Residual log10 transformed Testosterone', x=  'Handedness')

# ========= onto analyses =================
# prior=normal(-1, 5, autoscale=FALSE) add this to model normally distrubted priors

men_T %>% stan_glm(scale(sqrt(T))~handedness+age+factor(year)+session_time, data=., family=gaussian,prior=cauchy(scale = 2.5,autoscale=T),iter=4000) ->mod
mod %>% describe_posterior(ci = c(0.95), diagnostic = 'Rhat', test = NULL)
#mod %>% prior_summary()

women_T %>% stan_glm(scale(log(T))~handedness+age+factor(year)+session_time, data=., family=gaussian,prior=cauchy(scale = 2.5,autoscale=T),iter=4000) ->mod
mod %>% describe_posterior(ci = c(0.95), diagnostic = 'Rhat', test = NULL)

#stan_trace(model1) # all chains seem stable and well mixed
#pp_check(model1) # posterior predictive check shows our model fits the data pretty well 
#stan_hist(model1, pars=c("handedness2"), bins=50)

# expluding those who were tested in the morning as a robustness check

men_T %>% filter(session_time>0) %>%  stan_glm(scale(sqrt(T))~handedness+age+factor(year)+session_time, data=., family=gaussian,prior=cauchy(scale = 2.5,autoscale=T),iter=4000) ->mod
mod %>% describe_posterior(ci = c(0.95), diagnostic = 'Rhat', test = NULL)

women_T %>% filter(session_time>0) %>% stan_glm(scale(log(T))~handedness+age+factor(year)+session_time, data=., family=gaussian,prior=cauchy(scale = 2.5,autoscale=T),iter=4000) ->mod
mod %>% describe_posterior(ci = c(0.95), diagnostic = 'Rhat', test = NULL)

# and Estadiol? ========================================

data_E = data %>% filter(is.na(E)==F)
data_E$sex %>% table 
data_E$handedness %>% table
round((data_E$handedness %>% table)*100/(data_E %>% nrow),2)
data_E %>% nrow

men_E = data_E %>% filter(sex=='male')
women_E = data_E %>% filter(sex=='female')

men_E$handedness %<>%  factor(levels = c("Right-handed","Left-handed","Mixed-handed"))
women_E$handedness %<>%  factor(levels = c("Right-handed","Left-handed","Mixed-handed"))

men_E$handedness %>% table
women_E$handedness %>% table

c(men_E$E %>% kurtosis(),men_E$E %>% log %>%  kurtosis(),men_E$E %>% sqrt %>%  kurtosis())
c(men_E$E %>% skewness(),men_E$E %>% log %>% skewness(),men_E$E %>% sqrt %>% skewness())

c(women_E$E %>% skewness(),women_E$E %>% log %>% skewness(),women_E$E %>% sqrt %>% skewness())
c(women_E$E %>% kurtosis(),women_E$E %>% log %>%  kurtosis(),women_E$E %>% sqrt %>%  kurtosis() )

# models

men_E %>% stan_glm(scale(sqrt(E))~handedness+age+session_time, data=., family=gaussian,prior=cauchy(scale = 2.5,autoscale=T),iter=4000) ->mod
mod %>% describe_posterior(ci = c(0.95), diagnostic = 'Rhat', test = NULL)

resid_men_E = lm(sqrt(E)~age+session_time,men_E )
men_E %<>% add_residuals(resid_men_E)
men_E_plot=ggplot(men_E,aes(x=handedness,y=resid,fill=handedness))+geom_violin()+ theme(legend.position="none")+ labs(y = 'Residual square root transformed Oestradiol', x=  'Handedness')

women_E$handedness2 = women_E$handedness %>% as.character()
women_E$handedness2[women_E$handedness2=='Mixed-handed']='Non-Right-handed'
women_E$handedness2[women_E$handedness2=='Left-handed']='Non-Right-handed'
women_E$handedness2 %<>% factor(levels = c('Right-handed','Non-Right-handed') )

women_E %>% stan_glm(scale(log(E))~handedness2+age+session_time, data=., family=gaussian,prior=cauchy(scale = 2.5,autoscale=T),iter=4000) ->mod
mod %>% describe_posterior(ci = c(0.95), diagnostic = 'Rhat', test = NULL)

resid_women_E = lm(log(E)~age+session_time,women_E )
women_E %<>% add_residuals(resid_women_E)
women_E_plot=ggplot(women_E,aes(x=handedness2,y=resid,fill=handedness2))+geom_violin()+ theme(legend.position="none")+ labs(y = 'Residual log10 transformed Oestradiol', x=  'Handedness')

hormone_plot = plot_grid(men_T_plot,women_T_plot,men_E_plot,women_E_plot,ncol = 2,labels = c('Men T','Women T','Men E','Women E'))

plot_width = 25
plot_height = plot_width

ggsave('hormone_plot.jpg',plot=hormone_plot,dpi = 500,width = plot_width,height=plot_height,units='cm')
