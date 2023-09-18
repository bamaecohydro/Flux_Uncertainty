#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Response to reviewer
#Coder: Nate Jones
#Date: 9/17/2023
#Purpose: Address response to reviews
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup environment -------------------------------------------------------------
#Clear R environment
remove(list=ls())

#Load required packages
library(tidyverse)
library(lubridate)
library(zoo)
library(dataRetrieval)
library(parallel)
library(patchwork)


# Are error distributions normally distributed ---------------------------------
#Load data
gages<-read_csv('data/gages.csv')
sd<-read_csv("temp/sd.csv")
ts<-read_csv("temp/ts.csv")

#create simulation function
shapiro_fun<-function(n){
  
  #tidy data
  sd_gage <- sd %>% filter(site_no==gages$site_no[n]) 
  meas    <- sd_gage %>% filter(type=='raw')
  mod     <- sd_gage %>% filter(type=='model')
  
  #create interpolation function
  mod_fun <- approxfun(mod$stage_ft, mod$Q_cfs)
  
  #Estimate error between measured and modeled
  error<-meas %>% 
    #Rename Q_cfs
    rename(Q_meas = Q_cfs) %>% 
    #Estimate modeled value
    mutate(Q_mod = mod_fun(stage_ft)) %>% 
    #Estimate error as a proportion of modeled value
    mutate(error = (Q_meas-Q_mod)/Q_mod) %>% 
    #isolate error values
    dplyr::select(error) %>% 
    drop_na() %>% 
    pull()
  
  #run shapiro wilks test
  p_value<-shapiro.test(error)$p.value
  
  #export p value
  tibble(
    gage_id = gages$site_no[n],
    p_value)
}

#apply function
s_test<-lapply(seq(1,nrow(gages)), shapiro_fun) %>% bind_rows()

#how many gages above 0.05
s_test %>% filter(p_value>=0.01) %>% nrow()
  