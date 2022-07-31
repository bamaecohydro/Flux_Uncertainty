#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Biogeochem Uncertainty Analysis
#Coder: Nate Jones
#Date: 7/31/2021
#Purpose: Quantify uncertainty in sonde NO3 data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Workspace -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear R environment
remove(list=ls())

#Load required packages
library(tidyverse)
library(lubridate)
library(zoo)
library(dataRetrieval)
library(parallel)
library(patchwork)

#Load data
gages <- read_csv('data/gages.csv')
wq <- read_csv('data/wq_data.csv')
ts <- read_csv("data/ts.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Create sim function -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fun<-function(n){
  
  print(n)
  
  #define gage of interest ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gage<-gages$site_no[n]  

  #Quantify error between wwq measurements and sonde data~~~~~~~~~~~~~~~~~~~~~~~
  #subset wq data to from gage of interest
  wq_gage <- wq %>% filter(gage==gage)
  
  #Join  too larger ts
  error <- ts %>% 
    filter(site_no == gage) %>% 
    #aggregate to day
    mutate(date = as_date(datetime)) %>% 
    group_by(date) %>% 
    summarise(
      NO3_sonde = median(NO3_ppm, na.rm=T), 
      Q_cfs   = median(Q_cfs, na.rm=T)) %>% 
    ungroup() %>% 
    #apply left join
    left_join(., wq_gage) %>% 
    drop_na()
  
  #Estimate relative error
  error <- error %>% 
    mutate(error = (NO3_sonde-NO3_sample)/NO3_sample)
  
  # tidy data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Subset ts tibble to gage of interest
  ts_gage <- ts %>% filter(site_no==gage)  
  
  #Aggregate data to mean daily
  ts_gage <- ts_gage %>% 
    mutate(day=as_date(datetime)) %>% 
    group_by(day) %>% 
    summarise(
      Q_cfs=mean(Q_cfs, na.rm=T),
      NO3_ppm = mean(NO3_ppm, na.rm=T)) 
  
  #Add water year information
  ts_gage <- ts_gage %>% 
    ungroup() %>% 
    mutate(
      water_year = year(day),
      month = month(day)) %>% 
    mutate(
      water_year = if_else(
        month>=10, 
        water_year + 1, 
        water_year)) %>% 
    select(-month)
  
  #Remove water years with less than 300 days
  complete_years<-ts_gage %>% 
    group_by(water_year) %>% 
    summarise(days = n()) %>% 
    filter(days>300) %>% 
    select(water_year) %>% 
    pull()
  ts_gage<-ts_gage %>% filter(water_year %in% complete_years) %>% drop_na()
  remove(complete_years)
  
  #Resample Error Distribution~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  #Create df of resample error distribution
  error_resample<-tibble(
    n = seq(1,1000),
    error=sample(error$error, size = 1000, replace = T))
  
  #Create function to apply error to hydrograph
  sim_fun<-function(sim_n){
    
    #Define error term
    error_sim<-error_resample %>% 
      filter(n==sim_n) %>% 
      select(error) %>% pull()
    
    #Apply to hydorgraph
    sim<-ts_gage %>% 
      mutate(
        NO3_ppm = NO3_ppm*(1+error_sim),
        sim_n)
    
    #Export sim
    sim
  }
  
  #Apply function
  sim<-lapply(seq(1,1000), sim_fun) %>% bind_rows
  
  #Load Calculations
  #Estimate daily load (kg/day) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ts_gage<-ts_gage %>% 
    mutate(NO3_kg = Q_cfs*NO3_ppm*86400*(0.3048^3)*1000/(10^6))
  
  sim<-sim %>% 
    mutate(NO3_kg = Q_cfs*NO3_ppm*86400*(0.3048^3)*1000/(10^6))
  
  #Estimate annual loads 
  #Estiamte event-based loads
  annual_meas <- ts_gage %>% 
    group_by(water_year) %>% 
    summarise(NO3_kg_meas = sum(NO3_kg, na.rm = T))
  
  annual_sim <- sim %>% 
    group_by(sim_n, water_year) %>% 
    summarise(NO3_kg_sim = sum(NO3_kg, na.rm = T)) %>% 
    left_join(.,annual_meas) %>% 
    mutate(percent_diff = (NO3_kg_sim - NO3_kg_meas)/NO3_kg_meas*100) %>% 
    mutate(
      percent_total_load = NA, 
      ag_level = "annual"
    )
  
  # Export ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sim <- annual_sim %>% mutate(gage=gages$site_no[n])
  sim 
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Execute sim function ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove problematic gage
gages<-gages %>% filter(site_no != gages$site_no[44])

#Execute Function
df <- lapply(X = seq(1,nrow(gages)), fun) %>% bind_rows()

#Export
write_csv(df, "data//results_biogeo.csv")



