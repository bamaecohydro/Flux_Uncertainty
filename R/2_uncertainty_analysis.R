#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Stage-Discharge Uncertainty Analysis
#Coder: Nate Jones
#Date: 5/12/2021
#Purpose: Quantify uncertainty in stage-discharge uncertainty analysis
#         across event, annual, and total flux scales for one gage
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
gages<-read_csv('data/gages.csv')
sd<-read_csv("data/sd.csv")
ts<-read_csv("data/ts.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Create sim function -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sim_fun<-function(n){
  
  #tidy data and create error distributions~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #tidy data
  sd_gage<-sd %>% filter(site_no==gages$site_no[n]) 
  meas<-sd_gage %>% filter(type=='raw')
  mod<-sd_gage %>% filter(type=='model')
  
  #create interpolation function
  mod_fun<-approxfun(mod$stage_ft, mod$Q_cfs)
  
  #Estimate error between measured and modeled
  error<-meas %>% 
    #Rename Q_cfs
    rename(Q_meas = Q_cfs) %>% 
    #Estimate modeled value
    mutate(Q_mod = mod_fun(stage_ft)) %>% 
    #Estimate error as a proportion of modeled value
    mutate(error = (Q_meas-Q_mod)/Q_mod)

  # Cascade Error distribution through hydrograph~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #Subset ts tibble to gage of interest
  ts_gage <- ts %>% filter(site_no==gages$site_no[n])  
  
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
  
  #Identify local peaks
  ts_gage <- ts_gage %>% 
    #Apply rolling faverage filter to 
    mutate(Q_smooth = rollapply(Q_cfs, 10, mean, partial = T)) %>% 
    #identify local peaks
    mutate(
      Q_fwd = Q_smooth - lag(Q_smooth), 
      Q_bck = lead(Q_smooth) - Q_smooth, 
      peak = if_else(Q_fwd>0 & Q_bck<0, 1, 0)) %>% 
    select(-c(Q_fwd, Q_bck)) %>% 
    #remove peaks less than 90% quantile
    mutate(
      peak_filter = if_else(Q_smooth>quantile(Q_smooth, 0.90, na.rm=T), 1, 0),
      peak = peak*peak_filter
    ) %>%
    select(-peak_filter) %>% 
    #Define individual events
    mutate(peak_event = cumsum(peak) + 1)
  
  #Define min between peaks
  trough <- ts_gage %>%
    group_by(peak_event) %>% 
    slice(which.min(Q_smooth)) %>% 
    mutate(trough = 1) %>% 
    select(day, trough) %>% ungroup()
  ts_gage <- ts_gage %>% 
    left_join(., trough) %>% 
    mutate(trough = if_else(is.na(trough), 0, trough)) 
    
  #Define individual events
  ts_gage<-ts_gage %>% 
    mutate(event_id = cumsum(trough) + 1) %>% 
    select(-c(Q_smooth, peak, peak_event, trough))

  #Resample Error Distribution 
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
        Q_cfs = Q_cfs*(1+error_sim),
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

  #Estimate event based loads 
  #Estimate event loads
  event_meas <- ts_gage %>% 
    group_by(water_year) %>% 
    mutate(total_load = sum(NO3_kg)) %>% 
    ungroup() %>% 
    group_by(event_id) %>% 
    summarise(
      NO3_kg_meas = sum(NO3_kg, na.rm = T),
      total_load = mean(total_load, na.rm = T))
  
  event_sim <- sim %>% 
    group_by(sim_n, event_id) %>% 
    summarise(NO3_kg_sim = sum(NO3_kg, na.rm = T)) %>% 
    left_join(.,event_meas) %>% 
    mutate(
      percent_diff = (NO3_kg_sim - NO3_kg_meas)/NO3_kg_meas*100,
      percent_total_load = (NO3_kg_sim - NO3_kg_meas)/total_load*100) %>% 
    mutate(
      ag_level = "event"
    )

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
  sim <- bind_rows(annual_sim, event_sim) %>% mutate(gage=gages$site_no[n])

  sim 
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Execute sim function ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Execute Function
df <- lapply(X = seq(1,nrow(gages)), sim_fun) %>% bind_rows()

#Export
write_csv(df, "data//results_hydro.csv")


