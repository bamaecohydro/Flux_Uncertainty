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
sd<-read_csv("temp/sd.csv")
ts<-read_csv("temp/ts.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Create sim function -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create function
sim_fun<-function(n){
  
  #tidy data and create error distributions~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
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
      water_year = if_else(+
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
  
  #Resample Error Distribution 
  #Create df of resampled error distribution
  error_resample<-tibble(
    n = seq(1,1000),
    error= rnorm(1000, mean = mean(error$error, na.rm=T), sd = sd(error$error, na.rm=T)))
  
  #Create function to apply error to hydrograph
  sim_fun <- function(sim_n){
  
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

  #Estiamte error at annual sclae ~~~~!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Add gage data 
  sim <- sim %>% 
    left_join( 
      ts_gage %>% 
        select(day, Q_cfs) %>% 
        rename(Q_meas = Q_cfs))
  
  #Estimate daily loads
  sim <- sim %>% 
    mutate(
      flow_ft3_sim    = Q_cfs*86400,
      flow_ft3_meas = Q_meas*86400,
      NO3_kg_sim    = Q_cfs*NO3_ppm*86400*(0.3048^3)*1000/(10^6),
      NO3_kg_meas = Q_meas*NO3_ppm*86400*(0.3048^3)*1000/(10^6))
  
  #Aggregate annually
  sim <- sim %>% 
    group_by(water_year, sim_n) %>% 
    summarise(
      flow_ft3_sim    = sum(flow_ft3_sim, na.rm=T),
      flow_ft3_meas = sum(flow_ft3_meas, na.rm = T), 
      NO3_kg_sim    = sum(NO3_kg_sim, na.rm = T),
      NO3_kg_meas = sum(NO3_kg_meas, na.rm=T)) %>% 
    mutate(
      flow_diff_cm = ((flow_ft3_sim - flow_ft3_meas)*(30.48^3))/(gages$drain_area_va[n]*160934^2),
      flow_diff_percent = (flow_ft3_sim - flow_ft3_meas)/flow_ft3_meas*100,
      N_load_diff_kg = (NO3_kg_sim - NO3_kg_meas),
      N_load_diff_percent = (NO3_kg_sim - NO3_kg_meas)/NO3_kg_meas*100
    )
    
  #Export Results
  sim$gage <- gages$site_no[n]
  sim
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Execute sim function ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Execute Function
df <- lapply(X = seq(1,nrow(gages)), sim_fun) %>% bind_rows()

#Export
write_csv(df, "data//sim.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Subset data for plots -----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Read results files
df <- read_csv("data//sim.csv")

#Summarize range of uncertainty by gage
output<-df %>% 
  group_by(gage) %>% 
  summarise(
    Q_uncertainty = mean(abs(flow_diff_percent), na.rm=T),
    N_load_diff_kg = mean(abs(N_load_diff_kg), na.rm=T)) %>% 
  ungroup() %>% 
  rename(site_no = gage) %>% 
  filter(Q_uncertainty<200) 

#Export
write_csv(output, "data//uncertainty_results.csv")

