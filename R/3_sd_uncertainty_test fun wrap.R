#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Stage-Discharge Uncertainty Analysis
#Coder: Shannon Speir
#Date: 4/15/2022
#Purpose: Quantify uncertainty in stage-discharge uncertainty analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Workspace -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear R environment
remove(list=ls())

#Load required packages
library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(parallel)
library(patchwork)

#Load data
gages<-read_csv('data/gages.csv')
sd<-read_csv("data/sd.csv")
ts<-read_csv("data/ts.csv")

#Pull out 5 gages for test function
gage<-c("07374000", "07144100", "01632900", "02319302", "03343820")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Create function to run for all 5 gages called in line 27-------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

gage_fun <- function(gage){

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Develop Error Distributions -----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#tidy data
sd_gage<-sd %>% filter(site_no==gage) 
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Cascade Error distribution through hydrograph -----------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.1 Subset ts to gage and time period of interest------------------------------
ts_gage<-ts %>% 
  filter(site_no==gage) %>% 
  mutate(day=as_date(datetime)) %>% 
  group_by(day) %>% 
  summarise(
    Q_cfs=mean(Q_cfs, na.rm=T),
    NO3_ppm = mean(NO3_ppm, na.rm=T)
  ) %>% 
  ungroup() %>% 
  mutate(
    water_year = year(day),
    month = month(day)) %>% 
  mutate(
    water_year = if_else(
      month>=10, 
      water_year + 1, 
      water_year)) %>% 
  filter(water_year==2020) %>% 
  select(-water_year) %>% select(-month)
         
#4.2 Resample Error Distribution -----------------------------------------------
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 Biogeochemistry -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.1 Estimate daily load (kg/day)  ---------------------------------------------
sim<-sim %>% 
  mutate(NO3_kg = Q_cfs*NO3_ppm*86400*(0.3048^3)*1000/(10^6))

ts_gage<-ts_gage %>% 
  mutate(NO3_kg = Q_cfs*NO3_ppm*86400*(0.3048^3)*1000/(10^6))

#5.2 Estimate cumulative annual load ------------------------------------------
#Gage data
ts_gage<-ts_gage %>% 
  drop_na() %>% 
  mutate(N_tot = cumsum(NO3_kg)/10^6)

#Create function to estimate by sim
cumsum_fun<-function(n){
  #Add cumulative sum col
  sim_n<-sim %>% 
    filter(sim_n==n) %>% 
    drop_na() %>% 
    mutate(N_tot = cumsum(NO3_kg)) %>% 
    mutate(N_tot = N_tot/10^6) %>% 
    select(day, sim_n, N_tot)
  
  #Export 
  sim_n
}

#Apply function
cumsum<-
  lapply(
    X=seq(1,1000), 
    FUN=cumsum_fun) %>% 
  bind_rows()

#left join to sim
sim<-left_join(sim, cumsum)

#Add gage info
sim <- sim %>% mutate(gage = gage)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#6.0 Run Function for all 5 gages----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

gage_est <- lapply(X=gage, FUN=gage_fun) %>% 
  bind_rows()
