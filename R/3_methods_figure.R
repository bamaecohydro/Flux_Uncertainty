#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Methods Figure
#Coder: Nate Jones
#Date: 3/18/2023
#Purpose: Develop figures that demonstrate methods used in uncertainty analysis
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
library(tigris)
library(sf)

#Load data
gages<-read_csv('temp/gages.csv')
sd<-read_csv("temp/sd.csv")
ts<-read_csv("temp/ts.csv")

#Define gage for Demo
gage<-"07374000" 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Develop Error Distributions -----------------------------------------------
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
#3.0 Cascade Error distribution through hydrograph -----------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.1 Subset ts to gage and time period of interest------------------------------
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
         
#3.2 Resample Error Distribution -----------------------------------------------
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
#4.0 Biogeochemistry -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.1 Estimate daily load (kg/day)  ---------------------------------------------
sim<-sim %>% 
  mutate(NO3_kg = Q_cfs*NO3_ppm*86400*(0.3048^3)*1000/(10^6))

ts_gage<-ts_gage %>% 
  mutate(NO3_kg = Q_cfs*NO3_ppm*86400*(0.3048^3)*1000/(10^6))

#4.2 Estimate cummulative annual load ------------------------------------------
#Gage data
ts_gage<-ts_gage %>% 
  drop_na() %>% 
  mutate(N_tot = cumsum(NO3_kg)/10^6)

#Create function to estimate by sim
cumsum_fun<-function(n){
  #Add cummulative sum col
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 Plots----------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create a multi-panel plot
# 5.1 Gage Location Figure
# 5.2 Stage Discharge Curve
# 5.3 Residual Plot
# 5.4 Resulting Error Distribution
# 5.5 2011 Hydrograph
# 5.6 Event scale load estimates

#5.1 Gage Location -------------------------------------------------------------
#Download state shapefiles
states <- states() %>% 
  filter(
    NAME != 'Alaska', 
    NAME != 'Hawaii', 
    NAME != 'Guam', 
    NAME != 'Commonwealth of the Northern Mariana Islands', 
    NAME != 'American Samoa',
    NAME != 'Puerto Rico',
    NAME != 'United States Virgin Islands') %>% 
  st_transform(., crs="+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80") 

#Convert gages into sf 
gages <-   gages %>% 
  st_as_sf(
    coords = c("dec_long_va", "dec_lat_va"), 
    crs = '+proj=longlat +datum=WGS84 +no_defs') %>% 
  st_transform(., crs="+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80") 


#Plot
ggplot()+
  geom_sf(data = states) + 
  geom_sf(data = gages, 
          color = '#E57200', 
          alpha = 0.70, 
          pch=19, 
          cex=4) +
  theme_bw()

ggsave("docs/methods_gage_location.png", width=7, height = 5, units = "in", dpi=300)


#5.2 Stage-discharge -----------------------------------------------------------
rating_curve<-ggplot()+
  geom_line(
    aes(x=mod$stage_ft, y=mod$Q_cfs),
    lty=2,
    lwd=1.1, 
    col ="#e6550d") +
  geom_point(
    aes(x=meas$stage_ft, y=meas$Q_cfs), 
    pch=19, 
    col="#2b8cbe",
    alpha = 0.7) +
  #Log Axis
  scale_y_log10()+
  #Add predefined black/white theme
  theme_bw() +
  #Change font size of axes
  theme(
    axis.title = element_text(size = 14), 
    axis.text  = element_text(size = 10)
  ) + 
  #Add labels
  xlab("Stage [ft]") + 
  ylab("Discharge [cfs]") 

#Export
ggsave("docs//methods_rating_curve.png", height = 3.33, width = 3.75, units = "in", dpi=300)

#5.3 Residuals Plot ------------------------------------------------------------
resdiual_plot<-error %>% 
  mutate(error = error*100) %>% 
  ggplot() +
    #Add zero line  
    geom_hline(
      yintercept = 0, 
      lwd=1.1,
      lty=2, 
      col="#e6550d") +
    #Add points
    geom_point(
      aes(x=Q_mod, y=error), 
      pch=19, 
      col="#2b8cbe",
      alpha = 0.7) +
    #Add predefined black/white theme
    theme_bw() +
    #Change font size of axes
    theme(
      axis.title = element_text(size = 14), 
      axis.text  = element_text(size = 10)
    ) + 
    #Add labels
    xlab("Discharge [cfs]") + 
    ylab("Residual Error [%] ") 

#Export
ggsave("docs//methods_residual_plot.png", height = 3.33, width = 3.75, units = "in", dpi=300)

    
#5.4 Error Distribution---------------------------------------------------------
density_plot<-error %>% 
  mutate(error = error*100) %>% 
  ggplot() + 
  geom_density(
    aes(error),
    fill="#2b8cbe", 
    alpha=0.7) +
  #Add predefined black/white theme
  theme_bw() +
  #Change font size of axes
  theme(
    axis.title = element_text(size = 14), 
    axis.text  = element_text(size = 10)
  ) + 
  #Add labels
  xlab("Residual Error [%]") + 
  ylab("Density") 

#Export
ggsave("docs//methods_density_plot.png", height = 3.33, width = 3.75, units = "in", dpi=300)


#5.5 Simulated hydrographs------------------------------------------------------
hydro_plot<-ggplot() + 
  #Add simulated flows
  geom_line(
    aes(x=sim$day, y=sim$Q_cfs, group=sim$sim_n), 
    col= "#2b8cbe", 
    lwd=0.25, 
    alpha = 0.05
  ) + 
  #Add measured flow
  geom_line(
    aes(x=ts_gage$day, y=ts_gage$Q_cfs), 
    col="#e6550d",
    lwd=0.9) + 
  #Scale y-axis
  scale_y_log10()+
  #Add predefined black/white theme
  theme_bw() +
  #Change font size of axes
  theme(
    axis.title = element_text(size = 14), 
    axis.text  = element_text(size = 10)
  ) + 
  #Add labels
  xlab("Date") + 
  ylab("Discharge [cfs]") 

#Export
ggsave("docs//methods_hydro_plot.png", height = 3.33, width = 3.75, units = "in", dpi=300)


#5.6 N Export Plots ----------------------------------------------------------------
total_plot<-ggplot() + 
  #ADd sim export
  geom_line(
    aes(x=sim$day, y=sim$N_tot, group=sim$sim_n), 
    col= "#2b8cbe", 
    lwd=0.25, 
    alpha = 0.05
  ) +
  #Add gage export
  geom_line(
    aes(x=ts_gage$day, y=ts_gage$N_tot), 
    col="#e6550d",
    lwd=0.9) + 
  #Add predefined black/white theme
  theme_bw() +
  #Change font size of axes
  theme(
    axis.title = element_text(size = 14), 
    axis.text  = element_text(size = 10)
  ) + 
  #Add labels
  xlab("Date") + 
  ylab(expression("Total N Export [kg x 10 "^6*"]")) 

#Export
ggsave("docs//methods_total_n_export.png", height = 3.33, width = 3.75, units = "in", dpi=300)

#5.7 Annual N export -----------------------------------------------------------
sum_plot<-sim %>% 
  group_by(sim_n) %>% 
  summarise(N_tot = sum(NO3_kg, na.rm = T)/10^6) %>% 
  ggplot()+
  #ADd dist data
  geom_density(
    aes(N_tot),
    fill="#2b8cbe", 
    alpha=0.7) +
  #Add gage data
  geom_vline(
    xintercept = sum(ts_gage$NO3_kg/10^6, na.rm=T),
    col="#e6550d", 
    lty=2, 
    lwd=1.1)+
  #Add predefined black/white theme
  theme_bw() +
  #Change font size of axes
  theme(
    axis.title = element_text(size = 14), 
    axis.text  = element_text(size = 10)
  ) + 
  #Add labels
  xlab(expression("Total N Export [kg x 10"^6*"]")) +
  ylab("Density") 

#Export
ggsave("docs//methods_annual_N_export.png", height = 3.33, width = 3.75, units = "in", dpi=300)

#5.8 Combine and export plots---------------------------------------------------
#Create plot with patchwork
(rating_curve + resdiual_plot) /(density_plot+hydro_plot)/(total_plot+sum_plot)
ggsave("docs//methods.png", height = 10, width = 7.5, units = "in", dpi=300)
