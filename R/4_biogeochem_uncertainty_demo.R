#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Biogeochemistry Uncertainty Analysis Demo
#Coder: Nate Jones
#Date: 7/13/2021
#Purpose: Demo uncertainty in biogeochemistry measurements
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
gages <- read_csv('data/gages.csv')
ts <- read_csv("data/ts.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Develop Error Distributions -----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#See update on NWIS discrete sample download: https://cran.r-project.org/web/packages/dataRetrieval/vignettes/qwdata_changes.html
#USGS Parameter Codes here: https://help.waterdata.usgs.gov/parameter_cd?group_cd=NUT

#Define gage for Demo
gage <- "07374000" 

#Download discrete sample data
df <- readWQPqw(
  paste0("USGS-", gage), 
  parameterCd = c('00618'))

#Tidy data
df <- df %>% 
  #Create tibble
  as_tibble() %>% 
  #select cols of interest
  select(
    date      = ActivityStartDateTime,
    NO3_N_ppm = ResultMeasureValue) 

#Aggregate daily data
df <- df %>% 
  #aggreagate to day
  mutate(date = as_date(date)) %>% 
  group_by(date) %>% 
  summarise(
    NO3_sample = median(NO3_N_ppm, na.rm=T)) %>% 
  ungroup()
  
#Join  too larger ts
error <- ts %>% 
  filter(site_no == gage) %>% 
  #aggreagate to day
  mutate(date = as_date(datetime)) %>% 
  group_by(date) %>% 
  summarise(
    NO3_sonde = median(NO3_ppm, na.rm=T), 
    Q_cfs   = median(Q_cfs, na.rm=T)) %>% 
  ungroup() %>% 
  #apply left join
  left_join(., df) %>% 
  drop_na()

#Estimate relative error
error <- error %>% 
  mutate(error = (NO3_sonde-NO3_sample)/NO3_sample)

#Plot for funzies
par(mfrow=c(2,1))
plot(error$NO3_sample, error$NO3_sonde, type="n", 
     xlab = "Sample NO3 [ppm-N]", ylab = 'Sonde NO3 [ppm-N]', 
     cex.lab = 14/12, cex.axis = 10/12, ps=12)
abline(a=0, b=1, col='red', lty=2, lwd=2)
points(error$NO3_sample, error$NO3_sonde, pch=19, cex=0.75, col="grey30")
hist(error$error*100, xlab = "Error [%]", breaks=25, main = NULL)

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
      NO3_ppm = NO3_ppm*(1+error_sim),
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
# A) Stage Discharge Curve
# B) Residual Plot
# C) Resulting Error Distribution
# D) 2011 Hydrograph
# E) Event scale load estimates

#5.1 Stage-discharge -----------------------------------------------------------
one_2_one<-ggplot()+
  geom_abline(
    slope = 1, 
    intercept = 0, 
    lty=2, 
    lwd=1.1, 
    col="#e6550d"
  ) +
  geom_point(
    aes(x=error$NO3_sample, y=error$NO3_sonde), 
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
  xlab("Sample Nitrate [ppm-N]") + 
  ylab("Sonde Nitrate [ppm-N]") 

#5.2 Residuals Plot ------------------------------------------------------------
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
    aes(x=NO3_sample, y=error), 
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
  xlab("Sample NO3 [ppm-N]") + 
  ylab("Residual Error [%] ") 

#5.3 Error Distribution---------------------------------------------------------
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

#5.4 Simulated hydrographs------------------------------------------------------
biogeo_plot<-ggplot() + 
  #Add simulated flows
  geom_line(
    aes(x=sim$day, y=sim$NO3_ppm, group=sim$sim_n), 
    col= "#2b8cbe", 
    lwd=0.25, 
    alpha = 0.05
  ) + 
  #Add measured flow
  geom_line(
    aes(x=ts_gage$day, y=ts_gage$NO3_ppm), 
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
  ylab("Nitrate [ppm-N]") 

#5.5 N Export Plots ----------------------------------------------------------------
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

#5.6 Annual N export -----------------------------------------------------------
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

#5.6 Combine and export plots---------------------------------------------------
#Create plot with patchwork
(one_2_one + resdiual_plot) /(density_plot+biogeo_plot)/(total_plot+sum_plot)
ggsave("docs//baton_rouge_biogeo.jpg", height = 10, width = 7.5, units = "in", dpi=300)

