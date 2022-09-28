#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: JASM Plots
#Coder: Nate Jones
#Date: 7/31/2022
#Purpose: Initial Plots
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup workspace --------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear R environment
remove(list=ls())

#Load required packages
library(tidyverse)
library(ggplot2)
library(patchwork)
library(sf)

#Load data
states<-st_read("data/tl_2012_us_state/tl_2012_us_state.shp")
gages<-read_csv('data/gages.csv')
hydro<-read_csv("data/results_hydro.csv")
biogeo<-read_csv('data/results_biogeo.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Prep spatial data -------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Edit States shape
states <- states %>% 
  filter(
    NAME != 'Alaska', 
    NAME != 'Hawaii', 
    NAME != 'Guam', 
    NAME != 'Commonwealth of the Northern Mariana Islands', 
    NAME != 'American Samoa',
    NAME != 'Puerto Rico',
    NAME != 'United States Virgin Islands') %>% 
  st_transform(., crs="+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80") 

#Edit gages shape
gages <- gages %>% 
  st_as_sf(., 
           coords = c("dec_long_va", 'dec_lat_va'), 
           crs = '+proj=longlat +datum=WGS84 +no_defs') %>% 
  st_transform(., crs="+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 1. Gage locations -----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot()+
  geom_sf(data = states) + 
  geom_sf(data = gages, 
          color = '#9E1B32',
          alpha = 0.70, 
          pch=19, 
          cex=4) +
  theme_bw()

ggsave("docs/gage_location.png", width=7, height = 5, units = "in", dpi=300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 2. Mississippi River Examplar -----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Define gage of interest
msr<-"07374000"

#tidy data
hydro_msr<-hydro %>% 
  filter(gage == msr) %>% 
  filter(ag_level == 'annual') %>% 
  mutate(NO3_kg_sim = NO3_kg_sim/(10^6)) %>% 
  select(water_year, NO3_kg_sim) %>% 
  mutate(sim = 'hydro')
biogeo_msr<- biogeo %>% 
  filter(gage == msr) %>% 
  filter(ag_level == 'annual') %>% 
  mutate(NO3_kg_sim = NO3_kg_sim/(10^6)) %>% 
  select(water_year, NO3_kg_sim) %>% 
  mutate(sim = 'biogeo')
df<-bind_rows(hydro_msr, biogeo_msr)


#Annual MS River Plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df %>% 
  #Start ggplot object
  ggplot(aes(x=water_year, y = NO3_kg_sim , group = sim)) +
    #boxplot  
    # geom_jitter(
    #   width = 0.03, 
    #   alpha = 0.1
    # ) +
    geom_violin(
      fill = '#232D4B',
      outlier.alpha = 0)+
    #Theme blackwhite
    theme_bw() +
    #Change font size of axes
    theme(
      axis.title.y = element_text(size = 14), 
      axis.text.y  = element_text(size = 10),
      axis.title.x = element_text(size = 14), 
      axis.text.x = element_text(size = 10)
    ) + 
    #Add labels
    ylab("Total N Load [10^6 kg]") + 
    xlab('Year') 
