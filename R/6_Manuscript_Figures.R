#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Manuscript Figures
#Coder: Nate Jones
#Date: 5/15/2021
#Purpose: Plots for JASM
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
df<-read_csv("data/results_hydro.csv")

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
