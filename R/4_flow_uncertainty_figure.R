#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Flow uncertainty figures
#Coder: Nate Jones
#Date: 3/18/2023
#Purpose: Plots highlighting variation in relative error
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
library(tigris)

#Load data
states<-states()
gages<-read_csv('data//gages.csv')
df<-read_csv("data//uncertainty_results.csv")

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
  st_transform(., crs="+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80") %>% 
  left_join(df)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create Plots -----------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Map of relative error
map_plot <- ggplot()+
  geom_sf(data = states) + 
  geom_sf(data = gages, 
          color = '#E57200', 
          alpha = 0.70, 
          size = gages$Q_uncertainty/20+1) +
  theme_bw()

#Distribution of relative error
pdf_plot <- gages %>% 
  ggplot(aes(x = Q_uncertainty)) +
  geom_histogram(
    aes(y = ..density..), 
    bg="grey70", 
    binwidth = 10) +
  geom_density(
    adjust=2,
    color="#E57200", 
    lwd=1.2)+
  theme_bw() +
  #Change font size of axes
  theme(
    axis.title = element_text(size = 14), 
    axis.text  = element_text(size = 10)
  ) + 
  #Add labels
  xlab('Relative Error (%)')+
  ylab("Frequency") +
  #Add annotations
  annotate("text", x=125, y = 0.013, label = paste0('Median=',round(median(gages$Q_uncertainty, na.rm=T),1), "%"), size = 6)

#Export final plot
#Create plot with patchwork
map_plot + pdf_plot + plot_annotation(tag_levels = c("A"), tag_suffix = ")")
ggsave("docs//flow_uncertainty.png", height = 4, width = 7.5, units = "in", dpi=300)
