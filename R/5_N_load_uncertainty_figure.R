#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: N Load uncertainty figure
#Coder: Nate Jones
#Date: 3/18/2023
#Purpose: Plots highlighting variation in N load due to uncertainty in S-D Curve
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup workspace --------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear R environment
remove(list=ls())

#Load required packages
library(tidyverse)
library(ggplot2)
library(hrbrthemes)

#Load data
df <- left_join(
  read_csv('data//gages.csv'), 
  read_csv("data//uncertainty_results.csv")) %>% 
  distinct()

#Remove duplicate (need to track these down at some point)
df <- df %>% group_by(station_nm) %>% filter(!duplicated(station_nm)) %>% ungroup() %>% drop_na()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot error in kg/ha/yr -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Inspo: https://r-graph-gallery.com/304-highlight-a-group-in-lollipop.html

#Estimate error on an arial basis
df <- df %>% mutate(N_load_error = N_load_diff_kg/drain_area_va/258.999)  #convert to kg/ha

#Convert X and Y for ease of coding
df <- df %>% 
  mutate(
    x = station_nm, 
    y = N_load_error)
  
#reorder data
df <- df %>% select(x,y) %>% 
  arrange(y) %>%
  mutate(x=factor(x,x))

#Plot
df %>% 
  ggplot(aes(x=x, y=y)) +
  geom_segment(
    aes(x=x, xend=x, y=0, yend=y), 
    color="grey", 
    size=1.1) + 
  geom_point(
    col="grey",
    size=2
  )+
  theme_bw() +
  coord_flip() +
  xlab("") +
  ylab("") + 
  ggtitle("Uncertainty in N Loads (kg/ha/yr)")

#export
ggsave("docs//N_load_uncertainty.png", height = 10, width = 15, units = "in", dpi=300)



