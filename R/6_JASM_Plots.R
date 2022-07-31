#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: JASM Plots
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Gage location figure ---------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot()+
  geom_sf(data = states) + 
  geom_sf(data = gages, 
          color = '#E57200', 
          alpha = 0.70, 
          pch=19, 
          cex=4) +
  theme_bw()

ggsave("docs/gage_location.png", width=7, height = 5, units = "in", dpi=300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Annual variation figures------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Mississippi River Gage --------------------------------------------------------
#Define gage of interest
msr<-"07374000"

#Annual MS River Plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
total<-df %>% 
  filter(gage == msr) %>% 
  filter(ag_level == 'annual') %>% 
  mutate(NO3_kg_sim = NO3_kg_sim/(10^6)) %>% 
  #Start ggplot object
  ggplot(aes(x=water_year, y = NO3_kg_sim , group = water_year)) +
    #boxplot  
    geom_jitter(
      width = 0.03, 
      alpha = 0.1
    ) +
    geom_violin(
      fill = '#232D4B',
      outlier.alpha = 0)+
    #Theme blackwhite
    theme_bw() +
    #Change font size of axes
    theme(
      axis.title.y = element_text(size = 14), 
      axis.text.y  = element_text(size = 10),
      axis.title.x = element_blank(), 
      axis.text.x = element_blank()
    ) + 
    #Add labels
    ylab("Total N Load [10^6 kg]") + 
    xlab('Year') 

diff<-df %>% 
  filter(gage == msr) %>% 
  filter(ag_level == 'annual') %>% 
  mutate(
    diff = NO3_kg_meas - NO3_kg_sim,
    diff = diff/(10^6)) %>% 
  #Start ggplot object
  ggplot(aes(x=water_year, y = diff , group = water_year)) +
  geom_hline(yintercept = 0, lty=2, col="grey10", lwd=1.2) + 
  #boxplot  
  geom_jitter(
    width = 0.03, 
    alpha = 0.1
  ) +
  geom_violin(fill = '#E57200')+
  #Theme blackwhite
  theme_bw() +
  #Change font size of axes
  theme(
    axis.title = element_text(size = 14), 
    axis.text  = element_text(size = 10), 
    axis.title.x = element_blank()
  ) + 
  #Add labels
  ylab("Difference [10^6 kg]") + 
  xlab('Year') + 
  #Clip plot
  coord_cartesian(ylim = c(-130, 130))

#Combine plots
total/diff + plot_layout(heights = c(2,1.25))
ggsave("docs/missippi_annual.jpg", width=5, height = 8, units = "in", dpi=300)

#Distribution of Annual Loads
q_25 <- df %>% 
  filter(gage == msr) %>% 
  filter(ag_level == 'annual') %>% 
  filter(percent_diff>-200, 
         percent_diff<200) %>% 
  select(percent_diff) %>% 
  summarise(quantile(percent_diff, 0.25)) %>% pull()
q_75 <- df %>% 
  filter(gage == msr) %>% 
  filter(ag_level == 'annual') %>% 
  filter(percent_diff>-200, 
         percent_diff<200) %>% 
  select(percent_diff) %>% 
  summarise(quantile(percent_diff, 0.75)) %>% pull()
df %>% 
  filter(gage == msr) %>% 
  filter(ag_level == 'annual') %>% 
  filter(percent_diff>-200, 
         percent_diff<200) %>% 
  select(percent_diff) %>% 
  ggplot(aes(x = percent_diff)) +
  geom_histogram(
    aes(y = ..density..), 
    binwidth = 2, 
    bg="grey70") +
  geom_density(
    adjust=2,
    color="#E57200", 
    lwd=1.2)+
  geom_vline(
    xintercept = q_25,
    lwd=1.1, 
    lty=2, alpha = 0.5)+
  geom_vline(
    xintercept = q_75,
    lwd=1.1, 
    lty=2, alpha = 0.5)+
  theme_bw() +
  #Change font size of axes
  theme(
    axis.title = element_text(size = 14), 
    axis.text  = element_text(size = 10)
  ) + 
  #Add labels
  xlab('% Difference')+
  ylab("Frequency") + 
  #Add anotation
  annotate("text", x=16, y = 0.11, label = paste0('IQR=',round(q_75-q_25, 2), "%"), size = 8)
ggsave("docs/missippi_annual_dist.jpg", width=4, height = 3, units = "in", dpi=300)

#Map of IQR --------------------------------------------------------------------
#Estimate IQR
iqr<-df %>% 
  filter(ag_level == 'annual') %>% 
  group_by(gage) %>% 
  summarise(IQR = quantile(percent_diff, 0.75) - quantile(percent_diff, 0.25)) %>% 
  ungroup() %>% 
  rename(site_no = gage) %>% 
  filter(IQR<200) 
gages<-left_join(gages, iqr)

#Plot Map
ggplot()+
  geom_sf(data = states) + 
  geom_sf(data = gages, 
          color = '#E57200', 
          alpha = 0.70, 
          size = gages$IQR/10+1) +
  theme_bw()
ggsave("docs/gage_iqr.png", width=7, height = 5, units = "in", dpi=300)

#Plot PDF
iqr %>% 
  ggplot(aes(x = IQR)) +
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
  xlab('Interquartile Range (%)')+
  ylab("Frequency") +
  #Add annotations
  annotate("text", x=125, y = 0.013, label = paste0('Median=',round(median(iqr$IQR),1), "%"), size = 6)
ggsave("docs/iqr_dist.jpg", width=4, height = 3, units = "in", dpi=300)

#Distribution of annual %Diff---------------------------------------------------
df %>% 
  filter(ag_level == 'annual') %>% 
  filter(percent_diff>-200) %>% 
  filter(percent_diff<200) %>% 
  filter(percent_diff!=-100) %>% 
  select(percent_diff) %>% 
  ggplot(aes(x = percent_diff)) +
  geom_histogram(
    aes(y = ..density..), 
    binwidth = 10, 
    bg="grey70") +
  geom_density(
    adjust=2,
    color="#E57200", 
    lwd=1.2)+
  #Theme Black White
  theme_bw() +
  #Change font size of axes
  theme(
    axis.title = element_text(size = 14), 
    axis.text  = element_text(size = 10)
  ) + 
  #Add labels
  xlab('% Difference [Annual]')+
  ylab("Frequency") 
  #Add anotation
ggsave("docs/annual_dist.jpg", width=4, height = 3, units = "in", dpi=300)

#Distribution of event %Diff---------------------------------------------------
df %>% 
  filter(ag_level == 'event') %>% 
  filter(percent_diff>-200) %>% 
  filter(percent_diff<200) %>% 
  filter(percent_diff!=-100) %>% 
  select(percent_diff) %>% 
  ggplot(aes(x = percent_diff)) +
  geom_histogram(
    aes(y = ..density..), 
    binwidth = 2.5, 
    bg="grey70") +
  geom_density(
    adjust=2,
    color="#E57200", 
    lwd=1.2)+
  #Theme Black White
  theme_bw() +
  #Change font size of axes
  theme(
    axis.title = element_text(size = 14), 
    axis.text  = element_text(size = 10)
  ) + 
  #Add labels
  xlab('% of Annual Load')+
  ylab("Frequency") 
#Add anotation
ggsave("docs/event_dist.jpg", width=4, height = 3, units = "in", dpi=300)

