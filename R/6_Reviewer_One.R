#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Response to reviewer
#Coder: Nate Jones
#Date: 9/17/2023
#Purpose: Address response to reviews
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Setup environment -------------------------------------------------------------
#Clear R environment
remove(list=ls())

#Load required packages
library(tidyverse)
library(lubridate)
library(zoo)
library(dataRetrieval)
library(parallel)
library(patchwork)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comment #3 Are error distributions normally distributed ----------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Load data
gages<-read_csv('data/gages.csv')
sd<-read_csv("temp/sd.csv")
ts<-read_csv("temp/ts.csv")

#create simulation function
shapiro_fun<-function(n){
  
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
    mutate(error = (Q_meas-Q_mod)/Q_mod) %>% 
    #isolate error values
    dplyr::select(error) %>% 
    drop_na() %>% 
    pull()
  
  #run shapiro wilks test
  p_value<-shapiro.test(error)$p.value
  
  #export p value
  tibble(
    gage_id = gages$site_no[n],
    p_value)
}

#apply function
s_test<-lapply(seq(1,nrow(gages)), shapiro_fun) %>% bind_rows()

#how many gages above 0.05
s_test %>% filter(p_value>=0.01) %>% nrow()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comment #5 The nubmer of paired- stage-discharge measurement as a driver of error-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear R environment
remove(list=ls())

#load uncertainty results
df<-read_csv("data//uncertainty_results.csv")

#load sd curve
sd <- read_csv("temp/sd.csv")

#load ts data
ts <- read_csv("temp/ts.csv")

#estimate the number of stage-discharge curve measurements
sd <- sd %>% 
  filter(type=='raw') %>% 
  group_by(site_no) %>% 
  summarise(n_meas = n()) 

#Combine with df
df <- df %>% left_join(., sd)

#correlation test
cor.test(df$Q_uncertainty, df$n_meas, method = 'spearman')

#plot
sd_plot <- df %>% 
  filter(n_meas<3000) %>% 
  ggplot(aes(x = n_meas, y = Q_uncertainty)) + 
  #Add line data
  geom_point(col="steelblue2", pch=19, alpha=0.7, cex=3) +
  #Add predefined black/white theme
  theme_bw() +
  #Change font size of axes
  theme(
    axis.title = element_text(size = 14), 
    axis.text  = element_text(size = 10)
  ) + 
  #Add labels
  xlab("Number of Stage-Discharge Measurements") + 
  ylab("Q Relative Error (%)") 

#define length of flow record
ts <- ts %>% 
  mutate(date = lubridate::date(datetime)) %>% 
  group_by(site_no, date) %>% 
  summarise(Q_cfs = mean(Q_cfs, na.rm=T))

#Determine length of flow record
ts <- ts %>% 
  group_by(site_no) %>% 
  summarise(age_yr = n()/365)

#Combine with master df 
df <- df %>% left_join(., ts)

#correlation test
cor.test(df$Q_uncertainty, df$age_yr, method = 'spearman')

#plot
age_plot <- df %>% 
  filter(n_meas<3000) %>% 
  ggplot(aes(x = age_yr, y = Q_uncertainty)) + 
  #Add line data
  geom_point(col="steelblue2", pch=19, alpha=0.7, cex=3) +
  #Add predefined black/white theme
  theme_bw() +
  #Change font size of axes
  theme(
    axis.title = element_text(size = 14), 
    axis.text  = element_text(size = 10)
  ) + 
  #Add labels
  xlab("Gauge Record Length (Years)") + 
  ylab("Q Relative Error (%)") 

age_plot + sd_plot + plot_annotation(tag_levels = "A")

ggsave("docs/si_fig.png", 
        width = 12, 
        height = 5.15, 
        units = "in",
        dpi=300)
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comment #6 Checking ws area in Figure 1 --------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear R environment
remove(list=ls())

#load uncertainty results
df    <- read_csv("data//uncertainty_results.csv")
gages <- read_csv("data//gages.csv")

#recreate figure 1c
df %>% 
  left_join(., gages) %>% 
  ggplot(aes(x=drain_area_va, y = Q_uncertainty )) + 
  geom_point() +#Add predefined black/white theme
  theme_bw() +
  #Change font size of axes
  theme(
    axis.title = element_text(size = 14), 
    axis.text  = element_text(size = 10)
  ) + 
  #Add labels
  xlab("watershed area") + 
  ylab("Q Relative Error (%)")+ 
  #Scale axes
  scale_x_log10()

#wabash
