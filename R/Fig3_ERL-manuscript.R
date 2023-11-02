#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Flow uncertainty figures
#Coder: Shannon Speir
#Date: 4/24/2023
#Purpose: Plots highlighting variation in relative error for NO3
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
df<-read_csv("sim.csv")

df$site_no <- df$gage

test <- left_join(
  df, 
  read_csv("gages.csv")) %>% 
  distinct()

test <- test %>% mutate(area_ha = drain_area_va/258.999)  #convert to ha

#Estimate error on an areal basis
test <- test %>% mutate(N_load_err_kgha = N_load_diff_kg/drain_area_va/258.999)  #convert to kg/ha


#average error by gage
mean <- df %>%
  group_by(gage) %>%
  summarize(mean.no3 = mean(N_load_diff_percent, na.rm=TRUE), abs.mean.no3.kg = mean(abs(N_load_diff_kg), na.rm=TRUE), 
            mean.q = mean(flow_ft3_meas, na.rm = TRUE))
summary(mean)

#Distribution of relative error
#aspect ratio: 547x492
mean %>% 
  ggplot(aes(x = mean.no3))+
  geom_density(fill="#2b8cbe",alpha=0.6)+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(  axis.title = element_text(size = 16,color="black"), axis.text  = element_text(size = 14,color="black"))+
  xlim(-145,240)+
  xlab(expression(paste(NO["3"]~Load~Uncertainty~("%"))))+
  ylab("Density")

##########

#scatterplot
ggplot(data=mean,aes(x=mean.q, y=abs.mean.no3.kg))+
  geom_point(color="#2b8cbe",size=5,alpha=0.7)+
  scale_x_continuous(trans="log10")+
  scale_y_continuous(trans="log10")+
  geom_smooth(method="lm",se=FALSE,color="#e6550d",linetype="dashed", size=1)+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression(paste(NO["3"]~Load~Uncertainty~(kg))))+
  xlab("Mean Total Annual Flow (ft3)")+
  theme(axis.title = element_text(size = 14,color="black"),axis.text  = element_text(size = 12,color="black"))

cor.test(log10(mean$mean.q), log10(mean$abs.mean.no3.kg), method = c("spearman"))




