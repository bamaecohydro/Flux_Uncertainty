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
library(RColorBrewer)
library(patchwork)

#Load data
df <- left_join(
  read_csv('data//gages.csv'), 
  read_csv("data//uncertainty_results.csv")) %>% 
  distinct()

#Remove duplicate (need to track these down at some point) 
df <- df %>% group_by(station_nm) %>% filter(!duplicated(station_nm)) %>% ungroup() %>% drop_na()

#Estimate error on an areal basis
df <- df %>% mutate(N_load_error = N_load_diff_kg/drain_area_va/258.999)  #convert to kg/ha

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# scatterplot with watershed area & error --------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Q error
#aspect ratio: 547x492
wa <- ggplot(data=df,aes(x=drain_area_va, y=Q_uncertainty))+
  geom_point(color="#2b8cbe",size=4,alpha=0.7)+
  scale_x_continuous(trans="log10")+
  scale_y_continuous(trans="log10")+
  geom_smooth(method="lm",se=FALSE,color="#e6550d",linetype="dashed", size=1)+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Q Relative Error (%)")+
  xlab("Drainage Area (ha)")+
  theme(axis.title = element_text(size = 14,color="black"),axis.text  = element_text(size = 12,color="black")) 

cor.test(log10(df$drain_area_va), log10(df$Q_uncertainty), method = c("pearson")) #run correlation for Q and drainage area


#NO3 error - NOT USED IN PUB
ggplot(data=df,aes(x=drain_area_va, y=N_load_error))+
  geom_point(color="#2b8cbe",size=5,alpha=0.7)+
  scale_x_continuous(trans="log10")+
  scale_y_continuous(trans="log10")+
  geom_smooth(method="lm",se=FALSE,color="#e6550d")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression(paste(NO["3"]~Load~Uncertainty~(kg~ha^{"-1"}~yr^{"-2"}))))+
  xlab("Drainage Area (ha)")+
  theme(axis.title = element_text(size = 14,color="black"),axis.text  = element_text(size = 12,color="black")) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# scatterplot with gauge age & error --------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#calc approx. gage age
df$gage_age <- as.numeric((df$end_date-df$begin_date)/365)

#plot Q uncertainty vs. gage age
#aspect ratio: 547x492
ga <- ggplot(data=df,aes(x=gage_age, y=Q_uncertainty))+
  geom_point(color="#2b8cbe",size=4,alpha=0.7)+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_continuous(trans="log10")+
  scale_y_continuous(trans="log10")+
  #geom_smooth(method="lm",se=FALSE,color="#e6550d",linetype="dashed", size=1)+ #removed bc correlation was not significant
  theme(axis.title.x = element_text(size = 14,color="black"),axis.text.x  = element_text(size = 12,color="black"))+
  theme(axis.text.y=element_blank(),axis.title.y=element_blank())+
  xlab("Gauge Age (Yrs)")
   

cor.test(log10(df$gage_age), log10(df$Q_uncertainty), method = c("pearson")) #run correlation for Q and gage age
#not significant


#########

#combine gage age and watershed area scatterplots with patchwork - aspect ratio: 1000x492
wa|ga

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot error in kg/ha/yr -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Inspo: https://r-graph-gallery.com/304-highlight-a-group-in-lollipop.html

#relationship between N error and Q uncertainty - aspect ratio: 674x546
ggplot(data=df,aes(x=Q_uncertainty, y=N_load_error))+
  geom_point(color="#2b8cbe",size=5,alpha=0.7)+
  scale_x_continuous(trans="log10")+
  scale_y_continuous(trans="log10")+
  geom_smooth(method="lm",se=FALSE,color="#e6550d",linetype="dashed", size=1)+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(expression(paste(NO["3"]~Load~Uncertainty~(kg~ha^{"-1"}~yr^{"-1"}))))+
  xlab("Q Relative Error (%)")+
  theme(axis.title = element_text(size = 14,color="black"),axis.text  = element_text(size = 12,color="black"))

#lm <- lm(log10(N_load_error)~log10(Q_uncertainty),data=df)
#summary(lm)

cor.test(log10(df$Q_uncertainty), log10(df$N_load_error), method = c("pearson"))

#Convert X and Y for ease of coding
df2 <- df %>% 
  mutate(
    x = site_no, 
    y = N_load_error,
    z=drain_area_va)
  
#reorder data
df2 <- df2 %>% select(x,y,z) %>% 
  arrange(y) %>%
  mutate(x=factor(x,x))


#plot with color by watershed size - aspect ratio: 559x789
df2 %>% 
  ggplot(aes(x=x, y=y,color=z)) +
  geom_segment(aes(x=x, xend=x, y=0, yend=y),size=1.1) + 
  geom_point(size=2)+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  coord_flip() +
  ylab(expression(paste(NO["3"]~Load~Uncertainty~(kg~ha^{"-1"}~yr^{"-1"}))))+
  xlab("USGS Gage No.")+
  scale_color_distiller(palette="RdYlBu",direction=-1,trans="log10",values=c(0,0.25,0.499,0.5,0.501,0.75,1))+
  theme(legend.title=element_blank(), legend.position=c(0.85,0.12))+
  theme(axis.title = element_text(size = 14,color="black"),axis.text  = element_text(size = 12,color="black")) 


#export
ggsave("docs//N_load_uncertainty.png", height = 10, width = 15, units = "in", dpi=300)


