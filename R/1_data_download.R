#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Data Download
#Coder: Nate Jones
#Date: 12/19/2021
#Purpose: Download data for analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Workspace -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear R environment
remove(list=ls())

#Load required packages
library('tidyverse')
library('dataRetrieval')
library('parallel')
library('lubridate')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Download Data -------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Identify gages with in-situ NO3 data --------------------------------------
#Define list of states to search gages
states<-dataRetrieval::stateCd$STATE_NAME

#Create download function
gage_fun<-function(n){
  
  #load required libraries 
  library(tidyverse)
  library(dataRetrieval)
  
  #Define state of interest
  state<-states[n]
  
  #Identify gages within state
  sites<-
    whatNWISsites(
      stateCd=state,
      parameterCd=c("99133"),
      hasDataTypeCd="iv") %>% 
    as_tibble() %>% 
    select(
      site_no,
      station_nm,
      dec_lat_va,
      dec_long_va
    )

  #Define period of record
  period<-lapply(
    X = sites$site_no,
    FUN = function(x){
      whatNWISdata(
        siteNumber = x, 
        service = "uv",
        parameterCd=c("99133"))
    }
  ) 

  #Bind list and tidy 
  period<-period %>% 
    bind_rows() %>% 
    select(
      site_no,
      begin_date,
      end_date
    )

  #Left join to sites tibble
  sites<-left_join(sites, period)
  
  #Export 
  sites
}

#Create error function
error_fun<-function(m){tryCatch(gage_fun(m), error=function(e) NULL)}

#apply function 
n_cores<-parallel::detectCores()-1
cl<-makeCluster(n_cores)
clusterExport(cl, c("gage_fun","states"))
gages<-parLapply(cl, seq(1, length(states)), error_fun)
stopCluster(cl)

#Clean up output
gages<-gages %>% bind_rows()

#Filter gages to atleast 3 years of NO3 data
gages<-gages %>% 
  mutate(dT=as.numeric(paste(end_date-begin_date))) %>% 
  filter(dT>=(365*3)) %>% 
  select(-dT) %>% 
  distinct()

#2.2 Stage-discharge data ------------------------------------------------------
#Create download function
sd_fun<-function(n){
  
  #call required libraries
  library(tidyverse)
  
  #Isolate gage of interest
  gage<-gages$site_no[n]
  
  #Download stage discharge measurements 
  url<-paste0("http://waterdata.usgs.gov/nwis/measurements?site_no=",gage,"&agency_cd=USGS&format=rdb") #URL address
  sd_raw<-read.table(url, sep = "\t", skip=14, header = TRUE)  #download
  sd_raw<-sd_raw[-1,] #delete first line (non data)
  
  #Tidy raw data
  sd_raw <- 
    tibble(
      stage_ft = as.numeric(paste(sd_raw$gage_height_va)), 
      Q_cfs    = as.numeric(paste(sd_raw$discharge_va))) %>% 
    mutate(
      site_no=gage,
      type="raw"
    )
  
  #Downloadd stage discharge modeled data
  url<-paste0("http://waterdata.usgs.gov/nwisweb/get_ratings?site_no=",gage,"&file_type=exsa")
  sd_model<-readLines(url)
  sd_model<-read.table(url, sep = "\t", skip=length(subset(sd_model, substr(sd_model,1,1)=="#")), header = TRUE)
  sd_model<-sd_model[-1,]
  
  #Tidy modeled data
  sd_model <- 
    tibble(
      stage_ft = as.numeric(paste(sd_model[,1])), 
      Q_cfs    = as.numeric(paste(sd_model[,3]))) %>% 
    mutate(
      site_no=gage,
      type="model"
    )
  
  #bind rows
  sd<-bind_rows(sd_raw, sd_model)
  
  #export sd data
  sd
}

#Create error function
error_fun<-function(m){tryCatch(sd_fun(m), error=function(e) NULL)}

#apply function 
n_cores<-parallel::detectCores()-1
cl<-makeCluster(n_cores)
clusterExport(cl, c("sd_fun","gages"))
sd<-parLapply(cl, seq(1, nrow(gages)), error_fun)
parallel::stopCluster(cl)

#Tidy data a bit
sd<-sd %>% 
  bind_rows(.) %>% 
  drop_na()

#Remove gages without raw or modeled data
gages<-sd %>% 
  group_by(site_no, type) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from=type, values_from=count, values_fill = NA) %>% 
  left_join(gages, .) %>% 
  drop_na()

#2.3 Gage data -----------------------------------------------------------------
#Create download function
ts_fun<-function(n){
  
  #Load packages of interest
  library(tidyverse)
  library(dataRetrieval)
  
  #Download data
  ts<-readNWISuv(
    siteNumbers = gages$site_no[n],
    parameterCd=c("00060","99133"),
    startDate = gages$begin_date[n],
    endDate = gages$end_date[n])

  #Tidy data
  ts<-ts %>% 
    select(
      site_no, 
      datetime = dateTime,
      NO3_ppm  = X_99133_00000,
      Q_cfs    = X_00060_00000) %>% 
    as_tibble()
  
  #Export
  ts
}

#Create error function
error_fun<-function(m){tryCatch(ts_fun(m), error=function(e) NULL)}

#Setup parallels
n_cores<-parallel::detectCores()-1
cl<-makeCluster(n_cores)
clusterExport(cl, c("ts_fun","gages"))
ts<-parLapply(cl, seq(1, nrow(gages)),error_fun)
stopCluster(cl)

#Bind rows and tidy
ts<-ts %>% bind_rows()


#2.4  Export data --------------------------------------------------------------
dir.create('temp')
write_csv(gages, "data/gages.csv")
write_csv(sd, "temp/sd.csv")
write_csv(ts, "temp/ts.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Download WQ Measurement Data-----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.1 Create function to water quality data 
wq_data<-function(n){
  
  #define gage of interest
  gage<-gages$site_no[n]
  
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
    ungroup() %>% 
    mutate(gage = gage)
  
  #export dataframe
  df
}

#3.2 Apply function
wq<-lapply(
  X=seq(1, nrow(gages)), 
  FUN = wq_data) %>% 
  bind_rows()

#3.3 write csv file to use later
write_csv(wq, "temp/wq_data.csv")

