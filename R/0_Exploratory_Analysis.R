#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Exploratory Analysis 
#Coder: Nate Jones
#Date: 12/19/2021
#Purpose: Explore how uncertainty in discharge estimates impacts flux estimates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Table of contents
# Stpe 1: Setup workspace
# Step 2: Download Data

#stage-discharge curve uncertainty analysis
#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Workspace ---------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear R environment
remove(list=ls())

#Load required packages
library('tidyverse')
library('dataRetrieval')
library('parallel')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Download Data -----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Identify gages with in-situ NO3 data ------------------
#Search gages by state
sites<-lapply(
  #Get list of USGS State Codes
  X= dataRetrieval::stateCd$STATE_NAME[1:51], 
  #Create function to apply to each state
  FUN = function(x){
    whatNWISsites(
      stateCd=x,
      parameterCd=c("99133"),
      hasDataTypeCd="iv")})

#Bind list and tidy
sites<-sites %>% 
  bind_rows() %>% 
  as_tibble() %>% 
  select(
    site_no,
    station_nm,
    dec_lat_va,
    dec_long_va
  )

#Period of record
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

#Write list of sites in data folder
write_csv(sites, "data/sites.csv")
 
#2.2 Download data -----------------------------------------
#Setup parallels
n_cores<-parallel::detectCores()-1
cl<-makeCluster(n_cores)
clusterExport(cl, "sites")

#Search gages by state
ts<-parLapply(
  cl, 
  #Get list of USGS State Codes
  seq(1, nrow(sites)),
  #Create function to apply to each state
  function(n){
    dataRetrieval::readNWISuv(
      siteNumbers = sites$site_no[n],
      parameterCd=c("00060","99133"),
      startDate = sites$begin_date[n],
      endDate = sites$end_date[n])
    }
  )

#Stop clusters
parallel::stopCluster(cl)

#Bind rows and tidy
ts<-ts %>% 
  bind_rows() %>% 
  select(
    site_no, 
    datetime = dateTime,
    NO3_ppm  = X_99133_00000,
    Q_cfs    = X_00060_00000)





















#2.2 Download gage data ------------------------------------
#Create download function
gage<-readNWISuv(
  site = sites$site_no, 
  parameterCd = "99133")
  
#select columns of interest
gage<-gage %>%
  select(
    site_no,
    datetime = dateTime,
    flow_cfs = X_00060_00000,
    no3_ppm  = X_99133_00000)

#For testing
gage<-gage %>% filter()
View(gage)



parameterCd=c("99133")
