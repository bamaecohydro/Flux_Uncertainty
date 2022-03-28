#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Stage-Discharge Uncertainty Analysis
#Coder: Shannon Speir
#Date: 03/28/2022
#Purpose: Quantify uncertainty in stage-discharge uncertainty analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Workspace -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear R environment
remove(list=ls())

#Load required packages
library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(parallel)
library(patchwork)

#Load data
gages<-read_csv('data/gages.csv')
sd<-read_csv("data/sd.csv")
ts<-read_csv("data/ts.csv")

#Define gage for Demo
gage<-"07374000" 