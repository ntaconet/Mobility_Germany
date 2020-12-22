# This is the meta script to analyse the data.
# Put it in a folder "Mobility_Germany", with 
# 1/ the folder "CSV" that contains Mobility data
# 2/ a folder "Other_input" that will receive the other input we will need (emission factors, ...)
rm(list = ls())

# Set directory:
setwd("~/GitHub/Mobility_Germany/Mobility_data")


# Import necessary library
library(readxl) # to read excel files
library(tidyverse) # lots of useful packages
library("Hmisc")
library(rpart) # perform regression trees
library(rpart.plot) # plot regression trees
library("quantreg") # quantile regression package
#library(officer)
#library(flextable)
library(jtools) # to export results from regressions
library(huxtable) # to export results from regressions
library(grattan) # to have quantiles with weights

# Create Output folder
dir.create("Output")

##################################
# STEP 1 : Calculate emissions for each person ----
##################################

# This script takes Reisen and Wege data as an input, and:
# - Calculates emissions for each travel
# - Aggregates emissions for different categories of travel at the person level
# Per person emissions are stored as a csv file named "Person_emissions"

source("calculate_emissions.R")


########################################
# STEP 2 : Build dataset at person's level ----
#######################################

# Merge Person_emissions with data from the survey at the person's level
# result in stored in a csv file named Person_dataset
source("build_person_dataset.R")

########################################
# STEP 3 : Descriptive stat and graphs?
#######################################

source("descriptive_stats.R")
