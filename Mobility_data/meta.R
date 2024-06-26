# This is the meta script to analyse the data.
# Put it in a folder "Mobility_Germany", with 
# 1/ the folder "CSV" that contains Mobility data
# 2/ a folder "Other_input" that will receive the other input we will need (emission factors, ...)
rm(list = ls())

# Set directory:
setwd("~/GitHub/Mobility_Germany/Mobility_data")

library(vtable)
library(ggstance)
library(broom.mixed)
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
library(randomForest)
library(lawstat) # for plotting a lorenz curve (but function doesn't work...)
library(ineq) # for plotting a lorenz curve
library(stargazer)
library(prim) # for PRIM analysis?
library(sdtoolkit) # for PRIM analysis
library(foreign)
library(Ecdat)

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
# STEP 3 : Descriptive stat and graphs
#######################################

source("descriptive_stats.R")

source("descriptive_stats_additional.R")


########################################
# STEP 4 : Regressions
#######################################

source("prepare_regression_dataset.R")

########################################
# STEP 5 : Summary Stats
#######################################

source("summary_stats.R")

########################################
# STEP 6 : Prepare table for the regression
#######################################

source("quantile_regressions_torun.R")


########################################
# STEP 6 : Compute the graph for quantile regression
#######################################

source("quantile_regressions_graph.R")
