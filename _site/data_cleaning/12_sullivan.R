############################################
# Project: JRI New Hampshire
# File: sullivan.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

# load packages and custom functions
source("data_cleaning/00_library.R")
source("data_cleaning/01_functions.R")

###################
# Sullivan County
###################

# clean variable names
sullivan_adm_all <- clean_names(sullivan_adm.xlsx)

# fix date formats
sullivan_adm_all$booking_date <- as.POSIXct(sullivan_adm_all$booking, format = '%m/%d/%Y %H:%M')
sullivan_adm_all$booking_date <- format(sullivan_adm_all$booking_date, "%m/%d/%Y")
sullivan_adm_all$booking_date <- as.Date(sullivan_adm_all$booking_date, format = "%m/%d/%Y")
