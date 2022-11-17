############################################
# Project: JRI New Hampshire
# File: render_site.R
# Last updated: September 30, 2022
# Author: Mari Roberts

# Render site so you don't have to open each file
############################################

# list files
# wd <- getwd()
# list.files(paste0(wd, "/data_cleaning"))

# load files
source("data_cleaning/12_dataframes.R")
source("data_cleaning/13_incarceration_patterns_bookings.R")
source("data_cleaning/14_incarceration_patterns_entrances.R")
source("data_cleaning/15_pc_holds.R")
source("data_cleaning/16_high_utilizers.R")
source("data_cleaning/17_non_high_utilizers.R")
source("data_cleaning/18_los.R")
source("data_cleaning/19_data_availability.R")
source("data_cleaning/20_demographics.R")
source("data_cleaning/21_charges.R")

source("data_cleaning/22_county_reports_tables.R")
source("data_cleaning/23_county_reports_plots.R")
source("data_cleaning/24_generate_county_reports.R")

source("data_cleaning/rdas.R")

# render site
rmarkdown::render_site()
