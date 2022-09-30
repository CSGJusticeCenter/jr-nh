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
source("data_cleaning/13_incarceration_patterns.R")
source("data_cleaning/14_high_utilizers.R")
source("data_cleaning/15_non_high_utilizers.R")
# source("data_cleaning/16_demographics.R")
# source("data_cleaning/17_medicaid.R")
# source("data_cleaning/18_data_availability.R")

source("data_cleaning/20_county_reports_tables.R")
source("data_cleaning/21_county_reports_plots.R")
source("data_cleaning/22_generate_county_reports.R")

# render site
rmarkdown::render_site()
