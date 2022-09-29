############################################
# Project: JRI New Hampshire
# File: county_reports.R
# Last updated: August 22, 2022
# Author: Mari Roberts

# Generate html documents for each county
# based on county_adm_template.Rmd
############################################

# load packages
# remove.packages("rmarkdown")
# install.packages("rmarkdown")
library("rmarkdown")
library(yaml)

# save working directory
wd <- getwd()

# loop through each county and general an html document based
# off of county_adm_template.Rmd. These html files can be called
# in the _site.yml file which sets up the menu items on netlify
for (county in unique(nh_adm_all$county)){
  subgroup <- county
  render("county_adm_template.Rmd",
         output_file = paste0(county, '_county_report', '.html'),
         output_dir = paste0(wd, "/_site"))
}

