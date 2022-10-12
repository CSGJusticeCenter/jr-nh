############################################
# Project: JRI New Hampshire
# File: county_reports.R
# Last updated: August 22, 2022
# Author: Mari Roberts

# Generate html documents for each county
# based on county_report_template.Rmd
############################################

library(rmarkdown)
library(tidyverse)

# save working directory
wd <- getwd()

# loop through each county and general an html document based
# off of county_report_template.Rmd. These html files can be called
# in the _site.yml file which sets up the menu items on netlify
# for (county in unique(nh_adm_all$county)){
#   subgroup <- county
#   render("county_report_template.Rmd",
#          output_file = paste0(county, '_county_report', '.html'),
#          # output_options = list(lib_dir = here::here("_site/lib")),
#          output_dir = paste0(wd, "/_site"))
# }

# loop through each county and general an Rmd document, html documents need an Rmd file for netlify

# read in original rmd
orig_rmd <- read_lines("_county_report_template.Rmd")

# replacement values
counties_rmd <- as.character(counties)

# function to replace place-holder text in orig Rmd with our replacement values
# and write out to Rmd - name of Rmd should include replacement value
replace_write_rmd <- function(county) {
  str_replace(orig_rmd, "change_me", county) |>
    write_lines(paste0("county_report_", county, ".Rmd"))
}

# iterate over replacement values and write new Rmds
walk(counties_rmd, replace_write_rmd)
