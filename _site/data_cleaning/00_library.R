############################################
# Project: JRI New Hampshire
# File: library.R
# Last updated: August 22, 2022
# Author: Mari Roberts

# Load packages, set working directories
############################################

###################
# load packages
###################

library(readxl)
library(readr)
library(janitor)
library(lubridate)
library(tidyverse)
library(plyr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(stringr)
library(csgjcr)
library(highcharter)
library(labelled)
library(sjPlot)
library(grid)
library(gridExtra)
library(zoo)

###################
# Fonts
###################

default_fonts <- c("Noto Sans")

###################
# Local and research sharepoint
###################

# path to data on research div sharepoint
# make sure SP folder is synced locally
# https://csgorg.sharepoint.com/:f:/s/Team-JC-Research/EhdvImKN2rdPnmHQ2TrKlooBdYqnnWc0SUXBNuh9C7d41g?e=NCsh8I
# in your Renviron, set CSG_SP_PATH = "your sharepoint path here" and GITHUB_PAT = "your token here"
# To generate a github token - usethis::create_github_token()
# To edit Renviron - usethis::edit_r_environ()
sp_data_path <- csg_sp_path(file.path("JC Research - JR_NH"))
