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
library(gt)

###################
# Fonts
###################

default_fonts <- c("Noto Sans")
# install.packages('extrafont')
# library(extrafont)
# font_import(paths = "C:/Users/mroberts/AppData/Local/Microsoft/Windows/Fonts")
# Register fonts for Windows bitmap output
# loadfonts(device = "win", quiet = TRUE)

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

###################
# Colors
###################

# official jri colors
jri_light_blue <- "#167a9c"
jri_dark_blue  <- "#293e5c"
jri_red        <- "#b95826"
jri_green      <- "#557e39"

