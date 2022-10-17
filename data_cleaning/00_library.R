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

# remotes::install_github("kcuilla/reactablefmtr")

library(readxl)
library(readr)
library(janitor)
library(lubridate)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(reactable)
library(reactablefmtr)
library(stringr)
library(csgjcr)
library(highcharter)
library(labelled)
library(sjPlot)
library(grid)
library(gridExtra)
library(zoo)
library(gt)
library(extrafont)
library(showtext)
library(maditr)
library(formattable)
library(downloadthis)
library(scales)
library(officer)
library(rvg)
library(reactablefmtr)
library(ggtext)

# install.packages("rmarkdown")
# install.packages("knitr")
# install.packages("yaml")

# library(rmarkdown)
# library(knitr)
# library(yaml)

###################
# Fonts
###################

# Check the fonts path of your system
font_paths() # "C:\\Windows\\Fonts"

# Add a custom font. You will need to run this code every time you restart R
# Make sure you download the Franklin Gothic Book font to your computer
font_add(family  = "Franklin Gothic Book", # Name you want to use
         regular = "FRABK.ttf",
         italic  = "FRABKIT.ttf") # Text of the 'General' tab plus the font extension
showtext_auto()
default_fonts <- c("Franklin Gothic Book")

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
jri_light_blue <- "#4095B1"
jri_dark_blue  <- "#273C4C"
jri_red        <- "#E25449"
jri_green      <- "#61C280"
jri_off_green  <- "#779F38"
jri_orange     <- "#E17619"

