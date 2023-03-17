############################################
# Project: JRI New Hampshire
# File: library.R
# Last updated: August 22, 2022
# Author: Mari Roberts

# Load packages, set working directories
############################################

###################
# Load packages
###################

# Download this version
# remotes::install_github("kcuilla/reactablefmtr")

library(readxl)
library(readr)
library(janitor)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(reactable)
library(reactablefmtr)
library(stringr)
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
library(openxlsx)
library(svDialogs)
library(ggrepel)
library(tidycensus)
library(ggpattern)
library(DataExplorer)

###################
# Local and research sharepoint
###################

# Path to data on research div sharepoint
# Make sure SP folder is synced locally

# # Download CSGJCR package
# # devtools::install_github("CSGJusticeCenter/csgjcr@DEVELOP")

library(csgjcr)

# # Set project path for MAR
# csg_set_project_path(project = "NH", sp_folder = "C:/Users/mroberts/The Council of State Governments/JC Research - JR_NH", force = TRUE)
# csg_set_project_path(project = "NH", sp_folder = "C:/Users/abyrum/The Council of State Governments/JC Research - Documents/JR_NH", force = TRUE)

# Save path
sp_data_path <- csg_get_project_path("NH")
sp_viz_output_path <- paste(sp_data_path, "/Output/r_output_presentation")

###################
# Fonts
###################

# Check the fonts path of your system
font_paths() # "C:\\Windows\\Fonts"

# Add a custom font. You will need to run this code every time you restart R
# Make sure you download the Franklin Gothic Book font to your computer
font_add(family  = "Franklin Gothic Book",
         regular = "FRABK.ttf",
         italic  = "FRABKIT.ttf",
         bold    = "FRADM.ttf")
showtext_auto()
default_fonts <- c("Franklin Gothic Book")

###################
# Colors
###################

# Official jri colors
jri_light_blue <- "#167a9c"
jri_dark_blue  <- "#293e5c"
jri_red        <- "#E25449"
jri_green      <- "#557e39"
jri_orange     <- "#b95826"
jri_gray       <- "#404040"
