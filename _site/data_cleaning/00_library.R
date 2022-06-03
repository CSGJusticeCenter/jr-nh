############################################
# Project: JRI New Hampshire
# File: library.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Load packages
############################################

###################
# CHANGE THESE DEPENDING ON USER
###################

# path to jail data on research division sharepoint
CSG_SP_PATH = "C:/Users/mroberts/The Council of State Governments/JC Research - JR_NH"

# path to local environment
L_PATH = "C:/Users/mroberts/OneDrive - The Council of State Governments/Desktop/csgjc/repos/jr-nh"

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

# highchart
library(highcharter)

# data dictionary
library(labelled)
library(sjPlot)
library(grid)
library(gridExtra)
