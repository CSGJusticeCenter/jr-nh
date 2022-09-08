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
library(showtext)

# https://stackoverflow.com/questions/61204259/how-can-i-resolve-the-no-font-name-issue-when-importing-fonts-into-r-using-ext
# install older version so extrafonts works
remotes::install_version("Rttf2pt1", version = "1.3.8")
library(remotes)
library(extrafont)

###################
# Fonts
###################


# Import all the .ttf files from your system
# You will only need to run this once, but it will take a few minutes to finish
# extrafont::font_import()
# font_import(path = "C:/Users/mroberts/Downloads/Franklin Gothic Book Regular.otf")
# font_import(path = "C:/Users/mroberts/Downloads/Franklin Gothic Book Regular/Franklin Gothic Book Regular/Franklin Gothic Book Regular.ttf")
# font_import(paths = "C:/Windows/Fonts/Franklin Gothic Book")

# # show fonts
# fonts()

# device argument also supports "pdf" and "postscript"
loadfonts(device = "win", quiet = TRUE)

ggplot(mtcars) +
  geom_point(aes(wt, mpg)) +
  theme(text = element_text(family = "Franklin Gothic Book"))

##############

# # Check the fonts path of your system
# font_paths() # "C:\\Windows\\Fonts"
#
# # Add a custom font. You will need to run this code every time you restart R
# # Make sure you download the Franklin Gothic Book font to your computer
# font_add(family  = "Franklin Gothic Book", # Name you want to use
#          regular = "FRABK.ttf",
#          italic  = "FRABKIT.ttf") # Text of the 'General' tab plus the font extension
# showtext_auto()
# default_fonts <- c("Franklin Gothic Book")

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

