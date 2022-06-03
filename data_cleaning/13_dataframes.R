############################################
# Project: JRI New Hampshire
# File: dataframes.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Load rds files for Rmd files
    # frequency tables for demographics, bookings, releases
    # heatmap data for bookings by year and month
############################################

# set working directory to sharepoint where jail data is located
setwd(CSG_SP_PATH)

#1 Belknap
load(file="Data/r_data/belknap_adm.rds")
load(file="Data/r_data/belknap_booking.rds")
load(file="Data/r_data/belknap_sentence.rds")
load(file="Data/r_data/belknap_race.rds")
load(file="Data/r_data/belknap_sex.rds")
load(file="Data/r_data/belknap_heatmap.rds")
load(file="Data/r_data/belknap_hu_booking.rds")
load(file="Data/r_data/belknap_hu_sentence.rds")
load(file="Data/r_data/belknap_hu_race.rds")
load(file="Data/r_data/belknap_hu_sex.rds")

#2 Carroll
load(file="Data/r_data/carroll_adm.rds")
load(file="Data/r_data/carroll_booking.rds")
load(file="Data/r_data/carroll_sentence.rds")
load(file="Data/r_data/carroll_race.rds")
load(file="Data/r_data/carroll_sex.rds")
load(file="Data/r_data/carroll_heatmap.rds")
load(file="Data/r_data/carroll_hu_booking.rds")
load(file="Data/r_data/carroll_hu_sentence.rds")
load(file="Data/r_data/carroll_hu_race.rds")
load(file="Data/r_data/carroll_hu_sex.rds")

#3 Cheshire
load(file="Data/r_data/cheshire_adm.rds")
load(file="Data/r_data/cheshire_booking.rds")
load(file="Data/r_data/cheshire_sentence.rds")
load(file="Data/r_data/cheshire_race.rds")
load(file="Data/r_data/cheshire_sex.rds")
load(file="Data/r_data/cheshire_heatmap.rds")
load(file="Data/r_data/cheshire_hu_booking.rds")
load(file="Data/r_data/cheshire_hu_sentence.rds")
load(file="Data/r_data/cheshire_hu_race.rds")
load(file="Data/r_data/cheshire_hu_sex.rds")

#4 Coos

#5 Grafton

#6 Hillsborough

#7 Merrimack
load(file="Data/r_data/merrimack_adm.rds")
load(file="Data/r_data/merrimack_booking.rds")
load(file="Data/r_data/merrimack_sentence.rds")
load(file="Data/r_data/merrimack_race.rds")
load(file="Data/r_data/merrimack_sex.rds")
load(file="Data/r_data/merrimack_heatmap.rds")
load(file="Data/r_data/merrimack_hu_booking.rds")
load(file="Data/r_data/merrimack_hu_sentence.rds")
load(file="Data/r_data/merrimack_hu_race.rds")
load(file="Data/r_data/merrimack_hu_sex.rds")

#8 Rockingham

#9 Strafford

#10 Sullivan

# setwd back to local
setwd(L_PATH)
