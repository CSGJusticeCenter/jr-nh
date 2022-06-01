############################################
# Project: JRI New Hampshire
# File: import.R
# Last updated: May 12, 2022
# Author: Mari Roberts

# Imports files from jail partners
############################################

# load packages
source("data_cleaning/00_library.R")

# path to jail data on research division sharepoint
sp_data_path <- csg_sp_path(file.path("JR_NH", "Data", "r_data"))

###################
# Belknap County
###################

belknap.xlsx <- read_excel("Belknap County/Jail History Admin Form - Belknap DOC.xls")

###################
# Carroll County
###################

carroll_releases.xlsx <- read_excel("Carroll County/CSG release data 7.1.2018-6.30.2021.xls")
carroll_bookings.xlsx <- read_excel("Carroll County/CSG data 7.1.2018-6.30-2021.xls")

###################
# Cheshire County
###################

cheshire_adm.xlsx      <- read_excel("Cheshire County/CSG Jail Data D-IDENT.xlsx")
cheshire_prog.xlsx     <- read_excel("Cheshire County/MRT Attendance Logs CSG.xlsx")
cheshire_mh_stats.xlsx <- read_excel("Cheshire County/AAA MH & SU STATS CSG.xls")

###################
# Merrimack County
###################

merrimack_adm.xlsx     <- read_excel("Merrimack County/Data Field 2 CSG 2019 to 2021 (2).xlsx")
