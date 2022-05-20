############################################
# Import Data
# Last updated: May 12, 2022
# Author: Mari Roberts

# Imports files from jail partners
############################################

source("data_cleaning/00_library.R")

###################
# Carroll County
###################

carroll_releases.xlsx <- read_excel("jail_data/Carroll County/Release data CSG.xls")
carroll_bookings.xlsx <- read_excel("jail_data/Carroll County/CSG booking data edited.xls")

###################
# Cheshire County
###################

cheshire_adm.xlsx      <- read_excel("jail_data/Cheshire County/CSG Jail Data D-IDENT.xlsx")
cheshire_prog.xlsx     <- read_excel("jail_data/Cheshire County/MRT Attendance Logs CSG.xlsx")
cheshire_mh_stats.xlsx <- read_excel("jail_data/Cheshire County/AAA MH & SU STATS CSG.xls")

###################
# Merrimack County
###################

merrimack_adm.xlsx     <- read_excel("jail_data/Merrimack County/Data Field 2 CSG 2019 to 2021 (2).xlsx")
