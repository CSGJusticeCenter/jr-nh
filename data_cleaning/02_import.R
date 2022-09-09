############################################
# Project: JRI New Hampshire
# File: import.R
# Last updated: May 12, 2022
# Author: Mari Roberts

# Imports files from jail partners
############################################

# load packages
source("data_cleaning/00_library.R")

###################
# Belknap County
###################

belknap_adm.xlsx       <- read_csv(file=paste0(sp_data_path, "/Data/Belknap County/Jail History Admin Form - Belknap DOC.csv"))

###################
# Carroll County
###################

carroll_releases.xlsx  <- read_excel(paste0(sp_data_path, "/Data/Carroll County/CSG release data 7.1.2018-6.30.2021.xls"))
carroll_bookings.xlsx  <- read_excel(paste0(sp_data_path, "/Data/Carroll County/CSG data 7.1.2018-6.30-2021.xls"))

###################
# Cheshire County
###################

cheshire_adm.xlsx      <- read_excel(paste0(sp_data_path, "/Data/Cheshire County/CSG Jail Data D-IDENT.xlsx"))
cheshire_prog.xlsx     <- read_excel(paste0(sp_data_path, "/Data/Cheshire County/MRT Attendance Logs CSG.xlsx"))
cheshire_mh_stats.xlsx <- read_excel(paste0(sp_data_path, "/Data/Cheshire County/AAA MH & SU STATS CSG.xls"))

###################
# Coos County
###################

coos_adm.xlsx          <- read_excel(paste0(sp_data_path, "/Data/Coos County/Copy of CSG booking data.xls"))

###################
# Grafton County
###################


###################
# Hillsborough County
###################

hillsborough_adm.xlsx         <- read_excel(paste0(sp_data_path, "/Data/Hillsborough County/Justice Center 2.xls"))
hillsborough_bh.xlsx          <- read_excel(paste0(sp_data_path, "/Data/Hillsborough County/Justice Center 3.xls"))

###################
# Merrimack County
###################

merrimack_adm.xlsx    <- read_excel(paste0(sp_data_path, "/Data/Merrimack County/Data Field 2 CSG 2019 to 2021 (2).xlsx"))

###################
# Rockingham County
###################

rockingham_adm.xlsx   <- read_csv(paste0(sp_data_path, "/Data/Rockingham County/Inmate Report 070118 through 063021 Deidentified.csv"))

###################
# Strafford County
###################


###################
# Sullivan County
###################

sullivan_adm.xlsx     <- read_excel(paste0(sp_data_path, "/Data/Sullivan County/8.23.22 de identified (5).xlsx"))
