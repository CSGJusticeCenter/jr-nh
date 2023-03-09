############################################
# Project: JRI New Hampshire
# File: import.R
# Last updated: February 10, 2022
# Author: Mari Roberts

# Imports files from jail partners
# Files are imported from sharepoint (jail admin data) and hard drive "D:" (medicaid data)
############################################

# load packages
source("data_cleaning/00_library.R")

# data availability by county
raw_adm_data_availability.xlsx <- read_excel(paste0(sp_data_path, "/Data/Raw_data_dictionaries_09_12_22.xlsx"), sheet = "Overall")

# charge codes
charge_codes.xlsx              <- read_excel(paste0(sp_data_path, "/Data/Offense Information/CPI_DMV_COURT_20180412083730.xls"), sheet = "All (C,E)")

# county budgets
county_budgets_reported.xlsx   <- read_excel(paste0(sp_data_path, "/Data/County Budgets/budget_sources.xlsx"), sheet = "Reported Costs")
county_budgets_calculated.xlsx <- read_excel(paste0(sp_data_path, "/Data/County Budgets/budget_sources.xlsx"), sheet = "Year 2019 (If available)")

# medicaid events
medicaid_encounters.xlsx <- read_csv("D:/CSG-Encounters File 201407-202106.txt")

# medicaid enrollment
medicaid_enrollment.xlsx <- read_csv("D:/Medicaid Enrollment Data For County Matched Members.txt")

# eligibility categories descriptions
medicaid_categories.xlsx <- read_excel("D:/Categories of Eligibility Description.xlsx")

# medicaid data dictionary (icd-10)
medicaid_dictionary.xlsx <- read_excel(paste0(sp_data_path, "/Data/DHHS_data_dictionary.xlsx"), sheet = "DX_to_CCSR_Mapping", skip = 1)

###################
# Belknap County
###################

belknap_adm.xlsx       <- read_csv(file=paste0(sp_data_path, "/Data/Belknap County/Jail History Admin Form - Belknap DOC.csv"))
belknap_medicaid.xlsx  <- read_excel("D:/Belknap County Jail Data From DHHS.xlsx")

###################
# Carroll County
###################

# carroll county separates their data into releases and bookings which will be joined
carroll_bookings.xlsx  <- read_excel(paste0(sp_data_path, "/Data/Carroll County/July0118-June3021.xls"))
carroll_medicaid.xlsx  <- read_excel("D:/Carroll County Jail Data From DHHS.xlsx")

###################
# Cheshire County
###################

cheshire_adm.xlsx      <- read_excel(paste0(sp_data_path, "/Data/Cheshire County/CSG Jail Data D-IDENT.xlsx"))
cheshire_medicaid.xlsx  <- read_excel("D:/Cheshire County Jail Data From DHHS.xlsx")

# not looking at bh data for now
# cheshire_prog.xlsx     <- read_excel(paste0(sp_data_path, "/Data/Cheshire County/MRT Attendance Logs CSG.xlsx"))
# cheshire_mh_stats.xlsx <- read_excel(paste0(sp_data_path, "/Data/Cheshire County/AAA MH & SU STATS CSG.xls"))

###################
# Coos County
###################

coos_adm.xlsx          <- read_excel(paste0(sp_data_path, "/Data/Coos County/Copy of CSG booking data.xls"))
coos_medicaid.xlsx     <- read_excel("D:/Coos County Jail Data From DHHS.xlsx")

###################
# Hillsborough County
###################

hillsborough_adm.xlsx       <- read_excel(paste0(sp_data_path, "/Data/Hillsborough County/Justice Center 2.xls"))
hillsborough_medicaid.xlsx  <- read_excel("D:/Hillsborough County Jail Data From DHHS.xlsx")

# not looking at bh data for now
# hillsborough_bh.xlsx          <- read_excel(paste0(sp_data_path, "/Data/Hillsborough County/Justice Center 3.xls"))

###################
# Merrimack County
###################

merrimack_adm.xlsx       <- read_excel(paste0(sp_data_path, "/Data/Merrimack County/Data Field 2 CSG 2019 to 2021 (2).xlsx"))
merrimack_medicaid.xlsx  <- read_excel("D:/Merrimack County Jail Data From DHHS.xlsx")

###################
# Rockingham County
###################

rockingham_adm.xlsx       <- read_csv(paste0(sp_data_path, "/Data/Rockingham County/Inmate Report 070118 through 063021 Deidentified.csv"))
rockingham_medicaid.xlsx  <- read_excel("D:/Rockingham County Jail Data From DHHS.xlsx")

###################
# Strafford County
###################

strafford_adm.xlsx       <- read_excel(paste0(sp_data_path, "/Data/Strafford County/CSG Data.xlsx"))
strafford_medicaid.xlsx  <- read_excel("D:/Strafford County Jail Data From DHHS.xlsx")

###################
# Sullivan County
###################

sullivan_adm.xlsx       <- read_excel(paste0(sp_data_path, "/Data/Sullivan County/8.23.22 de identified (5).xlsx"))
sullivan_medicaid.xlsx  <- read_excel("D:/Sullivan County Jail Data From DHHS.xlsx")
