############################################
# Project: JRI New Hampshire
# File: rdas.R
# Last updated: January 30, 2023
# Author: Mari Roberts

# Load R data for Rmds
############################################

# Data Availability
# load(paste0(sp_data_path, "/Data/analysis/r_data//raw_adm_data_availability_table.Rda", sep = ""))

# Jail administrative data
load(paste0(sp_data_path, "/Data/analysis/r_data/adm_all.Rda",               sep = ""))
load(paste0(sp_data_path, "/Data/analysis/r_data/df_pch.Rda",                sep = ""))
load(paste0(sp_data_path, "/Data/analysis/r_data/bookings_entrances.Rda",    sep = ""))

# County specific administrative data
load(paste0(sp_data_path, "/Data/analysis/r_data/belknap_adm.Rda",      sep = ""))
load(paste0(sp_data_path, "/Data/analysis/r_data/carroll_adm.Rda",      sep = ""))
load(paste0(sp_data_path, "/Data/analysis/r_data/cheshire_adm.Rda",     sep = ""))
load(paste0(sp_data_path, "/Data/analysis/r_data/coos_adm.Rda",         sep = ""))
load(paste0(sp_data_path, "/Data/analysis/r_data/hillsborough_adm.Rda", sep = ""))
load(paste0(sp_data_path, "/Data/analysis/r_data/merrimack_adm.Rda",    sep = ""))
load(paste0(sp_data_path, "/Data/analysis/r_data/rockingham_adm.Rda",   sep = ""))
load(paste0(sp_data_path, "/Data/analysis/r_data/strafford_adm.Rda",    sep = ""))
load(paste0(sp_data_path, "/Data/analysis/r_data/sullivan_adm.Rda",     sep = ""))
