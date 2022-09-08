############################################
# Project: JRI New Hampshire
# File: sullivan.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

###################
# Sullivan County
###################

# clean variable names
sullivan_adm_all <- clean_names(sullivan_adm.xlsx)

# fix date formats
sullivan_adm_all$booking_date_time <- as.POSIXct(sullivan_adm_all$booking_date_time, format = '%m/%d/%Y %H:%M')
sullivan_adm_all$booking_date_time <- format(sullivan_adm_all$booking_date_time, "%m/%d/%Y")
sullivan_adm_all$booking_date_time <- as.Date(sullivan_adm_all$booking_date_time, format = "%m/%d/%Y")
sullivan_adm_all$release_date_time <- as.POSIXct(sullivan_adm_all$release_date_time, format = '%m/%d/%Y %H:%M')
sullivan_adm_all$release_date_time <- format(sullivan_adm_all$release_date_time, "%m/%d/%Y")
sullivan_adm_all$release_date_time <- as.Date(sullivan_adm_all$release_date_time, format = "%m/%d/%Y")

# clean variable names
sullivan_adm_all <- sullivan_adm_all %>%
  mutate(race_label = NA,
         release_type = NA) %>%
  dplyr::select(id = id_number_inmate_number,
                inmate_id = inmate_num,
                yob = year,
                race_code = race,
                race_label,
                sex = gender,
                housing = housing,
                charge_code = charge_id,
                charge_desc = charge,
                booking_date = booking_date_time,
                booking_type = detention_type,
                release_date = release_date_time,
                release_type,
                sentence_status) %>%
  mutate(county = "Sullivan")
