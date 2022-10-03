############################################
# Project: JRI New Hampshire
# File: hillsborough.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

###################
# Hillsborough.R County
###################

hillsborough_adm_all <- hillsborough_adm.xlsx %>%
  clean_names() %>%
  mutate(charge_code = NA,
         race_label = NA) %>%
  dplyr::select(id = x1,
                inmate_id = ccn,
                yob,
                race_code = race,
                race_label,
                sex,
                housing = homeless_y_n,   # switch N to Y??????????
                charge_code,
                charge_desc = charges,
                booking_date,
                booking_type,
                release_date,
                release_type,
                sentence_status = sentencing_status) %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%y"),
         release_date = as.Date(release_date, format = "%m/%d/%y"),
         county = "Hillsborough") %>%
  distinct()
