############################################
# Project: JRI New Hampshire
# File: coos.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

###################
# Coos County
###################

coos_adm_all <- coos_adm.xlsx %>%
  clean_names() %>%
  mutate(booking_type = NA,
         release_type = NA,
         race_label   = NA) %>%
  dplyr::select(id = unique_id,
                inmate_id,
                yob,
                race_code = race,
                race_label,
                sex,
                housing = homelessness_indicator,
                charge_code,
                charge_desc = charges,
                booking_date = booking_dt_tm,
                booking_type,
                release_date = release_dt_tm,
                release_type,
                sentence_status) %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y"),
         county = "Coos") %>%
  distinct()
