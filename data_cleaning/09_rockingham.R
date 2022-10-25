############################################
# Project: JRI New Hampshire
# File: rockingham.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

###################
# Rockingham County
###################

# clean variable names
rockingham_adm_all <- rockingham_adm.xlsx %>%
  clean_names() %>%
  mutate(id = NA,
         race_label = NA,
         release_type = NA) %>%
  dplyr::select(id,
                inmate_id = inmate_id_number,
                yob,
                race_code = race,
                race_label,
                sex,
                housing = homelessness,
                charge_code = charge_id,
                charge_desc = charge_description,
                booking_date = arrival_date_and_time,
                booking_type = commitment_documentation,
                release_date = release_date_and_time,
                release_type,
                sentence_status) %>%
  mutate(booking_date = format(booking_date, format = "%m/%d/%Y"),
         release_date = format(release_date, format = "%m/%d/%Y"),
         county = "Rockingham") %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y")) %>%
  distinct()
