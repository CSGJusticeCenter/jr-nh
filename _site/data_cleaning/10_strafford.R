############################################
# Project: JRI New Hampshire
# File: strafford.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

###################
# Strafford County
###################

strafford_adm_all <- strafford_adm.xlsx %>%
  clean_names() %>%
  mutate(inmate_id = NA,
         race_label = NA,
         charge_code = NA,
         charge_desc = NA,
         booking_type = NA,
         release_type = NA,
         housing = NA,
         sentence_status = NA) %>%
  dplyr::select(id = id_2,
                inmate_id = id_2,
                yob = year,
                race_code = race,
                race_label,
                sex,
                housing,
                charge_code,
                charge_desc,
                booking_date,
                booking_type,
                release_date,
                release_type,
                sentence_status) %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y"),
         county = "Strafford") %>% distinct()

# remove additional rows in excel
strafford_adm_all <- strafford_adm_all %>% filter(!is.na(id)) %>% droplevels()
