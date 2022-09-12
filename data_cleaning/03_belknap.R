############################################
# Project: JRI New Hampshire
# File: belknap.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

###################
# Belknap County
###################

belknap_adm_all <- belknap_adm.xlsx %>%
  clean_names() %>%
  mutate(charge_code = NA,
         race_label = NA) %>%
  dplyr::select(id = unique_person_id,
                inmate_id,
                yob = year_of_birth,
                race_code = race,
                race_label,
                sex,
                housing = housing_instability_or_homelessness_indicator,
                charge_code,
                charge_desc = charged_offense_code_description_including_technical_violations_of_supervision,
                booking_date,
                booking_type,
                release_date,
                release_type,
                sentence_status = sentencing_status) %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y"),
         county = "Belknap")
