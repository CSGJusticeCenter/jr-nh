############################################
# Project: JRI New Hampshire
# File: cheshire.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

###################
# Cheshire County
###################

cheshire_adm_all <- cheshire_adm.xlsx %>%
  clean_names() %>%
  mutate(race_label = NA) %>%
  dplyr::select(id,
                inmate_id,
                yob,
                race_code = race,
                race_label,
                sex,
                housing = housing_instability_or_homelessness_indicator,
                charge_code = charge_offence_code,
                charge_desc = charged_offense_code_description_including_technical_violations_of_supervision,
                booking_date,
                booking_type,
                release_date,
                release_type,
                sentence_status) %>%
  mutate(booking_date = format(booking_date, format = "%m/%d/%Y"),
         release_date = format(release_date, format = "%m/%d/%Y"),
         county = "Carroll") %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y"))
