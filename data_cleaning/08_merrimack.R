############################################
# Project: JRI New Hampshire
# File: merrimack.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

###################
# Merrimack County
###################

merrimack_adm_all <- merrimack_adm.xlsx %>%
  clean_names() %>%
  mutate(charge_code = NA,
         race_label = NA,
         release_type = NA) %>%
  dplyr::select(id = uniq_id,
                inmate_id = im_id,
                yob,
                race_code = race,
                race_label,
                sex,
                housing = housing_instability,
                charge_code,
                charge_desc = charges,
                booking_date,
                booking_type,
                release_date = rel_date,
                release_type,
                sentence_status) %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y"),
         county = "Merrimack") %>% distinct()
