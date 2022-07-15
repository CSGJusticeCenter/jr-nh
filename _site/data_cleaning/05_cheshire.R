############################################
# Project: JRI New Hampshire
# File: cheshire.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

# load packages and custom functions
source("data_cleaning/00_library.R")
source("data_cleaning/01_functions.R")

###################
# Cheshire County
###################

# clean variable names
cheshire_adm_all <- clean_names(cheshire_adm.xlsx)

# create FY year variable
# will be able to filter by CY later
cheshire_adm_all <- cheshire_adm_all %>%
  dplyr::rename(charge_desc = charged_offense_code_description_including_technical_violations_of_supervision,
                housing = housing_instability_or_homelessness_indicator,
                charge_code = charge_offence_code,
                transfer_type = transfer_type_if_applicable) %>%
  mutate(fy = case_when(booking_date >= "2018-07-01" & booking_date <= "2019-06-30" ~ 2019,
                        booking_date >= "2019-07-01" & booking_date <= "2020-06-30" ~ 2020,
                        booking_date >= "2020-07-01" & booking_date <= "2021-06-30" ~ 2021),
         charge_desc = case_when(charge_desc == "PROTECTIVE CUSTODY" | charge_desc == "PROTECTIVE CUSTODY - DRUGS" ~ "PROTECTIVE CUSTODY", TRUE ~ charge_desc),
         age = fy - yob,
         los = difftime(as.POSIXct(release_date), as.POSIXct(booking_date, tz="UTC"), units="days"),
         race = case_when(race == "A" ~ "AAPI",
                          race == "B" ~ "Black",
                          race == "H" ~ "Hispanic or Latino",
                          race == "I" ~ "American Indian or Alaskan Native",
                          race == "L" ~ "Hispanic or Latino",
                          race == "P" ~ "AAPI",
                          race == "U" ~ "Unknown",
                          race == "W" ~ "White")
         ) %>%
  # remove booking outside of study timeframe
  filter(!is.na(fy))

# change date formats
cheshire_adm_all$booking_date <- format(cheshire_adm_all$booking_date, "%m/%d/%Y")
cheshire_adm_all$release_date <- format(cheshire_adm_all$release_date, "%m/%d/%Y")
cheshire_adm_all$booking_date <- as.Date(cheshire_adm_all$booking_date, format = "%m/%d/%Y")
cheshire_adm_all$release_date <- as.Date(cheshire_adm_all$release_date, format = "%m/%d/%Y")

# organize variables
cheshire_adm_all <- cheshire_adm_all %>%
  dplyr::select(id,
                inmate_id,
                yob,
                race,
                sex,
                housing,
                charge_code,
                charge_desc,
                booking_date,
                booking_type,
                release_date,
                release_type,
                sentence_status,
                everything()) %>%
  select(-c(transfer_type, ethnicity))

# create high utilizer variable
cheshire_bookings <- fnc_create_hu_variable(cheshire_adm_all)

# merge data back
cheshire_adm_all <- left_join(cheshire_adm_all, cheshire_bookings, by = c("inmate_id", "fy"))

# create a PC hold variable and county variable
cheshire_adm_all <- cheshire_adm_all %>%
  mutate(pc_hold = ifelse(charge_desc == "PROTECTIVE CUSTODY", 1, 0),
         county = "Cheshire")
