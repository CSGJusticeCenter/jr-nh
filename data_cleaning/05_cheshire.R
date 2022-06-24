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

# organize variables
cheshire_adm_all <- cheshire_adm_all %>%
  dplyr::select(id,
         inmate_id,
         yob,
         race,
         ethnicity,
         sex,
         housing,
         charge_code,
         charge_desc,
         #charge,
         booking_date,
         booking_type,
         release_date,
         release_type,
         sentence_status,
         everything())

# save long file that includes all charges
cheshire_adm_charges <- cheshire_adm_all

# remove charge codes and duplicates to get picture of cohort
cheshire_adm <- cheshire_adm_all %>%
  dplyr::select(inmate_id, race, yob, age, sex,
                housing, sentence_status,
                booking_date, los, fy) %>%
  distinct()

# remove charge codes and duplicates to get picture of cohort
cheshire_booking_all <- cheshire_adm_all %>%
  dplyr::select(inmate_id, race, yob, age, sex,
                housing, booking_date, booking_type, los, fy) %>%
  distinct()

# sep by fiscal year
cheshire_adm_19 <- cheshire_adm %>% filter(fy == 2019)
cheshire_adm_20 <- cheshire_adm %>% filter(fy == 2020)
cheshire_adm_21 <- cheshire_adm %>% filter(fy == 2021)

# sep by fy year
cheshire_booking_19 <- cheshire_booking_all %>% filter(fy == 2019)
cheshire_booking_20 <- cheshire_booking_all %>% filter(fy == 2020)
cheshire_booking_21 <- cheshire_booking_all %>% filter(fy == 2021)
