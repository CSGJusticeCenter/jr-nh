############################################
# Format Merrimack County data
# Last updated: May 20, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

source("data_cleaning/00_library.R")
source("data_cleaning/01_functions.R")

###################
# Merrimack County
###################

# clean variable names
merrimack_adm_all <- clean_names(merrimack_adm.xlsx)

# create FY year variable
# will be able to filter by CY later
# WHAT DOES RACE X STAND FOR??????
merrimack_adm_all <- merrimack_adm_all %>%
  dplyr::rename(id = uniq_id,
                charge_desc = charges,
                housing = housing_instability,
                inmate_id = im_id,
                release_date = rel_date) %>%
  mutate(fy = case_when(booking_date >= "2018-07-01" & booking_date <= "2019-06-30" ~ 2019,
                        booking_date >= "2019-07-01" & booking_date <= "2020-06-30" ~ 2020,
                        booking_date >= "2020-07-01" & booking_date <= "2021-06-30" ~ 2021),
         age = fy - yob,
         los = difftime(as.POSIXct(release_date), as.POSIXct(booking_date, tz="UTC"), units="days"),
         race = case_when(race == "A" ~ "Asian or Pacific Islander",
                          race == "B" ~ "Black",
                          race == "H" ~ "Hispanic or Latino",
                          race == "I" ~ "American Indian or Alaskan Native",
                          race == "O" ~ "Other",
                          race == "P" ~ "Asian or Pacific Islander",
                          race == "U" ~ "Unknown",
                          race == "W" ~ "White",
                          race == 'X' ~ "X - Not sure")) %>%
  # remove booking outside of study timeframe
  filter(!is.na(fy))

# organize variables
merrimack_adm_all <- merrimack_adm_all %>%
  dplyr::select(id,
                inmate_id,
                yob,
                race,
                sex,
                housing,
                charge_desc,
                booking_date,
                booking_type,
                release_date,
                sentence_status,
                everything())

# save long file that includes all charges
merrimack_adm_charges <- merrimack_adm_all

# remove charge codes and duplicates to get picture of cohort
merrimack_adm <- merrimack_adm_all %>%
  dplyr::select(inmate_id, race, yob, age, sex,
                housing, sentence_status,
                booking_date, fy) %>%
  distinct()

# remove charge codes and duplicates to get picture of cohort
merrimack_booking <- merrimack_adm_all %>%
  dplyr::select(inmate_id, race, yob, age, sex,
                housing, booking_date, booking_type, fy) %>%
  distinct()

# sep by fiscal year
merrimack_adm_19 <- merrimack_adm %>% filter(fy == 2019)
merrimack_adm_20 <- merrimack_adm %>% filter(fy == 2020)
merrimack_adm_21 <- merrimack_adm %>% filter(fy == 2021)

# sep by fy year
merrimack_booking_19 <- merrimack_booking %>% filter(fy == 2019)
merrimack_booking_20 <- merrimack_booking %>% filter(fy == 2020)
merrimack_booking_21 <- merrimack_booking %>% filter(fy == 2021)
