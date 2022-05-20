############################################
# Format Carroll County data
# Last updated: May 12, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

source("data_cleaning/00_library.R")
source("data_cleaning/01_functions.R")

###################
# Carroll County
###################

# clean variable names
carroll_releases <- clean_names(carroll_releases.xlsx)
carroll_bookings <- clean_names(carroll_bookings.xlsx)

# merge two adm files together
carroll_adm_all <- merge(carroll_releases, carroll_bookings, by = c("inmate_id", "release_dt_tm", "yob"))

# rename variables for consistency
carroll_adm_all <- carroll_adm_all %>%
  dplyr::select(id = unique_id,
                inmate_id,
                yob,
                race,
                sex,
                housing,
                charge_offense_code = charge_code,
                charge_desc = charge_description,
                booking_date = booking_dt_tm,
                booking_type = document_type,
                release_date = release_dt_tm,
                release_type = release_type,
                release_status,
                sentence_status,
                everything())

# create FY year variable
# will be able to filter by CY later
carroll_adm_all <- carroll_adm_all %>%
  mutate(fy = case_when(booking_date >= "2018-07-01" & booking_date <= "2019-06-30" ~ 2019,
                        booking_date >= "2019-07-01" & booking_date <= "2020-06-30" ~ 2020,
                        booking_date >= "2020-07-01" & booking_date <= "2021-06-30" ~ 2021),
         age = fy - yob,
         los = difftime(as.POSIXct(release_date), as.POSIXct(booking_date, tz="UTC"), units="days"),
         race = case_when(race == "A" ~ "Asian or Pacific Islander",
                          race == "B" ~ "Black",
                          race == "I" ~ "American Indian or Alaskan Native",
                          race == "U" ~ "Unknown",
                          race == "W" ~ "White")) %>%
  # remove booking outside of study timeframe
  filter(!is.na(fy))

# save long file that includes all charges
carroll_adm_charges <- carroll_adm_all

# remove charge codes and duplicates to get picture of cohort
carroll_adm <- carroll_adm_all %>%
  dplyr::select(inmate_id, race, yob, age, sex,
                housing, sentence_status,
                booking_date, fy) %>%
  distinct()

# remove charge codes and duplicates to get picture of cohort
carroll_booking <- carroll_adm_all %>%
  dplyr::select(inmate_id, race, yob, age, sex,
                housing, booking_date, booking_type, fy) %>%
  distinct()

# sep by fiscal year
carroll_adm_19 <- carroll_adm %>% filter(fy == 2019)
carroll_adm_20 <- carroll_adm %>% filter(fy == 2020)
carroll_adm_21 <- carroll_adm %>% filter(fy == 2021)

# sep by fy year
carroll_booking_19 <- carroll_booking %>% filter(fy == 2019)
carroll_booking_20 <- carroll_booking %>% filter(fy == 2020)
carroll_booking_21 <- carroll_booking %>% filter(fy == 2021)

