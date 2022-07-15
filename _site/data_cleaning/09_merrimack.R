############################################
# Project: JRI New Hampshire
# File: merrimack.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

# load packages and custom functions
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
         charge_code = NA,
         release_type = NA,
         race = case_when(race == "A" ~ "AAPI",
                          race == "B" ~ "Black",
                          race == "H" ~ "Hispanic or Latino",
                          race == "I" ~ "American Indian or Alaskan Native",
                          race == "O" ~ "Other",
                          race == "P" ~ "AAPI",
                          race == "U" ~ "Unknown",
                          race == "W" ~ "White",
                          race == 'X' ~ "Unknown")
  ) %>%
  # remove booking outside of study timeframe
  filter(!is.na(fy))

# change date formats
merrimack_adm_all$booking_date <- format(merrimack_adm_all$booking_date, "%m/%d/%Y")
merrimack_adm_all$release_date <- format(merrimack_adm_all$release_date, "%m/%d/%Y")
merrimack_adm_all$booking_date <- as.Date(merrimack_adm_all$booking_date, format = "%m/%d/%Y")
merrimack_adm_all$release_date <- as.Date(merrimack_adm_all$release_date, format = "%m/%d/%Y")

# organize variables
merrimack_adm_all <- merrimack_adm_all %>%
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
                everything())

# create high utilizer variable
merrimack_bookings <- fnc_create_hu_variable(merrimack_adm_all)

# merge data back
merrimack_adm_all <- left_join(merrimack_adm_all, merrimack_bookings, by = c("inmate_id", "fy"))

# create a PC hold variable and county variable
merrimack_adm_all <- merrimack_adm_all %>%
  mutate(pc_hold = ifelse(charge_desc == "PROTECTIVE CUSTODY" | charge_desc == "PROTECTIVE CUSTODY/INTOXICATION", 1, 0),
         county = "Merrimack")
