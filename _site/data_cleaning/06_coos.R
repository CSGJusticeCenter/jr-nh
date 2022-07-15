############################################
# Project: JRI New Hampshire
# File: coos.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

# load packages and custom functions
source("data_cleaning/00_library.R")
source("data_cleaning/01_functions.R")

###################
# Coos County
###################

# clean variable names
coos_adm_all <- clean_names(coos_adm.xlsx)

# rename variables for consistency
coos_adm_all <- coos_adm_all %>%
  mutate(booking_type = NA, release_type = NA) %>%
  dplyr::select(id = unique_id,
                inmate_id,
                yob,
                race,
                sex,
                housing = homelessness_indicator,
                charge_code,
                charge_desc = charges,
                booking_date = booking_dt_tm,
                booking_type,
                release_date = release_dt_tm,
                release_type,
                sentence_status)

# change date formats
coos_adm_all$booking_date <- as.Date(coos_adm_all$booking_date, format = "%m/%d/%Y")
coos_adm_all$release_date <- as.Date(coos_adm_all$release_date, format = "%m/%d/%Y")

# create FY year variable
# will be able to filter by CY later
coos_adm_all <- coos_adm_all %>%
  mutate(fy = case_when(booking_date >= "2018-07-01" & booking_date <= "2019-06-30" ~ 2019,
                        booking_date >= "2019-07-01" & booking_date <= "2020-06-30" ~ 2020,
                        booking_date >= "2020-07-01" & booking_date <= "2021-06-30" ~ 2021),
         age = fy - yob,
         los = difftime(as.POSIXct(release_date), as.POSIXct(booking_date, tz="UTC"), units="days"),
         race = case_when(race == "A"  ~ "AAPI",
                          race == "B"  ~ "Black",
                          race == "C"  ~ "AAPI",
                          race == "H"  ~ "Hispanic or Latino",
                          race == "I"  ~ "AAPI",
                          race == "NH" ~ "Unknown",
                          race == "O"  ~ "Other",
                          race == "P"  ~ "Other",
                          race == "U"  ~ "Unknown",
                          race == "W"  ~ "White",
                          race == "X"  ~ "Unknown")
  ) %>%
  # remove booking outside of study timeframe
  filter(!is.na(fy))

# organize variables
coos_adm_all <- coos_adm_all %>%
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
coos_bookings <- fnc_create_hu_variable(coos_adm_all)

# merge data back
coos_adm_all <- left_join(coos_adm_all, coos_bookings, by = c("inmate_id", "fy"))

# create a PC hold variable and county variable
coos_adm_all <- coos_adm_all %>%
  mutate(pc_hold = NA,
         county = "Coos")
