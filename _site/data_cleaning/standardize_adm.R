############################################
# Project: JRI New Hampshire
# File: standardize.R
# Last updated: June 27, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

###################
# Belknap County
###################

# clean variable names
belknap_adm_all <- clean_names(belknap_adm.xlsx)

# rename variables for consistency
belknap_adm_all <- belknap_adm_all %>%
  dplyr::select(id = unique_person_id,
                inmate_id,
                yob = year_of_birth,
                race,
                sex,
                housing = housing_instability_or_homelessness_indicator,
                # charge_code - needs a charge code bc the code and description are in one field
                charge_desc = charged_offense_code_description_including_technical_violations_of_supervision,
                booking_date,
                booking_type,
                release_date,
                release_type,
                sentence_status = sentencing_status) 

# change date formats
belknap_adm_all$booking_date <- as.Date(belknap_adm_all$booking_date, format = "%m/%d/%Y")
belknap_adm_all$release_date <- as.Date(belknap_adm_all$release_date, format = "%m/%d/%Y")

# create FY year variable
# will be able to filter by CY later
belknap_adm_all <- belknap_adm_all %>%
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
belknap_adm_all <- belknap_adm_all %>%
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
                release_type,
                sentence_status,
                everything()) %>% 
  mutate(county = "Belknap")

# create high utilizer variable
belknap_bookings <- belknap_adm_all %>%
  select(inmate_id, booking_date, fy) %>% 
  distinct() %>% 
  group_by(inmate_id, fy) %>%
  dplyr::summarise(num_bookings = n()) %>% 
  mutate(high_utilizer = ifelse(
    num_bookings >= 3, "High Utilizer", "Not High Utilizer")) 

# merge data back
belknap_adm_all <- left_join(belknap_adm_all, belknap_bookings, by = c("inmate_id", "fy")) 

# # remove charge codes and duplicates to get picture of cohort
# # keep sentence status - more rows for each charge and sentence status
# belknap_adm <- belknap_adm_all %>%
#   dplyr::select(inmate_id, 
#                 race, 
#                 yob, 
#                 age, 
#                 sex, 
#                 housing, 
#                 sentence_status,
#                 booking_date, 
#                 los, 
#                 fy, 
#                 num_bookings, 
#                 high_utilizer,
#                 county) %>%
#   distinct()
# 
# # remove charge codes and duplicates to get picture of cohort
# # remove sentence status - less rows because each row is a booking event
# belknap_booking <- belknap_adm_all %>%
#   dplyr::select(inmate_id, 
#                 race, 
#                 yob, 
#                 age, 
#                 sex,
#                 housing, 
#                 booking_date, 
#                 booking_type, 
#                 release_date,
#                 los, 
#                 fy,
#                 num_bookings, 
#                 high_utilizer,
#                 county) %>%
#   distinct()

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

# organize variables
merrimack_adm_all <- merrimack_adm_all %>%
  mutate(release_type = NA) %>% 
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
                release_type,
                sentence_status,
                everything()) %>% 
  mutate(county = "Merrimack")

# create high utilizer variable
merrimack_bookings <- merrimack_adm_all %>%
  select(inmate_id, booking_date, fy) %>% 
  distinct() %>% 
  group_by(inmate_id, fy) %>%
  dplyr::summarise(num_bookings = n()) %>% 
  mutate(high_utilizer = ifelse(
    num_bookings >= 3, "High Utilizer", "Not High Utilizer")) 

# merge data back
merrimack_adm_all <- left_join(merrimack_adm_all, merrimack_bookings, by = c("inmate_id", "fy")) 

###################
# Combine data
###################

nh_adm <- rbind(belknap_adm_all, merrimack_adm_all)
