############################################
# Project: JRI New Hampshire
# File: coos.R
# Last updated: January 31, 2023
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

################################################################################

# Administrative data file

################################################################################

# Clean variable names
# Make booking type and release type NA since county doesn't have this data
# Assign race labels
# Make data names consistent with other counties
# Fix date formats
# Add county label
coos_adm_all <- coos_adm.xlsx %>%
  clean_names() %>%
  mutate(booking_type = NA,
         release_type = NA,
         race_label = case_when(race == "A"  ~ "Asian/Pacific Islander",
                                race == "B"  ~ "Black",
                                race == "H"  ~ "Hispanic",
                                race == "I"  ~ "American Indian/Alaskan Native",
                                race == "U"  ~ "Unknown",
                                race == "W"  ~ "White")) %>%
  dplyr::select(id = unique_id,
                inmate_id,
                yob,
                race_code = race,
                race_label,
                sex,
                homeless = homelessness_indicator,
                charge_code,
                charge_desc = charges,
                booking_date = booking_dt_tm,
                booking_type,
                release_date = release_dt_tm,
                release_type,
                sentence_status) %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y"),
         county = "Coos",
         homeless = case_when(homeless == "Unhoused" ~ "Homeless",
                              homeless == "Housed" ~ "Not Homeless",
                              is.na(homeless) ~ "Unknown")) %>%
  distinct()

# Create fy, age, los, recode race, and order variables
coos_adm <- fnc_data_setup(coos_adm_all)

# Add booking id using id and booking date
coos_adm <- fnc_booking_id(coos_adm, "Coos")

# Calculate los (release date - booking date)
coos_adm <- fnc_los(coos_adm)

# Create high utilizer variables
df_hu <- fnc_create_high_utilizer_variables(coos_adm)
coos_adm <- left_join(coos_adm, df_hu, by = c("id"))

###################################

# Standardize sentence statuses across counties so they have these categories:
# 1) PROTECTIVE CUSTODY
# 2) PRETRIAL
# 3) SENTENCED
# 4) NH STATE PRISONER
# 4) UNKNOWN
# 5) OTHER

###################################

booking_recordings_coos <- fnc_investigate_booking_recordings(coos_adm)

# Standardize booking info so it's consistent across counties
coos_adm <- coos_adm %>% select(-c(los)) %>% distinct() %>%

  mutate(charge_desc     = as.character(charge_desc),
         booking_type    = as.character(booking_type),
         release_type    = as.character(release_type),
         sentence_status = as.character(sentence_status),
         charge_desc     = toupper(charge_desc),
         booking_type    = toupper(booking_type),
         release_type    = toupper(release_type),
         sentence_status = toupper(sentence_status)) %>%

  mutate(sentence_status_standard = case_when(sentence_status == "PRETRIAL"    ~ "PRETRIAL",
                                              sentence_status == "DUAL STATUS" ~ "OTHER",
                                              sentence_status == "SENTENCED"   ~ "SENTENCED",
                                              is.na(sentence_status)           ~ "UNKNOWN",

                                              TRUE ~ sentence_status)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, sentence_status, sentence_status_standard, release_type, booking_date, everything()) %>%
  distinct()

# Create pc hold variable
coos_adm <- coos_adm %>%
  mutate(pc_hold = "Non-PC Hold")

# Add sex code labels
coos_adm <- fnc_sex_labels(coos_adm)

# Add data labels
coos_adm <- fnc_add_data_labels(coos_adm)

# Remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
# Create pretrial drug court and sentenced drug court variables - NA, since there is no data on drug courts for coos
# If race or gender are NA in some bookings but present in others, use the recorded race or gender.
# If races or genders are different for the same person, make NA since we don't know which is correct.
coos_adm <- coos_adm %>%
  filter(booking_date >= "2018-06-30" & booking_date < "2021-07-01") %>%
  mutate(drug_court_pretrial  = NA,
         drug_court_sentenced = NA) %>%

  # Race
  dplyr::group_by(id) %>%
  fill(race, .direction = "downup") %>%
  distinct() %>%
  group_by(id) %>%
  mutate(different_race_recorded = n_distinct(race) == 1) %>%
  mutate(race = ifelse(different_race_recorded == FALSE, NA, race)) %>%
  distinct() %>%

  # Gender
  dplyr::group_by(id) %>%
  fill(gender, .direction = "downup") %>%
  distinct() %>%
  group_by(id) %>%
  mutate(different_gender_recorded = n_distinct(gender) == 1) %>%
  mutate(gender = ifelse(different_gender_recorded == FALSE, NA, gender)) %>%
  distinct() %>%
  select(-different_gender_recorded, -different_race_recorded)

# Fix los issues
# Remove negatives because of data entry issues with booking and release dates
# If release date is missing, then change los to NA instead of Inf
# Create los categories
coos_adm <- coos_adm %>%
  mutate(los_max = ifelse(los_max == -Inf, NA, los_max)) %>%
  filter(los_max >= 0 | is.na(los_max)) %>%
  mutate(los_category =
           case_when(los_max == 0 ~ "0",
                     los_max == 1 ~ "1",
                     los_max == 2 ~ "2",
                     los_max == 3 ~ "3",
                     los_max == 4 ~ "4",
                     los_max == 5 ~ "5",
                     los_max >= 6   & los_max <= 10  ~ "6-10",
                     los_max >= 11  & los_max <= 30  ~ "11-30",
                     los_max >= 31  & los_max <= 50  ~ "31-50",
                     los_max >= 50  & los_max <= 100 ~ "50-100",
                     los_max >= 101 & los_max <= 180 ~ "101-180",
                     los_max >  180              ~ "Over 180")) %>%
  mutate(los_category = factor(los_category,
                               levels = c("0",
                                          "1",
                                          "2",
                                          "3",
                                          "4",
                                          "5",
                                          "6-10",
                                          "11-30",
                                          "31-50",
                                          "50-100",
                                          "101-180",
                                          "Over 180")))

# Remove rows with all missing data
# Find and remove bookings that have no information
all_nas <- coos_adm %>%
  filter(is.na(charge_desc) &
           is.na(booking_type) &
           is.na(release_type) &
           is.na(sentence_status))
coos_adm1 <- coos_adm %>% anti_join(all_nas) %>% distinct()

################################################################################

# Medicaid data file

################################################################################

# Clean names
# Create race and gender labels
# Fix date formats
coos_medicaid <- coos_medicaid.xlsx %>%
  clean_names() %>%
  distinct() %>%
  mutate(jail_race = case_when(jail_race == "A"  ~ "Asian/Pacific Islander",
                               jail_race == "B"  ~ "Black",
                               jail_race == "H"  ~ "Hispanic",
                               jail_race == "I"  ~ "American Indian/Alaskan Native",
                               jail_race == "U"  ~ "Unknown",
                               jail_race == "W"  ~ "White"),
         jail_sex = case_when(jail_sex == "F"     ~ "Female",
                              jail_sex == "M"     ~ "Male")) %>%
  mutate(jail_sex = ifelse(is.na(jail_sex), "Unknown", jail_sex)) %>%
  mutate(booking_dt_tm = .POSIXct(booking_dt_tm, tz="UTC"),
         release_dt_tm = .POSIXct(release_dt_tm, tz="UTC")) %>%
  mutate(booking_dt_tm =   format(booking_dt_tm, "%m/%d/%Y"),
         release_dt_tm =   format(release_dt_tm, "%m/%d/%Y")) %>%
  mutate(booking_dt_tm =  as.Date(booking_dt_tm, format = "%m/%d/%Y"),
         release_dt_tm =  as.Date(release_dt_tm, format = "%m/%d/%Y"))

# Create a unique booking id per person per booking date
coos_medicaid$booking_id <- coos_medicaid %>% group_indices(unique_person_id, booking_dt_tm)
coos_medicaid <- coos_medicaid %>%
  rename(booking_date = booking_dt_tm,
         release_date = release_dt_tm,
         county = source_id) %>%
  mutate(booking_id = paste("Coos", "booking", booking_id, sep = "_")) %>%
  select(unique_person_id, booking_id, everything())

# Remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
coos_medicaid <- coos_medicaid %>%
  filter(booking_date > "2018-06-30" & booking_date < "2021-07-01")

################################################################################

# Save files

################################################################################

save(coos_adm1, file=paste0(sp_data_path, "/Data/r_data/coos_adm.Rda", sep = ""))
