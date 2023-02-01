############################################
# Project: JRI New Hampshire
# File: sullivan.R
# Last updated: January 31, 2023
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

################################################################################

# Administrative data file

################################################################################

# Clean variable names
# Make release type NA since county doesn't have this data
# Assign race labels
# Make data names consistent with other counties
# Fix date formats
# Add county label
sullivan_adm_all <- sullivan_adm.xlsx %>%
  clean_names() %>%
  mutate(booking_date_time = as.POSIXct(booking_date_time, format = '%m/%d/%Y %H:%M:%S'),
         release_date_time = as.POSIXct(release_date_time, format = '%m/%d/%Y %H:%M:%S')) %>%
  mutate(booking_date_time = format(booking_date_time, "%m/%d/%Y"),
         release_date_time = format(release_date_time, "%m/%d/%Y")) %>%
  mutate(booking_date_time = as.Date(booking_date_time, format = "%m/%d/%Y"),
         release_date_time = as.Date(release_date_time, format = "%m/%d/%Y")) %>%
  mutate(release_type = NA,
         race_label = case_when(
           race == "A"  ~ "Asian/Pacific Islander",
           race == "B"  ~ "Black",
           race == "H"  ~ "Hispanic",
           race == "I"  ~ "American Indian/Alaskan Native",
           race == "K"  ~ "Black",                          # Black Hispanic
           race == "L"  ~ "White",                          # White Hispanic
           race == "N"  ~ "American Indian/Alaskan Native", # American Indian/Alaskan Native Hispanic
           race == "P"  ~ "Asian/Pacific Islander",
           race == "U"  ~ "Unknown",
           race == "W"  ~ "White")) %>%
  dplyr::select(id = id_number_inmate_number,
                inmate_id = inmate_num,
                yob = year,
                race_code = race,
                race_label,
                sex = gender,
                homeless = housing,
                charge_code = charge_id,
                charge_desc = charge,
                booking_date = booking_date_time,
                booking_type = detention_type,
                release_date = release_date_time,
                release_type,
                sentence_status) %>%
  mutate(county = "Sullivan",
         homeless = case_when(homeless = str_detect(homeless, regex("\\bhomeless", ignore_case = TRUE)) ~ "Homeless",
                              homeless = str_detect(homeless, regex("\\bunknown", ignore_case = TRUE)) ~ "Unknown",
                              is.na(homeless) ~ "Not Homeless",
                              TRUE ~ "Not Homeless")) %>%
  distinct()

sum# Create fy, age, los, recode race, and order variables
sullivan_adm <- fnc_data_setup(sullivan_adm_all)

# Add booking id using id and booking date
sullivan_adm <- fnc_booking_id(sullivan_adm, "Sullivan")

# Calculate los (release date - booking date)
sullivan_adm <- fnc_los(sullivan_adm)

# Create high utilizer variables
df_hu <- fnc_create_high_utilizer_variables(sullivan_adm)
sullivan_adm <- left_join(sullivan_adm, df_hu, by = c("id"))

###################################

# Standardize sentence statuses across counties so they have these categories:
# 1) PROTECTIVE CUSTODY
# 2) PRETRIAL
# 3) SENTENCED
# 4) NH STATE PRISONER
# 4) UNKNOWN
# 5) OTHER

###################################

booking_recordings_sullivan <- fnc_investigate_booking_recordings(sullivan_adm)

# Standardize booking info so it's consistent across counties
sullivan_adm <- sullivan_adm %>%

  mutate(charge_desc     = as.character(charge_desc),
         booking_type    = as.character(booking_type),
         release_type    = as.character(release_type),
         sentence_status = as.character(sentence_status),
         charge_desc     = toupper(charge_desc),
         booking_type    = toupper(booking_type),
         release_type    = toupper(release_type),
         sentence_status = toupper(sentence_status)) %>%

  mutate(sentence_status_standard = case_when(
    # PROTECTIVE CUSTODY
    charge_desc  == "Treatment and Services: Protective Custody" & los < 2        ~ "PROTECTIVE CUSTODY",
    booking_type == "PROTECTIVE CUSTODY" & los < 2                                ~ "PROTECTIVE CUSTODY",
    charge_desc  == "Intoxication." & booking_type == "24 HOUR DETENTION REQUEST" ~ "PROTECTIVE CUSTODY",

    # PRETRIAL
    sentence_status == "PRE-TRIAL"       ~ "PRETRIAL",

    # SENTENCED
    sentence_status == "SENTENCED"       ~ "SENTENCED",
    sentence_status == "SENTENCED T 1"   ~ "SENTENCED",
    sentence_status == "SENTENCED T 5"   ~ "SENTENCED",

    # NH STATE PRISONER

    # OTHER
    sentence_status == "DUAL"            ~ "OTHER",

    # UNKNOWN
    is.na(sentence_status)               ~ "UNKNOWN",

    TRUE ~ sentence_status)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, sentence_status, sentence_status_standard, release_type, booking_date, everything())

# Create pc hold variable
sullivan_adm <- sullivan_adm %>%
  mutate(pc_hold = ifelse(
    sentence_status_standard == "PROTECTIVE CUSTODY", "PC Hold", "Non-PC Hold"
  )) %>% select(-c(los)) %>% distinct()

# Add sex code labels
sullivan_adm <- fnc_sex_labels(sullivan_adm)

# Add data labels
sullivan_adm <- fnc_add_data_labels(sullivan_adm)

# Remove duplicates
sullivan_adm <- sullivan_adm %>% distinct()

# Remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
# Create pretrial drug court and sentenced drug court variables - NA, since there is no data on drug courts for sullivan
# If race or gender are NA in some bookings but present in others, use the recorded race or gender.
# If races or genders are different for the same person, make NA since we don't know which is correct.
sullivan_adm <- sullivan_adm %>%
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
sullivan_adm <- sullivan_adm %>%
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
all_nas <- sullivan_adm %>%
  filter(is.na(charge_desc) &
           is.na(booking_type) &
           is.na(release_type) &
           is.na(sentence_status))
sullivan_adm1 <- sullivan_adm %>% anti_join(all_nas) %>% distinct()

################################################################################

# Medicaid data file

################################################################################

# Clean names
# Create race and gender labels
# Fix date formats
sullivan_medicaid <- sullivan_medicaid.xlsx %>%
  clean_names() %>%
  distinct() %>%
  rename(booking_date = booking_date_time,
         release_date = release_date_time,
         county = source_id) %>%
  mutate(
    jail_race = case_when(
      jail_race == "A"  ~ "Asian/Pacific Islander",
      jail_race == "B"  ~ "Black",
      jail_race == "H"  ~ "Hispanic",
      jail_race == "I"  ~ "American Indian/Alaskan Native",
      jail_race == "K"  ~ "Black",                          # Black Hispanic
      jail_race == "L"  ~ "White",                          # White Hispanic
      jail_race == "N"  ~ "American Indian/Alaskan Native", # American Indian/Alaskan Native Hispanic
      jail_race == "P"  ~ "Asian/Pacific Islander",
      jail_race == "U"  ~ "Unknown",
      jail_race == "W"  ~ "White"),
    jail_sex = case_when(jail_sex == "F"  ~ "Female",
                         jail_sex == "M"  ~ "Male",
                         jail_sex == "U"  ~ "Unknown")) %>%
  mutate(jail_sex = ifelse(is.na(jail_sex), "Unknown", jail_sex)) %>%
  mutate(booking_date = as.POSIXct(booking_date, format = '%m/%d/%Y %H:%M:%S'),
         release_date = as.POSIXct(release_date, format = '%m/%d/%Y %H:%M:%S')) %>%
  mutate(booking_date = format(booking_date, "%m/%d/%Y"),
         release_date = format(release_date, "%m/%d/%Y")) %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y"))

# Create a unique booking id per person per booking date
sullivan_medicaid$booking_id <- sullivan_medicaid %>% group_indices(unique_person_id, booking_date)
sullivan_medicaid <- sullivan_medicaid %>%
  mutate(booking_id = paste("Sullivan", "booking", booking_id, sep = "_")) %>%
  select(unique_person_id, booking_id, everything())

# Remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
sullivan_medicaid <- sullivan_medicaid %>%
  filter(booking_date > "2018-06-30" & booking_date < "2021-07-01")

################################################################################

# Save files

################################################################################

save(sullivan_adm1, file=paste0(sp_data_path, "/Data/r_data/sullivan_adm.Rda", sep = ""))
