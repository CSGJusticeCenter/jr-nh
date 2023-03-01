############################################
# Project: JRI New Hampshire
# File: carroll.R
# Last updated: February 10, 2023
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

################################################################################

# Administrative data file

################################################################################

# Clean variable names
# Make 3rd row the header, rename variables
carroll_adm_all <- carroll_bookings.xlsx %>%
  row_to_names(row_number = 3) %>%
  clean_names() %>%
  rename(inmate_id = inmate_number,
         charge_code = na,
         charge_desc = na_2,
         booking_type = na_3) %>%
  select(-c(na_4:na_11)) %>%

  # Set release type and housing to NA since we don't have this data
  mutate(release_type = NA,
         housing = NA,
         id = NA,
         yob = as.numeric(yob),
         county = "Carroll") %>%

  # If NA, use inmate ID, race, sex, yob, sentence status from previous row
  fill(inmate_id, .direction = "down") %>%
  fill(race, .direction = "down") %>%
  fill(sex, .direction = "down") %>%
  fill(yob, .direction = "down") %>%
  fill(sentence_status, .direction = "down") %>%

  # Change to date format to get rid of unneccessary text
  mutate(booking_dt_tm = as.Date(booking_dt_tm, format = "%m/%d/%Y"),
         release_dt_tm = as.Date(release_dt_tm, format = "%m/%d/%Y")) %>%

  # If NA, use booking date and release date from previous row
  fill(booking_dt_tm, .direction = "down") %>%
  fill(release_dt_tm, .direction = "down") %>%
  filter(charge_code != "Charge Code") %>%

  # Remove duplicates ---- what do reference numbers mean? Someone can have 4 charges for simple assult which looks like duplicates but then have multiple reference numbers
  distinct() %>%

  # Fix race variable
  mutate(race_label = case_when(race == "A"  ~ "Asian/Pacific Islander",
                                race == "B"  ~ "Black",
                                race == "I"  ~ "American Indian/Alaskan Native",
                                race == "U"  ~ "Unknown",
                                race == "W"  ~ "White")) %>%
  dplyr::select(id,
                inmate_id,
                yob,
                race_code = race,
                race_label,
                sex,
                housing,
                charge_code,
                charge_desc,
                booking_date = booking_dt_tm,
                booking_type,
                release_date = release_dt_tm,
                release_type,
                sentence_status,
                county)

# Create fy, age, los, recode race, and order variables
carroll_adm <- fnc_data_setup(carroll_adm_all)

# Add booking id using id and booking date
carroll_adm <- fnc_booking_id(carroll_adm, "Carroll")

# Calculate los (release date - booking date)
carroll_adm <- fnc_los(carroll_adm)

# Create high utilizer variables
df_hu <- fnc_create_high_utilizer_variables(carroll_adm)
carroll_adm <- left_join(carroll_adm, df_hu, by = c("id"))

###################################

# Standardize sentence statuses across counties so they have these categories:
# 1) PROTECTIVE CUSTODY
# 2) PRETRIAL
# 3) SENTENCED
# 4) NH STATE PRISONER
# 4) UNKNOWN
# 5) OTHER

###################################

booking_recordings_carroll <- fnc_investigate_booking_recordings(carroll_adm)
# Booking types
# DETAINEE REQUEST
# BAIL ORDER
# COURT COMMITTED
# ARREST WARRANT
# ELECTRONIC BENCH WARRANT
# DETENTION ORDER
# ADULT ORDER OF COMMITMENT
# CAPIAS
# Converted Document

# Sentence statuses
# AWAITING TRIAL
# BAIL SET
# BOND DENIED
# DETAINER
# DISMISSED
# HELD
# PRE-TRIAL
# PROTECTIVE CUSTODY
# SENTENCE SUSPENDED
# SENTENCED
# SENTENCED FINES
# STATE PRISONER

# Standardize booking info so it's consistent across counties
carroll_adm <- carroll_adm %>%

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
    charge_desc == "PROTECTIVE CUSTODY" & los <= 2                                   ~ "PROTECTIVE CUSTODY", # some PC holds have very long lengths of stay and
    charge_desc != "PROTECTIVE CUSTODY" & sentence_status == "PROTECTIVE CUSTODY"    ~ "PROTECTIVE CUSTODY",

    # PRETRIAL
    str_detect("PRE-TRIAL", sentence_status)                                         ~ "PRETRIAL",
    str_detect("AWAITING TRIAL", sentence_status)                                    ~ "PRETRIAL",
    str_detect("BAIL SET", sentence_status)                                          ~ "PRETRIAL",
    str_detect("BOND DENIED", sentence_status)                                       ~ "PRETRIAL",
    str_detect("HELD", sentence_status)                                              ~ "PRETRIAL",

    # SENTENCED
    str_detect("SENTENCED", sentence_status)                                         ~ "SENTENCED",
    str_detect("SENTENCED FINES", sentence_status)                                   ~ "SENTENCED",
    str_detect("SENTENCED / DETAINER", sentence_status)                              ~ "SENTENCED",
    str_detect("DETAINER", sentence_status)                                          ~ "SENTENCED",
    str_detect("SENTENCE SUSPENDED", sentence_status)                                ~ "SENTENCED",

    # NH STATE PRISONER
    str_detect("STATE PRISONER", sentence_status)                                    ~ "OTHER",

    # OTHER
    str_detect("DISMISSED", sentence_status)                                         ~ "OTHER",

    TRUE ~ sentence_status)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, sentence_status, sentence_status_standard, release_type, booking_date, everything())

# create pc hold variable
carroll_adm <- carroll_adm %>%
  mutate(pc_hold = ifelse(
    sentence_status_standard == "PROTECTIVE CUSTODY", "PC Hold", "Non-PC Hold"
)) %>% select(-c(los)) %>% distinct()

# Add sex code labels
carroll_adm <- fnc_sex_labels(carroll_adm)

# Add data labels
carroll_adm <- fnc_add_data_labels(carroll_adm)

# Remove duplicates
carroll_adm <- carroll_adm %>% distinct()

# Remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
# Create pretrial drug court and sentenced drug court variables - NA, since there is no data on drug courts for carroll
# If race or gender are NA in some bookings but present in others, use the recorded race or gender.
# If races or genders are different for the same person, make NA since we don't know which is correct.
carroll_adm <- carroll_adm %>%
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
carroll_adm <- carroll_adm %>%
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
all_nas <- carroll_adm %>%
  filter(is.na(charge_desc) &
           is.na(booking_type) &
           is.na(release_type) &
           is.na(sentence_status))
carroll_adm1 <- carroll_adm %>% anti_join(all_nas) %>% distinct()

################################################################################

# Medicaid data file

################################################################################

# Clean names
# Fix date formats
carroll_medicaid <- carroll_medicaid.xlsx %>%
  clean_names() %>%
  mutate(booking_dt_tm = .POSIXct(booking_dt_tm, tz="UTC"),
         release_dt_tm = .POSIXct(release_dt_tm, tz="UTC")) %>%
  mutate(booking_dt_tm = format(booking_dt_tm, "%m/%d/%Y"),
         release_dt_tm = format(release_dt_tm, "%m/%d/%Y")) %>%
  mutate(booking_dt_tm = as.Date(booking_dt_tm, format = "%m/%d/%Y"),
         release_dt_tm = as.Date(release_dt_tm, format = "%m/%d/%Y")) %>%
  distinct()

# Create a unique booking id per person per booking date
carroll_medicaid$booking_id <- carroll_medicaid %>%
  dplyr::group_indices(unique_person_id, booking_dt_tm)
carroll_medicaid <- carroll_medicaid %>%
  rename(booking_date = booking_dt_tm,
         release_date = release_dt_tm,
         county = source_id) %>%
  mutate(booking_id = paste("Carroll", "booking", booking_id, sep = "_")) %>%
  select(unique_person_id, booking_id, everything())

# Remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
# Create race and gender labels
carroll_medicaid <- carroll_medicaid %>%
  mutate(jail_race = case_when(jail_race == "A"  ~ "Asian/Pacific Islander",
                               jail_race == "B"  ~ "Black",
                               jail_race == "I"  ~ "American Indian/Alaskan Native",
                               jail_race == "U"  ~ "Unknown",
                               jail_race == "W"  ~ "White"),
         jail_sex = case_when(jail_sex == "F"  ~ "Female",
                              jail_sex == "M"  ~ "Male")
  ) %>%
  mutate(jail_sex = ifelse(is.na(jail_sex), "Unknown", jail_sex)) %>%
  filter(booking_date > "2018-06-30" & booking_date < "2021-07-01")

################################################################################

# Save files

################################################################################

save(carroll_adm1, file=paste0(sp_data_path, "/Data/r_data/carroll_adm.Rda", sep = ""))
