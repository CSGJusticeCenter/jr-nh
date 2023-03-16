############################################
# Project: JRI New Hampshire
# File: cheshire.R
# Last updated: January 31, 2023
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

################################################################################

# Administrative data file

################################################################################

# Clean variable names
# Make charge code NA since county does not have charge code
# Assign race labels
# Make data names consistent with other counties
# Fix date formats
# Add county label
cheshire_adm_all <- cheshire_adm.xlsx %>%
  clean_names() %>%
  mutate(race_label = case_when(
    race == "A"  ~ "Asian/Pacific Islander",
    race == "B"  ~ "Black",
    race == "H"  ~ "Hispanic",
    race == "I"  ~ "American Indian/Alaskan Native",
    race == "L"  ~ "Hispanic",
    race == "P"  ~ "Asian/Pacific Islander",
    race == "U"  ~ "Unknown",
    race == "W"  ~ "White"
         )) %>%
  dplyr::select(id,
                inmate_id,
                yob,
                race_code = race,
                race_label,
                sex,
                housing = housing_instability_or_homelessness_indicator, # not reliable
                charge_code = charge_offence_code,
                charge_desc = charged_offense_code_description_including_technical_violations_of_supervision,
                booking_date,
                booking_type,
                release_date,
                release_type,
                sentence_status) %>%
  mutate(booking_date = format(booking_date, format = "%m/%d/%Y"),
         release_date = format(release_date, format = "%m/%d/%Y"),
         county = "Cheshire") %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y")) %>%
  distinct()

# Create fy, age, los, recode race, and order variables
cheshire_adm <- fnc_data_setup(cheshire_adm_all)

# Add booking id using id and booking date
cheshire_adm <- fnc_booking_id(cheshire_adm, "Cheshire")

# Calculate los (release date - booking date)
cheshire_adm <- fnc_los(cheshire_adm)

# Create high utilizer variables
df_hu <- fnc_create_high_utilizer_variables(cheshire_adm)
cheshire_adm <- left_join(cheshire_adm, df_hu, by = c("id"))

###################################

# Standardize sentence statuses across counties so they have these categories:
# 1) PROTECTIVE CUSTODY
# 2) PRETRIAL
# 3) SENTENCED
# 4) NH STATE PRISONER
# 4) UNKNOWN
# 5) OTHER

###################################

booking_recordings_cheshire <- fnc_investigate_booking_recordings(cheshire_adm)
# Booking types:
# ADULT ORDER OF COMMITMENT
# Capias
# DETAINEE REQUEST
# DETAINER
# ELECTRONIC BENCH WARRANT
# FEDERAL HOLD
# FELONY FIRST
# Mittimus
# PAROLE
# PROBATION
# SUPERIOR COURT ARREST WARRANT
# UNKNOWN

# Sentence statuses:
# DUAL STATUS
# FEDERAL INMATE
# HOLD FOR OTHER AGENCY
# HOLD FOR STATE PRISON
# PRE-TRIAL
# PRE-TRIAL / DRUG COURT
# PRE-TRIAL / EM
# PROTECTIVE CUSTODY
# SENTENCED
# SENTENCED / DRUG COURT
# SENTENCED / EM
# SENTENCED / PROGRAM
# SENTENCED / WEEKENDS
# SENTENCED / WORK RELEASE

# If charge is temporary removal or transfer and sentence status indicates PC hold then it isn't a PC hold.
# Change sentence status to unknown for these since they aren't PC holds.
# Standardize booking info so it's consistent across counties
cheshire_adm <- cheshire_adm %>%

  mutate(charge_desc     = as.character(charge_desc),
         booking_type    = as.character(booking_type),
         release_type    = as.character(release_type),
         sentence_status = as.character(sentence_status),
         charge_desc     = toupper(charge_desc),
         booking_type    = toupper(booking_type),
         release_type    = toupper(release_type),
         sentence_status = toupper(sentence_status)) %>%

  mutate(sentence_status = as.character(sentence_status)) %>%
  mutate(booking_type    = as.character(booking_type)) %>%

  mutate(sentence_status_standard = case_when(
    # PROTECTIVE CUSTODY
    charge_desc == "PROTECTIVE CUSTODY" | charge_desc == "PROTECTIVE CUSTODY - DRUGS" & los <= 2 ~ "PROTECTIVE CUSTODY",

    # This was a mistake to label them as PC hold - checked with county
    # When you view these charge descriptions, there are three records. One for a transfer, one for public urination, and another for a dui
    sentence_status == "PROTECTIVE CUSTODY" &
      charge_desc != "PROTECTIVE CUSTODY - DRUGS" &
      charge_desc != "PROTECTIVE CUSTODY"                                            ~ "UNKNOWN",

    # PRETRIAL
    str_detect("PRE-TRIAL", sentence_status)                                         ~ "PRETRIAL",
    str_detect("PRE-TRIAL / DRUG COURT", sentence_status)                            ~ "PRETRIAL",
    str_detect("PRE-TRIAL / EM", sentence_status)                                    ~ "PRETRIAL",

    # SENTENCED
    str_detect("SENTENCED", sentence_status)                                         ~ "SENTENCED",
    str_detect("SENTENCED / DRUG COURT", sentence_status)                            ~ "SENTENCED",
    str_detect("SENTENCED / EM", sentence_status)                                    ~ "SENTENCED",
    str_detect("SENTENCED / PROGRAM", sentence_status)                               ~ "SENTENCED",
    str_detect("SENTENCED / WEEKENDS", sentence_status)                              ~ "SENTENCED",
    str_detect("SENTENCED / WORK RELEASE", sentence_status)                          ~ "SENTENCED",

    # NH STATE PRISONER
    str_detect("FEDERAL INMATE", sentence_status)                                    ~ "OTHER",
    str_detect("HOLD FOR STATE PRISON", sentence_status)                             ~ "OTHER",
    (is.na(sentence_status) & str_detect("FEDERAL HOLD", booking_type))              ~ "OTHER",

    # OTHER
    str_detect("DUAL STATUS", sentence_status)                                       ~ "OTHER",
    (is.na(sentence_status) & str_detect("FELONY FIRST", booking_type))              ~ "OTHER",
    str_detect("HOLD FOR OTHER AGENCY", sentence_status)                             ~ "OTHER",
    str_detect("DETAINEE REQUEST", sentence_status)                                  ~ "OTHER",
    str_detect("DETAINER", sentence_status)                                          ~ "OTHER",

    # UNKNOWN
    # no data in sentence status but info in booking type
    is.na(sentence_status)                                                           ~ "UNKNOWN",
    sentence_status == "UNKNOWN"                                                     ~ "UNKNOWN",

    TRUE ~ sentence_status)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, sentence_status, sentence_status_standard, release_type, booking_date, everything())

# Create pc hold variable
cheshire_adm <- cheshire_adm %>%
  mutate(pc_hold = ifelse(
    sentence_status_standard == "PROTECTIVE CUSTODY", "PC Hold", "Non-PC Hold")) %>%
  select(-c(los))

# Add sex code labels
cheshire_adm <- fnc_sex_labels(cheshire_adm)

# Add data labels
cheshire_adm <- fnc_add_data_labels(cheshire_adm)

# Remove duplicates
cheshire_adm <- cheshire_adm %>% distinct()

# Remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
# Create pretrial drug court and sentenced drug court variables
# If race or gender are NA in some bookings but present in others, use the recorded race or gender.
# If races or genders are different for the same person, make NA since we don't know which is correct.
cheshire_adm <- cheshire_adm %>%
  filter(booking_date >= "2018-06-30" & booking_date < "2021-07-01") %>%
  mutate(drug_court_pretrial  = case_when(sentence_status == "PRE-TRIAL / DRUG COURT" ~ 1,
                                          TRUE ~ 0),
         drug_court_sentenced = case_when(sentence_status == "SENTENCED / DRUG COURT" ~ 1,
                                          TRUE ~ 0)) %>%

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
cheshire_adm <- cheshire_adm %>%
  mutate(los_max = ifelse(los_max == -Inf, NA, los_max)) %>%
  filter(los_max >= 0 | is.na(los_max))  %>%
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
all_nas <- cheshire_adm %>%
  filter(is.na(charge_desc) &
           is.na(booking_type) &
           is.na(release_type) &
           is.na(sentence_status))
cheshire_adm1 <- cheshire_adm %>% anti_join(all_nas) %>% distinct()

# Make housing NA because this data is likely inaccurate. It says that no one was housing which is very unlikely.
cheshire_adm1 <- cheshire_adm1 %>% mutate(housing = NA)

################################################################################

# Medicaid data file

################################################################################

# Clean names
# Create race and gender labels
cheshire_medicaid <- cheshire_medicaid.xlsx %>%
  clean_names() %>%
  distinct() %>%
  rename(booking_date = admission_date_s,
         release_date = release_date_s,
         county = source_id) %>%
  mutate(booking_date = format(booking_date, format = "%m/%d/%Y"),
         release_date = format(release_date, format = "%m/%d/%Y")) %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y")) %>%
  mutate(jail_race = case_when(jail_race == "A"  ~ "Asian/Pacific Islander",
                               jail_race == "B"  ~ "Black",
                               jail_race == "H"  ~ "Hispanic",
                               jail_race == "I"  ~ "American Indian/Alaskan Native",
                               jail_race == "L"  ~ "Hispanic",
                               jail_race == "P"  ~ "Asian/Pacific Islander",
                               jail_race == "U"  ~ "Unknown",
                               jail_race == "W"  ~ "White"),
         jail_sex = case_when(jail_sex == "F"     ~ "Female",
                              jail_sex == "M"     ~ "Male",
                              jail_sex == "TRANF" ~ "Transgender")
  ) %>%
  mutate(jail_sex = ifelse(is.na(jail_sex), "Unknown", jail_sex))

# Create a unique booking id per person per booking date
cheshire_medicaid$booking_id <- cheshire_medicaid %>% group_indices(unique_person_id, booking_date)
cheshire_medicaid <- cheshire_medicaid %>%
  mutate(booking_id = paste("Cheshire", "booking", booking_id, sep = "_")) %>%
  select(unique_person_id, booking_id, everything())

# Remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
cheshire_medicaid <- cheshire_medicaid %>%
  filter(booking_date > "2018-06-30" & booking_date < "2021-07-01")

################################################################################

# Save files

################################################################################

save(cheshire_adm1, file=paste0(sp_data_path, "/Data/analysis/r_data/cheshire_adm.Rda", sep = ""))
