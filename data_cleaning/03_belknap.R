############################################
# Project: JRI New Hampshire
# File: belknap.R
# Last updated: January 30, 2023
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
belknap_adm_all <- belknap_adm.xlsx %>%
  clean_names() %>%
  mutate(charge_code = NA,
         # Comments show what the jail indicated the letter stands for
         race_label = case_when(race == "A"  ~ "Asian/Pacific Islander",
                                race == "B"  ~ "Black",
                                race == "C"  ~ "Asian/Pacific Islander",       # Chinese
                                race == "H"  ~ "Hispanic",
                                race == "I"  ~ "American Indian/Alaskan Native",
                                race == "NH" ~ "Unknown",                      # Delete
                                race == "O"  ~ "Unknown",                      # Other
                                race == "P"  ~ "Unknown",                      # Portuguese
                                race == "U"  ~ "Unknown",
                                race == "W"  ~ "White",
                                race == "X"  ~ "Unknown"                       # Delete
                                )) %>%
  dplyr::select(id = unique_person_id,
                inmate_id,
                yob = year_of_birth,
                race_code = race,
                race_label,
                sex,
                housing = housing_instability_or_homelessness_indicator,
                charge_code,
                charge_desc = charged_offense_code_description_including_technical_violations_of_supervision,
                booking_date,
                booking_type,
                release_date,
                release_type,
                sentence_status = sentencing_status) %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y"),
         county = "Belknap")

# Create fy, age, los, recode race, and order variables
belknap_adm <- fnc_data_setup(belknap_adm_all)

# Add booking id using id and booking date
belknap_adm <- fnc_booking_id(belknap_adm, "Belknap")

# Calculate los (release date - booking date)
belknap_adm <- fnc_los(belknap_adm)

# Create high utilizer variables
df_hu <- fnc_create_high_utilizer_variables(belknap_adm)
belknap_adm <- left_join(belknap_adm, df_hu, by = c("id"))

###################################

# Standardize sentence statuses across counties so they have these categories:
# 1) PROTECTIVE CUSTODY
# 2) PRETRIAL
# 3) SENTENCED
# 4) NH STATE PRISONER
# 4) UNKNOWN
# 5) OTHER

###################################

booking_recordings_belknap <- fnc_investigate_booking_recordings(belknap_adm)
# Booking types:
# ADMIN TRANSFER -> Other
# DUAL -> Other
# NH STATE PRISONER
# OVERNIGHT HOLD -> Other
# PRETRIAL
# PROTECTIVE CUSTODY
# SENTENCED

# Remove LOS (keep los max) and release date due to release date differences by booking id.
# If charge is present then it was a mistake to book them as a PC hold.
# Change to non-PC hold and the booking type (create new variable to preserve raw data) to unknown these since they aren't PC holds. Keep charge info though.

belknap_adm <- belknap_adm %>%

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
    str_detect("PROTECTIVE CUSTODY|PROTECTIVE CUSTODY/INTOXICATION", charge_desc)       ~ "PROTECTIVE CUSTODY",

    # PRETRIAL
    str_detect("PRETRIAL", booking_type)                                                ~ "PRETRIAL",

    # SENTENCED
    str_detect("SENTENCED", booking_type)                                               ~ "SENTENCED",

    # NH STATE PRISONER
    str_detect("NH STATE PRISONER", booking_type)                                       ~ "OTHER",

    # OTHER
    str_detect("DUAL", booking_type)                                                    ~ "OTHER",
    str_detect("OVERNIGHT HOLD", booking_type)                                          ~ "OTHER",
    str_detect("ADMIN TRANSFER", booking_type)                                          ~ "OTHER",

    # UNKNOWN bc they have charges but were booked as PC which was a mistake (only 11 records)
    ((charge_desc == "TEMPORARY REMOVAL OR TRANSFER" |
        charge_desc == "RESIST ARREST OR DETENTION 642:2" |
        charge_desc == "DISORDERLY CONDUCT 644:2" |
        charge_desc == "DRIVING OR OPERATING UNDER THE INFLUENCE OF DRUGSOR LIQUOR 265-A:2" |
        charge_desc == "RESISTING ARREST 594:5"|
        charge_desc == "SIMPLE ASSAULT 631:2-A" |
        charge_desc == "VIOLATION OF PROTECTIVE ORDER") &
       booking_type == "PROTECTIVE CUSTODY")                                             ~ "UNKNOWN",

    TRUE ~ booking_type)) %>%

  select(county,
         fy,
         id,
         inmate_id,
         booking_id,
         charge_code,
         charge_desc,
         booking_type,
         sentence_status,
         sentence_status_standard,
         release_type,
         booking_date,
         everything())

# Create pc hold variable
belknap_adm <- belknap_adm %>%
  mutate(pc_hold = ifelse(
    sentence_status_standard == "PROTECTIVE CUSTODY", "PC Hold", "Non-PC Hold"
  )) %>%
  select(-c(los))

# Add sex code labels
belknap_adm <- fnc_sex_labels(belknap_adm)

# Add data labels
belknap_adm <- fnc_add_data_labels(belknap_adm)

# Remove duplicates
belknap_adm <- belknap_adm %>% distinct()

# Remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
belknap_adm <- belknap_adm %>%
  filter(booking_date >= "2018-06-30" & booking_date < "2021-07-01")

# Create pretrial drug court and sentenced drug court variables - NA, since there is no data on drug courts for Belknap
belknap_adm <- belknap_adm %>%
  mutate(drug_court_pretrial  = NA,
         drug_court_sentenced = NA)

# If race or gender are NA in some bookings but present in others, use the recorded race or gender.
# If races or genders are different for the same person, make NA since we don't know which is correct.
belknap_adm <- belknap_adm %>%

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
belknap_adm <- belknap_adm %>%
  mutate(los_max = ifelse(los_max == -Inf, NA, los_max)) %>%
  filter(los_max >= 0 | is.na(los_max))

# Create los categories
belknap_adm <- belknap_adm %>%
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
all_nas <- belknap_adm %>%
  filter(is.na(charge_desc) &
           is.na(booking_type) &
           is.na(release_type) &
           is.na(sentence_status))
belknap_adm1 <- belknap_adm %>% anti_join(all_nas) %>% distinct()

################################################################################

# Medicaid data file

################################################################################

# Clean names
# Create race and gender labels
belknap_medicaid <- belknap_medicaid.xlsx %>%
  clean_names() %>%
  distinct() %>%
  rename(county = source_id) %>%
  mutate(jail_race = case_when(jail_race == "A"  ~ "Asian/Pacific Islander",
                               jail_race == "B"  ~ "Black",
                               jail_race == "C"  ~ "Asian/Pacific Islander",       # Chinese
                               jail_race == "H"  ~ "Hispanic",
                               jail_race == "I"  ~ "American Indian/Alaskan Native",
                               jail_race == "NH" ~ "Unknown",                      # Delete
                               jail_race == "O"  ~ "Unknown",                      # Other
                               jail_race == "P"  ~ "Unknown",                      # Portuguese
                               jail_race == "U"  ~ "Unknown",
                               jail_race == "W"  ~ "White",
                               jail_race == "X"  ~ "Unknown"                       # Delete
                               ),
         jail_sex = case_when(jail_sex == "F"  ~ "Female",
                              jail_sex == "M"  ~ "Male",
                              jail_sex == "T"  ~ "Transgender")) %>%
  mutate(jail_sex = ifelse(is.na(jail_sex), "Unknown", jail_sex))

# Create a unique booking id per person per booking date
belknap_medicaid$booking_id <- belknap_medicaid %>% group_indices(unique_person_id, booking_date)
belknap_medicaid <- belknap_medicaid %>%
  mutate(booking_id = paste("Belknap", "booking", booking_id, sep = "_")) %>%
  select(unique_person_id, booking_id, everything())

# Remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
belknap_medicaid <- belknap_medicaid %>%
  filter(booking_date > "2018-06-30" & booking_date < "2021-07-01")

################################################################################

# Save files

################################################################################

save(belknap_adm1, file=paste0(sp_data_path, "/Data/r_data/belknap_adm.Rda", sep = ""))
