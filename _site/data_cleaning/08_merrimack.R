############################################
# Project: JRI New Hampshire
# File: merrimack.R
# Last updated: January 30, 2023
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

################################################################################

# Administrative data file

################################################################################

# Clean variable names
# Make charge code and release type NA since county does not have this data
# Assign race labels
# Make data names consistent with other counties
# Fix date formats
# Add county label
merrimack_adm_all <- merrimack_adm.xlsx %>%
  clean_names() %>%
  mutate(charge_code = NA,
         release_type = NA,
         race_label = case_when(
           race == "A" ~ "Asian/Pacific Islander",
           race == "B" ~ "Black",
           race == "H" ~ "Hispanic",
           race == "I" ~ "American Indian/Alaskan Native",
           race == "O" ~ "Unknown",
           race == "P" ~ "Asian/Pacific Islander",
           race == "U" ~ "Unknown",
           race == "W" ~ "White",
           race == "X" ~ "Unknown"
         )) %>%
  dplyr::select(id = uniq_id,
                inmate_id = im_id,
                yob,
                race_code = race,
                race_label,
                sex,
                housing = housing_instability,
                charge_code,
                charge_desc = charges,
                booking_date,
                booking_type,
                release_date = rel_date,
                release_type,
                sentence_status) %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y"),
         county = "Merrimack") %>%
  distinct()

# Create fy, age, los, recode race, and order variables
merrimack_adm <- fnc_data_setup(merrimack_adm_all)

# Add booking id using id and booking date
merrimack_adm <- fnc_booking_id(merrimack_adm, "Merrimack")

# Calculate los (release date - booking date)
merrimack_adm <- fnc_los(merrimack_adm)

# Create high utilizer variables
df_hu <- fnc_create_high_utilizer_variables(merrimack_adm)
merrimack_adm <- left_join(merrimack_adm, df_hu, by = c("id"))

###################################

# Standardize sentence statuses across counties so they have these categories:
# 1) PROTECTIVE CUSTODY
# 2) PRETRIAL
# 3) SENTENCED
# 4) NH STATE PRISONER
# 4) UNKNOWN
# 5) OTHER

###################################

booking_recordings_merrimack <- fnc_investigate_booking_recordings(merrimack_adm)

# Standardize booking info so it's consistent across counties
merrimack_adm <- merrimack_adm %>%

  mutate(charge_desc     = as.character(charge_desc),
         booking_type    = as.character(booking_type),
         release_type    = as.character(release_type),
         sentence_status = as.character(sentence_status),
         charge_desc     = toupper(charge_desc),
         booking_type    = toupper(booking_type),
         release_type    = toupper(release_type),
         sentence_status = toupper(sentence_status)) %>%

  mutate(sentence_status_standard = case_when(

    charge_desc != "PROTECTIVE CUSTODY HOLD" & booking_type == "ADULT ORDER OF COMMITMENT" & sentence_status == "PC-IEA" ~ "UNKNOWN", # los too long, not PC hold

    charge_desc == "PROTECTIVE CUSTODY" & booking_type == "PROTECTIVE CUSTODY" &
       sentence_status == "24 HOUR HOLD"                                              ~ "PROTECTIVE CUSTODY",
    charge_desc == "PROTECTIVE CUSTODY" & booking_type == "ADULT ORDER OF COMMITMENT" ~ "PROTECTIVE CUSTODY",
    charge_desc == "PROTECTIVE CUSTODY" & booking_type == "ARREST WARRANT"            ~ "PROTECTIVE CUSTODY",
    charge_desc == "PROTECTIVE CUSTODY" & booking_type == "DETAINEE REQUEST" &
      sentence_status != "DUAL STATUS"                                                ~ "PROTECTIVE CUSTODY",
    charge_desc == "PROTECTIVE CUSTODY" & booking_type == "DETAINER"                  ~ "PROTECTIVE CUSTODY",
    charge_desc == "PROTECTIVE CUSTODY" & booking_type == "PROBATION"                 ~ "PROTECTIVE CUSTODY",
    charge_desc == "PROTECTIVE CUSTODY" &
      booking_type == "PROTECTIVE CUSTODY" & los <= 1                                 ~ "PROTECTIVE CUSTODY",

    sentence_status == "PC-IEA" & los <= 1                                            ~ "PROTECTIVE CUSTODY",
    sentence_status == "PROTECTIVE CUSTODY HOLD" & los <= 1                           ~ "PROTECTIVE CUSTODY",

    # PRETRIAL
    sentence_status == "PRE-TRIAL FELONY"                                             ~ "PRETRIAL",
    sentence_status == "PRE-TRIAL MISDEMEANOR"                                        ~ "PRETRIAL",
    charge_desc == "PROTECTIVE CUSTODY" &
      booking_type == "PROTECTIVE CUSTODY" &
      sentence_status == "PRE-TRIAL FELONY" &
      los > 1                                                                         ~ "PRETRIAL", # long LOS so pretrial

    # SENTENCED
    sentence_status == "SENTENCED FELONY"                                             ~ "SENTENCED",
    sentence_status == "SENTENCED MISDEMEANOR"                                        ~ "SENTENCED",
    charge_desc == "PROTECTIVE CUSTODY" &
      booking_type == "PROTECTIVE CUSTODY" &
      sentence_status == "SENTENCED FELONY"                                           ~ "SENTENCED", # long LOS so sentenced

    # NH STATE PRISONER

    # OTHER
    (sentence_status == "24 HOUR HOLD" & booking_type != "PROTECTIVE CUSTODY")        ~ "OTHER",
    (sentence_status == "24 HOUR HOLD" & is.na(booking_type))                         ~ "OTHER",
    sentence_status == "72 HOUR HOLD"                                                 ~ "OTHER",
    sentence_status == "DUAL STATUS"                                                  ~ "OTHER",
    sentence_status == "PAROLE VIOLATION"                                             ~ "OTHER",
    charge_desc == "PROTECTIVE CUSTODY" & sentence_status == "DUAL STATUS"            ~ "OTHER",  # long LOS so not a PC hold

    # UNKNOWN
    charge_desc == "PROTECTIVE CUSTODY" & booking_type == "PROTECTIVE CUSTODY" &
      sentence_status == "PROTECTIVE CUSTODY HOLD" & los > 1                          ~ "UNKNOWN", # long LOS so not a PC hold
    is.na(sentence_status)                                                            ~ "UNKNOWN",

    TRUE ~ sentence_status)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, sentence_status, sentence_status_standard, release_type, booking_date, everything())

# Create pc hold variable
merrimack_adm <- merrimack_adm %>%
  mutate(pc_hold = ifelse(
    sentence_status_standard == "PROTECTIVE CUSTODY", "PC Hold", "Non-PC Hold"
  )) %>% select(-c(los)) %>% distinct()

# Add sex code labels
merrimack_adm <- fnc_sex_labels(merrimack_adm)

# Add data labels
merrimack_adm <- fnc_add_data_labels(merrimack_adm)

# Remove duplicates
merrimack_adm <- merrimack_adm %>% distinct()

# Remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
merrimack_adm <- merrimack_adm %>%
  filter(booking_date >= "2018-06-30" & booking_date < "2021-07-01")

# Create pretrial drug court and sentenced drug court variables - NA, since there is no data on drug courts for merrimack
# there is info on drug court violations in the charge descriptions though
merrimack_adm <- merrimack_adm %>%
  mutate(drug_court_pretrial  = NA,
         drug_court_sentenced = NA)

# If race or gender are NA in some bookings but present in others, use the recorded race or gender.
# If races or genders are different for the same person, make NA since we don't know which is correct.
merrimack_adm <- merrimack_adm %>%

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
merrimack_adm <- merrimack_adm %>%
  mutate(los_max = ifelse(los_max == -Inf, NA, los_max)) %>%
  filter(los_max >= 0 | is.na(los_max))

# Create los categories
merrimack_adm <- merrimack_adm %>%
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
all_nas <- merrimack_adm %>%
  filter(is.na(charge_desc) &
           is.na(booking_type) &
           is.na(release_type) &
           is.na(sentence_status))
merrimack_adm1 <- merrimack_adm %>% anti_join(all_nas) %>% distinct()

################################################################################

# Medicaid data file

################################################################################

# Clean names
# Create race and gender labels
merrimack_medicaid <- merrimack_medicaid.xlsx %>%
  clean_names() %>%
  distinct() %>%
  rename(booking_date = book_date,
         release_date = rel_date,
         county = source_id) %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y"),
         jail_race = case_when(
           jail_race == "A" ~ "Asian/Pacific Islander",
           jail_race == "B" ~ "Black",
           jail_race == "H" ~ "Hispanic",
           jail_race == "I" ~ "American Indian/Alaskan Native",
           jail_race == "O" ~ "Unknown",
           jail_race == "P" ~ "Asian/Pacific Islander",
           jail_race == "U" ~ "Unknown",
           jail_race == "W" ~ "White",
           jail_race == "X" ~ "Unknown"
         ),
         jail_sex = case_when(jail_sex == "F"  ~ "Female",
                              jail_sex == "M"  ~ "Male",
                              jail_sex == "TF" ~ "Transgender",
                              jail_sex == "U"  ~ "Unknown")
  ) %>%
  mutate(jail_sex = ifelse(is.na(jail_sex), "Unknown", jail_sex))

# Create a unique booking id per person per booking date
merrimack_medicaid$booking_id <- merrimack_medicaid %>% group_indices(unique_person_id, booking_date)
merrimack_medicaid <- merrimack_medicaid %>%
  mutate(booking_id = paste("Merrimack", "booking", booking_id, sep = "_")) %>%
  select(unique_person_id, booking_id, everything())

# Remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
merrimack_medicaid <- merrimack_medicaid %>%
  filter(booking_date > "2018-06-30" & booking_date < "2021-07-01")

# # Does the medicaid file have the same number of unique individuals as the adm? Off by 49
# length(unique(merrimack_adm$id)); length(unique(merrimack_medicaid$unique_person_id))

################################################################################

# Save files

################################################################################

save(merrimack_adm1, file=paste0(sp_data_path, "/Data/r_data/merrimack_adm.Rda", sep = ""))
