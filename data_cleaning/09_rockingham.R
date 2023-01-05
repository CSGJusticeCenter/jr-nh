############################################
# Project: JRI New Hampshire
# File: rockingham.R
# Last updated: December 8, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

###################
# Rockingham County
###################

rockingham_adm_all <- rockingham_adm.xlsx %>%
  clean_names() %>%
  mutate(id = NA,
         release_type = NA,
         race_label = case_when(
           race == "A" ~ "Asian/Pacific Islander",
           race == "B" ~ "Black",
           race == "H" ~ "Hispanic",
           race == "I" ~ "American Indian/Alaskan Native",
           race == "O" ~ "Unknown",
           race == "U" ~ "Unknown",
           race == "W" ~ "White"
         )) %>%
  dplyr::select(id,
                inmate_id = inmate_id_number,
                yob,
                race_code = race,
                race_label,
                sex,
                housing = homelessness,
                charge_code = charge_id,
                charge_desc = charge_description,
                booking_date = arrival_date_and_time,
                booking_type = commitment_documentation,
                release_date = release_date_and_time,
                release_type,
                sentence_status) %>%
  mutate(booking_date = format(booking_date, format = "%m/%d/%Y"),
         release_date = format(release_date, format = "%m/%d/%Y"),
         county = "Rockingham") %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y")) %>%
  distinct()

# Custom functions below creates the variables we need and relabels codes so they're consistent across counties.
# Creates booking_id, los, fy, num_entrances, high_utilizer_1_pct(y/n), high_utilizer_5_pct(y/n), high_utilizer_10_pct(y/n),
#    pc_hold_booking(y/n), pc_hold_charge(y/n), pc_hold_sentence(y/n), pc_hold_release(y/n),
#    pc_hold(y/n) which is the overall pc hold variable (if pc hold was indicated in other pc variables).
# Ignore warning messages.

# Note about LOS: some people can be booked on the same day for multiple charges.
# For example, someone could enter jail on a protective custody hold on 10/19 with a release
#   date of 10/20 but also be booked for a criminal charge on 10/19 with a release date of 10/26
#   For this reason, find the maximum release date for each booking id (created using id and booking_date).

# Create fy, age, los, recode race, and order variables
rockingham_adm <- fnc_data_setup(rockingham_adm_all)

# Add booking id using id and booking date
rockingham_adm <- fnc_booking_id(rockingham_adm, "Rockingham")

# Calculate los (release date - booking date)
rockingham_adm <- fnc_los(rockingham_adm)

# Create high utilizer variables
df_hu <- fnc_create_high_utilizer_variables(rockingham_adm)
rockingham_adm <- left_join(rockingham_adm, df_hu, by = c("id", "fy"))

###################################

# Standardize sentence statuses across counties so they have these categories:
# 1) PROTECTIVE CUSTODY
# 2) PRETRIAL
# 3) SENTENCED
# 4) NH STATE PRISONER
# 4) UNKNOWN
# 5) OTHER

###################################

booking_recordings_rockingham <- fnc_investigate_booking_recordings(rockingham_adm)

# Standardize booking info so it's consistent across counties
rockingham_adm <- rockingham_adm %>%

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
    charge_desc == "PROTECTIVE CUSTODY" & los < 2     ~ "PROTECTIVE CUSTODY", # PC holds over 2 days are likely not PC hold and pretrial instead

    # PRETRIAL
    str_detect("DIVERSION", sentence_status)          ~ "OTHER",
    str_detect("DIVERSION", sentence_status)          ~ "OTHER",

    # SENTENCED
    str_detect("SENTENCED", sentence_status)          ~ "SENTENCED",

    # NH STATE PRISONER

    # OTHER
    str_detect("ADMIN TRANSFER", sentence_status)     ~ "OTHER",
    str_detect("DIVERSION", sentence_status)          ~ "OTHER",
    str_detect("DUAL", sentence_status)               ~ "OTHER",
    str_detect("IAD INMATE", sentence_status)         ~ "OTHER",

    # UNKNOWN
    is.na(sentence_status)                            ~ "UNKNOWN",

    TRUE ~ sentence_status)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, sentence_status, sentence_status_standard, release_type, booking_date, everything())

# create pc hold variable
rockingham_adm <- rockingham_adm %>%
  mutate(pc_hold = ifelse(
    sentence_status_standard == "PROTECTIVE CUSTODY", "PC Hold", "Non-PC Hold"
  )) %>% select(-c(los, release_date)) %>% distinct()

# Add sex code labels
rockingham_adm <- fnc_sex_labels(rockingham_adm)

# Add data labels
rockingham_adm <- fnc_add_data_labels(rockingham_adm)

# Remove duplicates
rockingham_adm <- rockingham_adm %>% distinct()

# remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
rockingham_adm <- rockingham_adm %>%
  filter(booking_date >= "2018-06-30" & booking_date < "2021-07-01")

# create pretrial drug court and sentenced drug court variables - NA, since there is no data on drug courts for rockingham
rockingham_adm <- rockingham_adm %>%
  mutate(drug_court_pretrial  = ifelse(booking_type == "DRUG COURT SENTENCING ORDER" & sentence_status == "PRETRIAL", 1, 0),
         drug_court_sentenced = ifelse(booking_type == "DRUG COURT SENTENCING ORDER" & sentence_status == "SENTENCED", 1, 0))

# If race or gender are NA in some bookings but present in others, use the recorded race or gender.
# If races or genders are different for the same person, make NA since we don't know which is correct.
rockingham_adm <- rockingham_adm %>%

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
rockingham_adm <- rockingham_adm %>%
  mutate(los_max = ifelse(los_max == -Inf, NA, los_max)) %>%
  filter(los_max >= 0 | is.na(los_max))

# Create los categories
rockingham_adm <- rockingham_adm %>%
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

# Remove rows with all missing data (37 entries).
# Find and remove bookings that have no information. These are likely errors. - CHECK WITH EACH JAIL.
# Don't remove Strafford since all of their info is blank except for dates.
all_nas <- rockingham_adm %>%
  filter(is.na(charge_desc) &
           is.na(booking_type) &
           is.na(release_type) &
           is.na(sentence_status))
rockingham_adm1 <- rockingham_adm %>% anti_join(all_nas) %>% distinct()

################################################################################

# Charges

################################################################################













################################################################################

# Medicaid data file

################################################################################

# clean names
# create race labels
rockingham_medicaid <- rockingham_medicaid.xlsx %>%
  clean_names() %>%
  distinct() %>%
  rename(booking_date = arrival_date_and_time,
         release_date = release_date_and_time,
         county = source_id) %>%
  mutate(booking_date = format(booking_date, format = "%m/%d/%Y"),
         release_date = format(release_date, format = "%m/%d/%Y")) %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y"),
         jail_race = case_when(
           race == "A" ~ "Asian/Pacific Islander",
           race == "B" ~ "Black",
           race == "H" ~ "Hispanic",
           race == "I" ~ "American Indian/Alaskan Native",
           race == "O" ~ "Unknown",
           race == "U" ~ "Unknown",
           race == "W" ~ "White"
         ))

# create a unique booking id per person per booking date
rockingham_medicaid$booking_id <- rockingham_medicaid %>% group_indices(unique_person_id, booking_date)
rockingham_medicaid <- rockingham_medicaid %>%
  mutate(booking_id = paste("Rockingham", "booking", booking_id, sep = "_")) %>%
  select(unique_person_id, booking_id, everything())

# remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
rockingham_medicaid <- rockingham_medicaid %>%
  filter(booking_date >= "2018-06-30" & booking_date < "2021-07-01")

# # Does the medicaid file have the same number of unique individuals as the adm? Off by 33
# length(unique(rockingham_adm$id)); length(unique(rockingham_medicaid$unique_person_id))

################################################################################

# Save files

################################################################################

save(rockingham_adm1, file=paste0(sp_data_path, "/Data/r_data/rockingham_adm.Rda", sep = ""))
