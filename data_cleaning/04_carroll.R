############################################
# Project: JRI New Hampshire
# File: carroll.R
# Last updated: December 8, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

###################
# Carroll County
###################

# Clean variable names
carroll_adm_all <- clean_names(carroll_bookings.xlsx)

# Not using releases for now because of merge issues
# carroll_releases <- clean_names(carroll_releases.xlsx)
# carroll_bookings <- clean_names(carroll_bookings.xlsx)
# Merge two adm files together
# carroll_adm_all <- merge(carroll_releases, carroll_bookings, by = c("inmate_id", "release_dt_tm"), all.x = TRUE)

# Change date formats for booking and release dataes
carroll_adm_all$booking_dt_tm <- .POSIXct(carroll_adm_all$booking_dt_tm, tz="UTC")
carroll_adm_all$booking_dt_tm <-   format(carroll_adm_all$booking_dt_tm, "%m/%d/%Y")
carroll_adm_all$booking_dt_tm <-  as.Date(carroll_adm_all$booking_dt_tm, format = "%m/%d/%Y")
carroll_adm_all$release_dt_tm <- .POSIXct(carroll_adm_all$release_dt_tm, tz="UTC")
carroll_adm_all$release_dt_tm <-   format(carroll_adm_all$release_dt_tm, "%m/%d/%Y")
carroll_adm_all$release_dt_tm <-  as.Date(carroll_adm_all$release_dt_tm, format = "%m/%d/%Y")

# Set up data to be consistent with other counties
carroll_adm_all <- carroll_bookings.xlsx %>%
  clean_names() %>%
  mutate(race_label = NA,
         release_type = NA) %>%
  dplyr::select(id = unique_person_id,
                inmate_id,
                yob,
                race_code = race,
                race_label,
                sex,
                housing,
                charge_code,
                charge_desc = charge,
                booking_date = booking_dt_tm,
                booking_type,
                release_date = release_dt_tm,
                release_type,
                sentence_status = sentencing_status) %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y"),
         county = "Carroll") %>%
  distinct()

# dim(carroll_adm_all); length(unique(carroll_adm_all$inmate_id)) # 5402, 1849

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
carroll_adm <- fnc_data_setup(carroll_adm_all)

# Add booking id using id and booking date
carroll_adm <- fnc_booking_id(carroll_adm, "Carroll")

# Calculate los (release date - booking date)
carroll_adm <- fnc_los(carroll_adm)

# Create high utilizer variables
df_hu <- fnc_create_high_utilizer_variables(carroll_adm)
carroll_adm <- left_join(carroll_adm, df_hu, by = c("id", "fy"))

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

    # SENTENCED
    str_detect("SENTENCED", sentence_status)                                         ~ "SENTENCED",
    str_detect("SENTENCED FINES", sentence_status)                                   ~ "SENTENCED",

    # NH STATE PRISONER
    str_detect("STATE PRISONER", sentence_status)                                    ~ "NH STATE PRISONER",

    # OTHER
    str_detect("BAIL SET", sentence_status)                                          ~ "OTHER",
    str_detect("BOND DENIED", sentence_status)                                       ~ "OTHER",
    str_detect("DETAINER", sentence_status)                                          ~ "OTHER",
    str_detect("HELD", sentence_status)                                              ~ "OTHER",
    str_detect("DISMISSED", sentence_status)                                         ~ "OTHER",
    str_detect("SENTENCE SUSPENDED", sentence_status)                                ~ "OTHER",

    TRUE ~ sentence_status)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, sentence_status, sentence_status_standard, release_type, booking_date, everything())

# create pc hold variable
carroll_adm <- carroll_adm %>%
  mutate(pc_hold = ifelse(
    sentence_status_standard == "PROTECTIVE CUSTODY", "PC Hold", "Non-PC Hold"
)) %>% select(-c(los, release_date)) %>% distinct()

# Add sex code labels
carroll_adm <- fnc_sex_labels(carroll_adm)

# Add data labels
carroll_adm <- fnc_add_data_labels(carroll_adm)

# Remove duplicates
carroll_adm <- carroll_adm %>% distinct()

# remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
carroll_adm <- carroll_adm %>%
  filter(booking_date >= "2018-06-30" & booking_date < "2021-07-01")

# create pretrial drug court and sentenced drug court variables - NA, since there is no data on drug courts for carroll
carroll_adm <- carroll_adm %>%
  mutate(drug_court_pretrial  = NA,
         drug_court_sentenced = NA)

# If race or gender are NA in some bookings but present in others, use the recorded race or gender.
# If races or genders are different for the same person, make NA since we don't know which is correct.
carroll_adm <- carroll_adm %>%

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
carroll_adm <- carroll_adm %>%
  mutate(los_max = ifelse(los_max == -Inf, NA, los_max)) %>%
  filter(los_max >= 0 | is.na(los_max))

# Create los categories
carroll_adm <- carroll_adm %>%
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
all_nas <- carroll_adm %>%
  filter(is.na(charge_desc) &
           is.na(booking_type) &
           is.na(release_type) &
           is.na(sentence_status))
carroll_adm1 <- carroll_adm %>% anti_join(all_nas) %>% distinct()

################################################################################

# Charges

################################################################################













################################################################################

# Medicaid data file

################################################################################

# clean names
carroll_medicaid <- carroll_medicaid.xlsx %>%
  clean_names() %>%
  distinct()

# Change date formats for booking and release dataes
carroll_medicaid$booking_dt_tm <- .POSIXct(carroll_medicaid$booking_dt_tm, tz="UTC")
carroll_medicaid$booking_dt_tm <-   format(carroll_medicaid$booking_dt_tm, "%m/%d/%Y")
carroll_medicaid$booking_dt_tm <-  as.Date(carroll_medicaid$booking_dt_tm, format = "%m/%d/%Y")
carroll_medicaid$release_dt_tm <- .POSIXct(carroll_medicaid$release_dt_tm, tz="UTC")
carroll_medicaid$release_dt_tm <-   format(carroll_medicaid$release_dt_tm, "%m/%d/%Y")
carroll_medicaid$release_dt_tm <-  as.Date(carroll_medicaid$release_dt_tm, format = "%m/%d/%Y")

# create a unique booking id per person per booking date
carroll_medicaid$booking_id <- carroll_medicaid %>% group_indices(unique_person_id, booking_dt_tm)
carroll_medicaid <- carroll_medicaid %>%
  rename(booking_date = booking_dt_tm,
         release_date = release_dt_tm,
         county = source_id) %>%
  mutate(booking_id = paste("Carroll", "booking", booking_id, sep = "_")) %>%
  select(unique_person_id, booking_id, everything())
# %>%
#   select(-encrypted_id)

# remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
carroll_medicaid <- carroll_medicaid %>%
  filter(booking_date >= "2018-06-30" & booking_date < "2021-07-01")

# # Does the medicaid file have the same number of unique individuals as the adm? Off by 5
# length(unique(carroll_adm$id)); length(unique(carroll_medicaid$unique_person_id))

################################################################################

# Save files

################################################################################

save(carroll_adm1, file=paste0(sp_data_path, "/Data/r_data/carroll_adm.Rda", sep = ""))
