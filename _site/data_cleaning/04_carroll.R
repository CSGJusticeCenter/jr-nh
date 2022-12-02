############################################
# Project: JRI New Hampshire
# File: carroll.R
# Last updated: December 1, 2022
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

# Create a PC hold variables
carroll_adm <- fnc_pc_hold_variables(carroll_adm)

# Add sex code labels
carroll_adm <- fnc_sex_labels(carroll_adm)

# Add data labels
carroll_adm <- fnc_add_data_labels(carroll_adm)

# Remove duplicates
carroll_adm <- carroll_adm %>% distinct()

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
carroll_adm1 <- carroll_adm %>% select(-c(los, release_date)) %>% distinct() %>%

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
    str_detect("PROTECTIVE CUSTODY", charge_desc)                                                                  ~ "PROTECTIVE CUSTODY",
    str_detect("DETAINEE REQUEST", booking_type)               & str_detect("PROTECTIVE CUSTODY", sentence_status) ~ "PROTECTIVE CUSTODY",
    str_detect("INVOLUNTARY EMERGENCY ADMISSION", charge_desc) & str_detect("PROTECTIVE CUSTODY", sentence_status) ~ "PROTECTIVE CUSTODY",
    str_detect("DOMESTIC VIOLENCE OFFENSE", charge_desc)       & str_detect("PROTECTIVE CUSTODY", sentence_status) ~ "PROTECTIVE CUSTODY",

    # PRETRIAL
    str_detect("PRE-TRIAL", sentence_status)                                                                       ~ "PRETRIAL",
    str_detect("AWAITING TRIAL", sentence_status)                                                                  ~ "PRETRIAL",

    # SENTENCED
    str_detect("SENTENCED", sentence_status)                                                                       ~ "SENTENCED",
    str_detect("SENTENCED FINES", sentence_status)                                                                 ~ "SENTENCED",

    # NH STATE PRISONER
    str_detect("STATE PRISONER", sentence_status)                                                                  ~ "NH STATE PRISONER",

    # OTHER
    str_detect("BAIL SET", sentence_status)                                                                        ~ "OTHER",
    str_detect("BOND DENIED", sentence_status)                                                                     ~ "OTHER",
    str_detect("DETAINER", sentence_status)                                                                        ~ "OTHER",
    str_detect("HELD", sentence_status)                                                                            ~ "OTHER",
    str_detect("DISMISSED", sentence_status)                                                                       ~ "OTHER",
    str_detect("SENTENCE SUSPENDED", sentence_status)                                                              ~ "OTHER",

    TRUE ~ sentence_status)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, sentence_status, sentence_status_standard, release_type, booking_date, everything())

# create pretrial drug court and sentenced drug court variables - NA, since there is no data on drug courts for carroll
carroll_adm1 <- carroll_adm1 %>%
  mutate(drug_court_pretrial  = NA,
         drug_court_sentenced = NA)

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

################################################################################

# Save files

################################################################################


save(carroll_adm1, file=paste0(sp_data_path, "/Data/r_data/data_dictionaries_page/carroll_adm.Rda", sep = ""))
