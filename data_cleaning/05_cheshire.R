############################################
# Project: JRI New Hampshire
# File: cheshire.R
# Last updated: December 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

###################
# Cheshire County
###################

cheshire_adm_all <- cheshire_adm.xlsx %>%
  clean_names() %>%
  mutate(race_label = NA) %>%
  dplyr::select(id,
                inmate_id,
                yob,
                race_code = race,
                race_label,
                sex,
                housing = housing_instability_or_homelessness_indicator,
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
cheshire_adm <- fnc_data_setup(cheshire_adm_all)

# Add booking id using id and booking date
cheshire_adm <- fnc_booking_id(cheshire_adm, "Cheshire")

# Calculate los (release date - booking date)
cheshire_adm <- fnc_los(cheshire_adm)

# Create high utilizer variables
df_hu <- fnc_create_high_utilizer_variables(cheshire_adm)
cheshire_adm <- left_join(cheshire_adm, df_hu, by = c("id", "fy"))

# Create a PC hold variables
cheshire_adm <- fnc_pc_hold_variables(cheshire_adm)

# Add sex code labels
cheshire_adm <- fnc_sex_labels(cheshire_adm)

# Add data labels
cheshire_adm <- fnc_add_data_labels(cheshire_adm)

# Remove duplicates
cheshire_adm <- cheshire_adm %>% distinct()

# remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
cheshire_adm <- cheshire_adm %>%
  filter(booking_date >= "2018-06-30" & booking_date < "2021-07-01")

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
cheshire_adm1 <- cheshire_adm %>%
  select(-c(los, release_date)) %>%
  distinct() %>%

  mutate(charge_desc     = as.character(charge_desc),
         booking_type    = as.character(booking_type),
         release_type    = as.character(release_type),
         sentence_status = as.character(sentence_status),
         charge_desc     = toupper(charge_desc),
         booking_type    = toupper(booking_type),
         release_type    = toupper(release_type),
         sentence_status = toupper(sentence_status)) %>%

  mutate(pc_hold         = as.character(pc_hold)) %>%
  mutate(sentence_status = as.character(sentence_status)) %>%
  mutate(booking_type    = as.character(booking_type)) %>%

  mutate(pc_hold         = case_when(charge_desc == "TEMPORARY REMOVAL OR TRANSFER" & sentence_status == "PROTECTIVE CUSTODY" ~ "Non-PC Hold", TRUE ~ pc_hold)) %>%
  mutate(sentence_status = case_when(charge_desc == "TEMPORARY REMOVAL OR TRANSFER" & sentence_status == "PROTECTIVE CUSTODY" ~ "UNKNOWN", TRUE ~ sentence_status)) %>%

  mutate(sentence_status_standard = case_when(# PROTECTIVE CUSTODY
    str_detect("PROTECTIVE CUSTODY|PROTECTIVE CUSTODY - DRUGS", charge_desc)         ~ "PROTECTIVE CUSTODY",
    str_detect("PROTECTIVE CUSTODY", sentence_status)                                ~ "PROTECTIVE CUSTODY",

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
    str_detect("FEDERAL INMATE", sentence_status)                                    ~ "NH STATE PRISONER",
    str_detect("HOLD FOR STATE PRISON", sentence_status)                             ~ "NH STATE PRISONER",
    (is.na(sentence_status) & str_detect("FEDERAL HOLD", booking_type))              ~ "NH STATE PRISONER",

    # OTHER
    str_detect("DUAL STATUS", sentence_status)                                       ~ "OTHER",
    (is.na(sentence_status) & str_detect("FELONY FIRST", booking_type))               ~ "OTHER",
    str_detect("HOLD FOR OTHER AGENCY", sentence_status)                             ~ "OTHER",
    str_detect("DETAINEE REQUEST", sentence_status)                                  ~ "OTHER",

    # UNKNOWN
    # no data in sentence status but info in booking type
    is.na(sentence_status)                                                           ~ "UNKNOWN",
    sentence_status == "UNKNOWN"                                                     ~ "UNKNOWN",

    TRUE ~ sentence_status)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, sentence_status, sentence_status_standard, release_type, booking_date, everything())

# create pretrial drug court and sentenced drug court variables
cheshire_adm1 <- cheshire_adm1 %>%
  mutate(drug_court_pretrial  = ifelse(sentence_status == "PRE-TRIAL / DRUG COURT", 1, 0),
         drug_court_sentenced = ifelse(sentence_status == "SENTENCED / DRUG COURT", 1, 0))

################################################################################

# Charges

################################################################################













################################################################################

# Medicaid data file

################################################################################

# clean names
cheshire_medicaid <- cheshire_medicaid.xlsx %>%
  clean_names() %>%
  distinct() %>%
  rename(booking_date = admission_date_s,
         release_date = release_date_s,
         county = source_id) %>%
  mutate(booking_date = format(booking_date, format = "%m/%d/%Y"),
         release_date = format(release_date, format = "%m/%d/%Y")) %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y"))

# create a unique booking id per person per booking date
cheshire_medicaid$booking_id <- cheshire_medicaid %>% group_indices(unique_person_id, booking_date)
cheshire_medicaid <- cheshire_medicaid %>%
  mutate(booking_id = paste("Cheshire", "booking", booking_id, sep = "_")) %>%
  select(unique_person_id, booking_id, everything())

# remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
cheshire_adm <- cheshire_adm %>%
  filter(booking_date >= "2018-06-30" & booking_date < "2021-07-01")

# # Does the medicaid file have the same number of unique individuals as the adm? Off by 116
# length(unique(cheshire_adm1$id)); length(unique(cheshire_medicaid$unique_person_id))

################################################################################

# Save files

################################################################################

save(cheshire_adm1, file=paste0(sp_data_path, "/Data/r_data/data_dictionaries_page/cheshire_adm.Rda", sep = ""))
