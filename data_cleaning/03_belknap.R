############################################
# Project: JRI New Hampshire
# File: belknap.R
# Last updated: December 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

################################################################################

# Administrative data file

################################################################################

belknap_adm_all <- belknap_adm.xlsx %>%
  clean_names() %>%
  mutate(charge_code = NA,
         race_label = NA) %>%
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
belknap_adm <- fnc_data_setup(belknap_adm_all)

# Add booking id using id and booking date
belknap_adm <- fnc_booking_id(belknap_adm, "Belknap")

# Calculate los (release date - booking date)
belknap_adm <- fnc_los(belknap_adm)

# Create high utilizer variables
df_hu <- fnc_create_high_utilizer_variables(belknap_adm)
belknap_adm <- left_join(belknap_adm, df_hu, by = c("id", "fy"))

# Create a PC hold variables
belknap_adm <- fnc_pc_hold_variables(belknap_adm)

# Add sex code labels
belknap_adm <- fnc_sex_labels(belknap_adm)

# Add data labels
belknap_adm <- fnc_add_data_labels(belknap_adm)

# Remove duplicates
belknap_adm <- belknap_adm %>% distinct()

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

belknap_adm1 <- belknap_adm %>% select(-c(los, release_date)) %>% distinct() %>%

  mutate(pc_hold = as.character(pc_hold),
         charge_desc     = as.character(charge_desc),
         booking_type    = as.character(booking_type),
         release_type    = as.character(release_type),
         sentence_status = as.character(sentence_status),
         charge_desc     = toupper(charge_desc),
         booking_type    = toupper(booking_type),
         release_type    = toupper(release_type),
         sentence_status = toupper(sentence_status)) %>%

  mutate(pc_hold = case_when((charge_desc == "TEMPORARY REMOVAL OR TRANSFER" |
                                charge_desc == "RESIST ARREST OR DETENTION 642:2" |
                                charge_desc == "DISORDERLY CONDUCT 644:2" |
                                charge_desc == "DRIVING OR OPERATING UNDER THE INFLUENCE OF DRUGSOR LIQUOR 265-A:2" |
                                charge_desc == "RESISTING ARREST 594:5"|
                                charge_desc == "SIMPLE ASSAULT 631:2-A" |
                                charge_desc == "VIOLATION OF PROTECTIVE ORDER")
                             & booking_type == "PROTECTIVE CUSTODY"              ~ "Non-PC Hold",
                             TRUE                                                ~ pc_hold)) %>%

  mutate(sentence_status_standard = case_when(
    # PROTECTIVE CUSTODY
    str_detect("PROTECTIVE CUSTODY|PROTECTIVE CUSTODY/INTOXICATION", charge_desc)       ~ "PROTECTIVE CUSTODY",

    # PRETRIAL
    str_detect("PRETRIAL", booking_type)                                                ~ "PRETRIAL",

    # SENTENCED
    str_detect("SENTENCED", booking_type)                                               ~ "SENTENCED",

    # NH STATE PRISONER
    str_detect("NH STATE PRISONER", booking_type)                                       ~ "NH STATE PRISONER",

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

# create pretrial drug court and sentenced drug court variables - NA, since there is no data on drug courts for Belknap
belknap_adm1 <- belknap_adm1 %>%
  mutate(drug_court_pretrial  = NA,
         drug_court_sentenced = NA)

################################################################################

# Charges

################################################################################













################################################################################

# Medicaid data file

################################################################################

# clean names
belknap_medicaid <- belknap_medicaid.xlsx %>%
  clean_names() %>%
  distinct() %>%
  rename(county = source_id)

# create a unique booking id per person per booking date
belknap_medicaid$booking_id <- belknap_medicaid %>% group_indices(unique_person_id, booking_date)
belknap_medicaid <- belknap_medicaid %>%
  mutate(booking_id = paste("Belknap", "booking", booking_id, sep = "_")) %>%
  select(unique_person_id, booking_id, everything())

################################################################################

# Save files

################################################################################


save(belknap_adm1, file=paste0(sp_data_path, "/Data/r_data/data_dictionaries_page/belknap_adm.Rda", sep = ""))
