############################################
# Project: JRI New Hampshire
# File: sullivan.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

###################
# Sullivan County
###################

# clean variable names
sullivan_adm_all <- clean_names(sullivan_adm.xlsx)

# fix date formats
sullivan_adm_all$booking_date_time <- as.POSIXct(sullivan_adm_all$booking_date_time, format = '%m/%d/%Y %H:%M')
sullivan_adm_all$booking_date_time <- format(sullivan_adm_all$booking_date_time, "%m/%d/%Y")
sullivan_adm_all$booking_date_time <- as.Date(sullivan_adm_all$booking_date_time, format = "%m/%d/%Y")
sullivan_adm_all$release_date_time <- as.POSIXct(sullivan_adm_all$release_date_time, format = '%m/%d/%Y %H:%M')
sullivan_adm_all$release_date_time <- format(sullivan_adm_all$release_date_time, "%m/%d/%Y")
sullivan_adm_all$release_date_time <- as.Date(sullivan_adm_all$release_date_time, format = "%m/%d/%Y")

# clean variable names
sullivan_adm_all <- sullivan_adm_all %>%
  mutate(race_label = NA,
         release_type = NA) %>%
  dplyr::select(id = id_number_inmate_number,
                inmate_id = inmate_num,
                yob = year,
                race_code = race,
                race_label,
                sex = gender,
                housing = housing,
                charge_code = charge_id,
                charge_desc = charge,
                booking_date = booking_date_time,
                booking_type = detention_type,
                release_date = release_date_time,
                release_type,
                sentence_status) %>%
  mutate(county = "Sullivan") %>%
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
sullivan_adm <- fnc_data_setup(sullivan_adm_all)

# Add booking id using id and booking date
sullivan_adm <- fnc_booking_id(sullivan_adm, "Sullivan")

# Calculate los (release date - booking date)
sullivan_adm <- fnc_los(sullivan_adm)

# Create high utilizer variables
df_hu <- fnc_create_high_utilizer_variables(sullivan_adm)
sullivan_adm <- left_join(sullivan_adm, df_hu, by = c("id", "fy"))

# Create a PC hold variables
sullivan_adm <- fnc_pc_hold_variables(sullivan_adm)

# Add sex code labels
sullivan_adm <- fnc_sex_labels(sullivan_adm)

# Add data labels
sullivan_adm <- fnc_add_data_labels(sullivan_adm)

# Remove duplicates
sullivan_adm <- sullivan_adm %>% distinct()

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
sullivan_adm1 <- sullivan_adm %>% select(-c(los, release_date)) %>% distinct() %>%

  mutate(charge_desc     = as.character(charge_desc),
         booking_type    = as.character(booking_type),
         release_type    = as.character(release_type),
         sentence_status = as.character(sentence_status),
         charge_desc     = toupper(charge_desc),
         booking_type    = toupper(booking_type),
         release_type    = toupper(release_type),
         sentence_status = toupper(sentence_status)) %>%

  mutate(sentence_status_standard = case_when(# PROTECTIVE CUSTODY
    str_detect("PROTECTIVE CUSTODY", booking_type) ~ "PROTECTIVE CUSTODY",
    str_detect("PROTECTIVE CUSTODY", charge_desc)  ~ "PROTECTIVE CUSTODY",

    # PRETRIAL
    str_detect("PRE-TRIAL", sentence_status)       ~ "PRETRIAL",

    # SENTENCED
    str_detect("SENTENCED", sentence_status)       ~ "SENTENCED",
    str_detect("SENTENCED T 1", sentence_status)   ~ "SENTENCED",
    str_detect("SENTENCED T 5", sentence_status)   ~ "SENTENCED",

    # NH STATE PRISONER

    # OTHER
    str_detect("DUAL", sentence_status)            ~ "OTHER",

    # UNKNOWN
    is.na(sentence_status)                         ~ "UNKNOWN",

    TRUE ~ sentence_status)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, sentence_status, sentence_status_standard, release_type, booking_date, everything())

# create pretrial drug court and sentenced drug court variables - NA, since there is no data on drug courts for sullivan
sullivan_adm1 <- sullivan_adm1 %>%
  mutate(drug_court_pretrial  = NA,
         drug_court_sentenced = NA)

################################################################################

# Charges

################################################################################













################################################################################

# Medicaid data file

################################################################################

# clean names
sullivan_medicaid <- sullivan_medicaid.xlsx %>%
  clean_names() %>%
  distinct()

# create a unique booking id per person per booking date
sullivan_medicaid$booking_id <- sullivan_medicaid %>% group_indices(unique_person_id, booking_date)
sullivan_medicaid <- sullivan_medicaid %>%
  mutate(booking_id = paste("Sullivan", "booking", booking_id, sep = "_")) %>%
  select(unique_person_id, booking_id, everything())

################################################################################

# Save files

################################################################################


save(sullivan_adm1, file=paste0(sp_data_path, "/Data/r_data/data_dictionaries_page/sullivan_adm.Rda", sep = ""))
