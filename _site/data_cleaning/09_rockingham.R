############################################
# Project: JRI New Hampshire
# File: rockingham.R
# Last updated: June 1, 2022
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
         race_label = NA,
         release_type = NA) %>%
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

# Create a PC hold variables
rockingham_adm <- fnc_pc_hold_variables(rockingham_adm)

# Add sex code labels
rockingham_adm <- fnc_sex_labels(rockingham_adm)

# Add data labels
rockingham_adm <- fnc_add_data_labels(rockingham_adm)

# Remove duplicates
rockingham_adm <- rockingham_adm %>% distinct()

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
rockingham_adm1 <- rockingham_adm %>% select(-c(los, release_date)) %>% distinct() %>%

  mutate(charge_desc     = as.character(charge_desc),
         booking_type    = as.character(booking_type),
         release_type    = as.character(release_type),
         sentence_status = as.character(sentence_status),
         charge_desc     = toupper(charge_desc),
         booking_type    = toupper(booking_type),
         release_type    = toupper(release_type),
         sentence_status = toupper(sentence_status)) %>%

  mutate(sentence_status_standard = case_when(# PROTECTIVE CUSTODY
    str_detect("PROTECTIVE CUSTODY", charge_desc) ~ "PROTECTIVE CUSTODY",

    # PRETRIAL
    str_detect("DIVERSION", sentence_status)      ~ "OTHER",
    str_detect("DIVERSION", sentence_status)      ~ "OTHER",

    # SENTENCED
    str_detect("SENTENCED", sentence_status)      ~ "SENTENCED",

    # NH STATE PRISONER

    # OTHER
    str_detect("ADMIN TRANSFER", sentence_status) ~ "OTHER",
    str_detect("DIVERSION", sentence_status)      ~ "OTHER",
    str_detect("DUAL", sentence_status)           ~ "OTHER",
    str_detect("IAD INMATE", sentence_status)     ~ "OTHER",

    # UNKNOWN
    is.na(sentence_status)                        ~ "UNKNOWN",

    TRUE ~ sentence_status)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, sentence_status, sentence_status_standard, release_type, booking_date, everything())

# create pretrial drug court and sentenced drug court variables - NA, since there is no data on drug courts for rockingham
rockingham_adm1 <- rockingham_adm1 %>%
  mutate(drug_court_pretrial  = ifelse(booking_type == "DRUG COURT SENTENCING ORDER" & sentence_status == "PRETRIAL", 1, 0),
         drug_court_sentenced = ifelse(booking_type == "DRUG COURT SENTENCING ORDER" & sentence_status == "SENTENCED", 1, 0))

################################################################################

# Charges

################################################################################













################################################################################

# Medicaid data file

################################################################################

# clean names
rockingham_medicaid <- rockingham_medicaid.xlsx %>%
  clean_names() %>%
  distinct() %>%
  rename(booking_date = arrival_date_and_time,
         release_date = release_date_and_time,
         county = source_id) %>%
  mutate(booking_date = format(booking_date, format = "%m/%d/%Y"),
         release_date = format(release_date, format = "%m/%d/%Y")) %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y"))

# create a unique booking id per person per booking date
rockingham_medicaid$booking_id <- rockingham_medicaid %>% group_indices(unique_person_id, booking_date)
rockingham_medicaid <- rockingham_medicaid %>%
  mutate(booking_id = paste("Rockingham", "booking", booking_id, sep = "_")) %>%
  select(unique_person_id, booking_id, everything())

################################################################################

# Save files

################################################################################


save(rockingham_adm1, file=paste0(sp_data_path, "/Data/r_data/data_dictionaries_page/rockingham_adm.Rda", sep = ""))
