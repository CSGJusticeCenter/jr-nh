############################################
# Project: JRI New Hampshire
# File: coos.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

###################
# Coos County
###################

coos_adm_all <- coos_adm.xlsx %>%
  clean_names() %>%
  mutate(booking_type = NA,
         release_type = NA,
         race_label   = NA) %>%
  dplyr::select(id = unique_id,
                inmate_id,
                yob,
                race_code = race,
                race_label,
                sex,
                housing = homelessness_indicator,
                charge_code,
                charge_desc = charges,
                booking_date = booking_dt_tm,
                booking_type,
                release_date = release_dt_tm,
                release_type,
                sentence_status) %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y"),
         county = "Coos") %>%
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
coos_adm <- fnc_data_setup(coos_adm_all)

# Add booking id using id and booking date
coos_adm <- fnc_booking_id(coos_adm, "Coos")

# Calculate los (release date - booking date)
coos_adm <- fnc_los(coos_adm)

# Create high utilizer variables
df_hu <- fnc_create_high_utilizer_variables(coos_adm)
coos_adm <- left_join(coos_adm, df_hu, by = c("id", "fy"))

# Create a PC hold variables
coos_adm <- fnc_pc_hold_variables(coos_adm)

# Add sex code labels
coos_adm <- fnc_sex_labels(coos_adm)

# Add data labels
coos_adm <- fnc_add_data_labels(coos_adm)

###################################

# Standardize sentence statuses across counties so they have these categories:
# 1) PROTECTIVE CUSTODY
# 2) PRETRIAL
# 3) SENTENCED
# 4) NH STATE PRISONER
# 4) UNKNOWN
# 5) OTHER

###################################

booking_recordings_coos <- fnc_investigate_booking_recordings(coos_adm)

# Standardize booking info so it's consistent across counties
coos_adm1 <- coos_adm %>% select(-c(los, release_date)) %>% distinct() %>%

  mutate(charge_desc     = as.character(charge_desc),
         booking_type    = as.character(booking_type),
         release_type    = as.character(release_type),
         sentence_status = as.character(sentence_status),
         charge_desc     = toupper(charge_desc),
         booking_type    = toupper(booking_type),
         release_type    = toupper(release_type),
         sentence_status = toupper(sentence_status)) %>%

  mutate(sentence_status_standard = case_when(sentence_status == "PRETRIAL"    ~ "PRETRIAL",
                                              sentence_status == "DUAL STATUS" ~ "OTHER",
                                              sentence_status == "SENTENCED"   ~ "SENTENCED",
                                              is.na(sentence_status)           ~ "UNKNOWN",

                                              TRUE ~ sentence_status)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, sentence_status, sentence_status_standard, release_type, booking_date, everything()) %>%
  distinct()

# create pretrial drug court and sentenced drug court variables - NA, since there is no data on drug courts for coos
coos_adm1 <- coos_adm1 %>%
  mutate(drug_court_pretrial  = NA,
         drug_court_sentenced = NA)

################################################################################

# Charges

################################################################################













################################################################################

# Medicaid data file

################################################################################

# clean names
coos_medicaid <- coos_medicaid.xlsx %>%
  clean_names() %>%
  distinct()

# Change date formats for booking and release dataes
coos_medicaid$booking_dt_tm <- .POSIXct(coos_medicaid$booking_dt_tm, tz="UTC")
coos_medicaid$booking_dt_tm <-   format(coos_medicaid$booking_dt_tm, "%m/%d/%Y")
coos_medicaid$booking_dt_tm <-  as.Date(coos_medicaid$booking_dt_tm, format = "%m/%d/%Y")
coos_medicaid$release_dt_tm <- .POSIXct(coos_medicaid$release_dt_tm, tz="UTC")
coos_medicaid$release_dt_tm <-   format(coos_medicaid$release_dt_tm, "%m/%d/%Y")
coos_medicaid$release_dt_tm <-  as.Date(coos_medicaid$release_dt_tm, format = "%m/%d/%Y")

# create a unique booking id per person per booking date
coos_medicaid$booking_id <- coos_medicaid %>% group_indices(unique_person_id, booking_dt_tm)
coos_medicaid <- coos_medicaid %>%
  rename(booking_date = booking_dt_tm,
         release_date = release_dt_tm,
         county = source_id) %>%
  mutate(booking_id = paste("Coos", "booking", booking_id, sep = "_")) %>%
  select(unique_person_id, booking_id, everything())

################################################################################

# Save files

################################################################################


save(coos_adm1, file=paste0(sp_data_path, "/Data/r_data/data_dictionaries_page/coos_adm.Rda", sep = ""))
