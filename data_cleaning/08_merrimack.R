############################################
# Project: JRI New Hampshire
# File: merrimack.R
# Last updated: December 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

###################
# Merrimack County
###################

merrimack_adm_all <- merrimack_adm.xlsx %>%
  clean_names() %>%
  mutate(charge_code = NA,
         race_label = NA,
         release_type = NA) %>%
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
merrimack_adm <- fnc_data_setup(merrimack_adm_all)

# Add booking id using id and booking date
merrimack_adm <- fnc_booking_id(merrimack_adm, "Merrimack")

# Calculate los (release date - booking date)
merrimack_adm <- fnc_los(merrimack_adm)

# Create high utilizer variables
df_hu <- fnc_create_high_utilizer_variables(merrimack_adm)
merrimack_adm <- left_join(merrimack_adm, df_hu, by = c("id", "fy"))

# Create a PC hold variables
merrimack_adm <- fnc_pc_hold_variables(merrimack_adm)

# Add sex code labels
merrimack_adm <- fnc_sex_labels(merrimack_adm)

# Add data labels
merrimack_adm <- fnc_add_data_labels(merrimack_adm)

# Remove duplicates
merrimack_adm <- merrimack_adm %>% distinct()

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
merrimack_adm1 <- merrimack_adm %>% select(-c(los, release_date)) %>% distinct() %>%

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
         booking_type== "PROTECTIVE CUSTODY" & sentence_status == "24 HOUR HOLD"           ~ "PROTECTIVE CUSTODY",
         charge_desc == "PROTECTIVE CUSTODY"                                               ~ "PROTECTIVE CUSTODY",
         sentence_status == "PROTECTIVE CUSTODY"                                           ~ "PROTECTIVE CUSTODY",
         sentence_status == "PC-IEA"                                                       ~ "PROTECTIVE CUSTODY",
         booking_type == "DETAINEE REQUEST" & sentence_status == "PROTECTIVE CUSTODY HOLD" ~ "PROTECTIVE CUSTODY",
         booking_type == "ARREST WARRANT" & sentence_status == "PROTECTIVE CUSTODY HOLD"   ~ "PROTECTIVE CUSTODY",
         is.na(booking_type) & sentence_status == "PROTECTIVE CUSTODY HOLD"                ~ "PROTECTIVE CUSTODY",

         # PRETRIAL
         sentence_status == "PRE-TRIAL FELONY"                                             ~ "PRETRIAL",
         sentence_status == "PRE-TRIAL MISDEMEANOR"                                        ~ "PRETRIAL",

         # SENTENCED
         sentence_status == "SENTENCED FELONY"                                             ~ "SENTENCED",
         sentence_status == "SENTENCED MISDEMEANOR"                                        ~ "SENTENCED",

         # NH STATE PRISONER

         # OTHER
         (sentence_status == "24 HOUR HOLD" & booking_type != "PROTECTIVE CUSTODY")        ~ "OTHER",
         (sentence_status == "24 HOUR HOLD" & is.na(booking_type))                         ~ "OTHER",
         sentence_status == "72 HOUR HOLD"                                                 ~ "OTHER",
         sentence_status == "DUAL STATUS"                                                  ~ "OTHER",
         sentence_status == "PAROLE VIOLATION"                                             ~ "OTHER",

         # UNKNOWN
         is.na(sentence_status)                                                            ~ "UNKNOWN",

         TRUE ~ sentence_status)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, sentence_status, sentence_status_standard, release_type, booking_date, everything())

# create pretrial drug court and sentenced drug court variables - NA, since there is no data on drug courts for merrimack
# there is info on drug court violations in the charge descriptions though
merrimack_adm1 <- merrimack_adm1 %>%
  mutate(drug_court_pretrial  = NA,
         drug_court_sentenced = NA)

################################################################################

# Charges

################################################################################













################################################################################

# Medicaid data file

################################################################################

# clean names
merrimack_medicaid <- merrimack_medicaid.xlsx %>%
  clean_names() %>%
  distinct() %>%
  rename(booking_date = book_date,
         release_date = rel_date,
         county = source_id) %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y"))

# create a unique booking id per person per booking date
merrimack_medicaid$booking_id <- merrimack_medicaid %>% group_indices(unique_person_id, booking_date)
merrimack_medicaid <- merrimack_medicaid %>%
  mutate(booking_id = paste("Merrimack", "booking", booking_id, sep = "_")) %>%
  select(unique_person_id, booking_id, everything())

################################################################################

# Save files

################################################################################


save(merrimack_adm1, file=paste0(sp_data_path, "/Data/r_data/data_dictionaries_page/merrimack_adm.Rda", sep = ""))
