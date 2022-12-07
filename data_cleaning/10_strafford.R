############################################
# Project: JRI New Hampshire
# File: strafford.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

###################
# Strafford County
###################

strafford_adm_all <- strafford_adm.xlsx %>%
  clean_names() %>%
  mutate(inmate_id = NA,
         race_label = NA,
         charge_code = NA,
         charge_desc = NA,
         booking_type = NA,
         release_type = NA,
         housing = NA,
         sentence_status = NA) %>%
  dplyr::select(id = id_2,
                inmate_id = id_2,
                yob = year,
                race_code = race,
                race_label,
                sex,
                housing,
                charge_code,
                charge_desc,
                booking_date,
                booking_type,
                release_date,
                release_type,
                sentence_status) %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y"),
         county = "Strafford") %>% distinct()

# remove additional rows in excel
strafford_adm_all <- strafford_adm_all %>% filter(!is.na(id)) %>% droplevels()

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
strafford_adm <- fnc_data_setup(strafford_adm_all)

# Add booking id using id and booking date
strafford_adm <- fnc_booking_id(strafford_adm, "Strafford")

# Calculate los (release date - booking date)
strafford_adm <- fnc_los(strafford_adm)

# Create high utilizer variables
df_hu <- fnc_create_high_utilizer_variables(strafford_adm)
strafford_adm <- left_join(strafford_adm, df_hu, by = c("id", "fy"))

# Create a PC hold variables
strafford_adm <- fnc_pc_hold_variables(strafford_adm)

# Add sex code labels
strafford_adm <- fnc_sex_labels(strafford_adm)

# Add data labels
strafford_adm <- fnc_add_data_labels(strafford_adm)

# Remove duplicates
strafford_adm <- strafford_adm %>% distinct()

# remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
strafford_adm <- strafford_adm %>%
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

# Standardize booking info so it's consistent across counties
strafford_adm1 <- strafford_adm %>% select(-c(los, release_date)) %>% distinct() %>%

  mutate(sentence_status_standard = "UNKNOWN") %>%
  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, sentence_status, sentence_status_standard, release_type, booking_date, everything())

# create pretrial drug court and sentenced drug court variables - NA, since there is no data on drug courts for strafford
strafford_adm1 <- strafford_adm1 %>%
  mutate(drug_court_pretrial  = NA,
         drug_court_sentenced = NA)

################################################################################

# Charges

################################################################################













################################################################################

# Medicaid data file

################################################################################

# clean names
strafford_medicaid <- strafford_medicaid.xlsx %>%
  clean_names() %>%
  distinct() %>%
  rename(county = source_id)

# create a unique booking id per person per booking date
strafford_medicaid$booking_id <- strafford_medicaid %>% group_indices(unique_person_id, booking_date)
strafford_medicaid <- strafford_medicaid %>%
  mutate(booking_id = paste("Strafford", "booking", booking_id, sep = "_")) %>%
  select(unique_person_id, booking_id, everything())

# remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
strafford_adm <- strafford_adm %>%
  filter(booking_date >= "2018-06-30" & booking_date < "2021-07-01")

# # Does the medicaid file have the same number of unique individuals as the adm? Off by 88
# length(unique(strafford_adm1$id)); length(unique(strafford_medicaid$unique_person_id))

################################################################################

# Save files

################################################################################


save(strafford_adm1, file=paste0(sp_data_path, "/Data/r_data/data_dictionaries_page/strafford_adm.Rda", sep = ""))
