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
         charge_code = NA,
         charge_desc = NA,
         booking_type = NA,
         release_type = NA,
         housing = NA,
         sentence_status = NA,
         race_label = case_when(
           race == "A" ~ "Asian/Pacific Islander",
           race == "B" ~ "Black",
           race == "H" ~ "Hispanic",
           race == "I" ~ "American Indian/Alaskan Native",
           race == "O" ~ "Unknown",
           race == "U" ~ "Unknown",
           race == "W" ~ "White"
         )) %>%
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
strafford_adm <- strafford_adm %>% select(-c(los, release_date)) %>% distinct() %>%

  mutate(sentence_status_standard = "UNKNOWN") %>%
  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, sentence_status, sentence_status_standard, release_type, booking_date, everything())

# create pc hold variable
strafford_adm <- strafford_adm %>%
  mutate(pc_hold = NA)

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

# create pretrial drug court and sentenced drug court variables - NA, since there is no data on drug courts for strafford
strafford_adm <- strafford_adm %>%
  mutate(drug_court_pretrial  = NA,
         drug_court_sentenced = NA)

# If race or gender are NA in some bookings but present in others, use the recorded race or gender.
# If races or genders are different for the same person, make NA since we don't know which is correct.
strafford_adm <- strafford_adm %>%

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
strafford_adm <- strafford_adm %>%
  mutate(los_max = ifelse(los_max == -Inf, NA, los_max)) %>%
  filter(los_max >= 0 | is.na(los_max))

# Create los categories
strafford_adm1 <- strafford_adm %>%
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

################################################################################

# Charges

################################################################################













################################################################################

# Medicaid data file

################################################################################

# clean names
# create race labels
strafford_medicaid <- strafford_medicaid.xlsx %>%
  clean_names() %>%
  distinct() %>%
  rename(county = source_id) %>%
  mutate(
    jail_race = case_when(
      jail_race == "A" ~ "Asian/Pacific Islander",
      jail_race == "B" ~ "Black",
      jail_race == "H" ~ "Hispanic",
      jail_race == "I" ~ "American Indian/Alaskan Native",
      jail_race == "O" ~ "Unknown",
      jail_race == "U" ~ "Unknown",
      jail_race == "W" ~ "White"
    ))

# create a unique booking id per person per booking date
strafford_medicaid$booking_id <- strafford_medicaid %>% group_indices(unique_person_id, booking_date)
strafford_medicaid <- strafford_medicaid %>%
  mutate(booking_id = paste("Strafford", "booking", booking_id, sep = "_")) %>%
  select(unique_person_id, booking_id, everything())

# remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
strafford_medicaid <- strafford_medicaid %>%
  filter(booking_date >= "2018-06-30" & booking_date < "2021-07-01")

# # Does the medicaid file have the same number of unique individuals as the adm? Off by 88
# length(unique(strafford_adm$id)); length(unique(strafford_medicaid$unique_person_id))

################################################################################

# Save files

################################################################################

save(strafford_adm1, file=paste0(sp_data_path, "/Data/r_data/strafford_adm.Rda", sep = ""))
