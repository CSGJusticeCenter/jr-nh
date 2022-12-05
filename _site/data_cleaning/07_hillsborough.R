############################################
# Project: JRI New Hampshire
# File: hillsborough.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

###################
# Hillsborough County
###################

hillsborough_adm_all <- hillsborough_adm.xlsx %>%
  clean_names() %>%
  mutate(charge_code = NA,
         race_label = NA) %>%
  dplyr::select(id = x1,
                inmate_id = ccn,
                yob,
                race_code = race,
                race_label,
                sex,
                housing = homeless_y_n,   # switch N to Y? - not using anyway
                charge_code,
                charge_desc = charges,
                booking_date,
                booking_type,
                release_date,
                release_type,
                sentence_status = sentencing_status) %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%y"),
         release_date = as.Date(release_date, format = "%m/%d/%y"),
         county = "Hillsborough") %>%
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
hillsborough_adm <- fnc_data_setup(hillsborough_adm_all)

# Add booking id using id and booking date
hillsborough_adm <- fnc_booking_id(hillsborough_adm, "Hillsborough")

# Calculate los (release date - booking date)
hillsborough_adm <- fnc_los(hillsborough_adm)

# Create high utilizer variables
df_hu <- fnc_create_high_utilizer_variables(hillsborough_adm)
hillsborough_adm <- left_join(hillsborough_adm, df_hu, by = c("id", "fy"))

# Create a PC hold variables
hillsborough_adm <- fnc_pc_hold_variables(hillsborough_adm)

# Add sex code labels
hillsborough_adm <- fnc_sex_labels(hillsborough_adm)

# Add data labels
hillsborough_adm <- fnc_add_data_labels(hillsborough_adm)

# Remove duplicates
hillsborough_adm <- hillsborough_adm %>% distinct()

# remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
hillsborough_adm <- hillsborough_adm %>%
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

booking_recordings_hillsborough <- fnc_investigate_booking_recordings(hillsborough_adm)

# Standardize booking info so it's consistent across counties
hillsborough_adm1 <- hillsborough_adm %>% select(-c(los, release_date)) %>% distinct() %>%

  mutate(charge_desc     = as.character(charge_desc),
         booking_type    = as.character(booking_type),
         release_type    = as.character(release_type),
         sentence_status = as.character(sentence_status),
         charge_desc     = toupper(charge_desc),
         booking_type    = toupper(booking_type),
         release_type    = toupper(release_type),
         sentence_status = toupper(sentence_status)) %>%

  mutate(sentence_status_standard = case_when(# PROTECTIVE CUSTODY
    str_detect("172B:1 XIII - PROTECTIVE CUSTODY 172-B:1 XIII", charge_desc)                    ~ "PROTECTIVE CUSTODY",
    str_detect("TREATMENT AND SERVICES", booking_type) & str_detect("PC RELEASE", release_type) ~ "PROTECTIVE CUSTODY",
    str_detect("NEW ARREST", booking_type) & str_detect("PC RELEASE", release_type)             ~ "PROTECTIVE CUSTODY",

    # PRETRIAL
    sentence_status == "PRETRIAL"                                                               ~ "PRETRIAL",
    sentence_status == "PRE TRIAL DRUG COURT (MANCH)"                                           ~ "PRETRIAL",
    sentence_status == "PRE TRIAL DRUG COURT (NASHUA)"                                          ~ "PRETRIAL",
    sentence_status == "PRE TRIAL ROCK SATCO"                                                   ~ "PRETRIAL",
    sentence_status == "PRETRIAL hillsborough"                                                  ~ "PRETRIAL",
    sentence_status == "PRETRIAL CARROLL"                                                       ~ "PRETRIAL",
    sentence_status == "PRETRIAL COOS"                                                          ~ "PRETRIAL",
    sentence_status == "PRETRIAL DRUG COURT SATCO"                                              ~ "PRETRIAL",
    sentence_status == "PRETRIAL GRAFTON"                                                       ~ "PRETRIAL",
    sentence_status == "PRETRIAL GRAFTON SATCO"                                                 ~ "PRETRIAL",
    sentence_status == "PRETRIAL MERRIMACK"                                                     ~ "PRETRIAL",
    sentence_status == "PRETRIAL ROCK"                                                          ~ "PRETRIAL",
    sentence_status == "PRETRIAL SATCO"                                                         ~ "PRETRIAL",
    sentence_status == "PRETRIAL SULLIVAN"                                                      ~ "PRETRIAL",
    sentence_status == "PRETRIAL SULLIVAN"                                                      ~ "PRETRIAL",
    sentence_status == "PRETRIAL SULLIVAN"                                                      ~ "PRETRIAL",

    # SENTENCED
    sentence_status == "SENTENCED"                                                              ~ "SENTENCED",
    sentence_status == "SENTENCED-HSC NORTH DRUG COURT SANCTION"                                ~ "SENTENCED",
    sentence_status == "SENTENCED-HSC SOUTH DRUG COURT SANCTION"                                ~ "SENTENCED",
    sentence_status == "SENTENCED MERRIMACK"                                                    ~ "SENTENCED",
    sentence_status == "SENTENCED ROCK"                                                         ~ "SENTENCED",
    sentence_status == "SENTENCED ROCK SATCO"                                                   ~ "SENTENCED",
    sentence_status == "SENTENCED SATCO"                                                        ~ "SENTENCED",
    sentence_status == "SENTENCED SULLIVAN"                                                     ~ "SENTENCED",
    sentence_status == "SENTENCED W/HOLD"                                                       ~ "SENTENCED",
    sentence_status == "SENTENCED W/HOLD MERRIMACK"                                             ~ "SENTENCED",
    sentence_status == "SENTENCED W/HOLD ROCK"                                                  ~ "SENTENCED",
    sentence_status == "SENTENCED W/HOLD SATCO"                                                 ~ "SENTENCED",
    sentence_status == "SENTENCED WALK IN"                                                      ~ "SENTENCED",
    sentence_status == "SENTENCED WEEKENDER"                                                    ~ "SENTENCED",
    sentence_status == "SENTENCED WEEKENDER W/HOLD"                                             ~ "SENTENCED",
    sentence_status == "CONVICTED"                                                              ~ "SENTENCED", # ?
    sentence_status == "CONVICTED ROCK"                                                         ~ "SENTENCED", # ?

    # NH STATE PRISONER
    sentence_status == "STATE INMATE"                                                           ~ "NH STATE PRISONER",

    # OTHER
    (sentence_status == "TREATMENT AND SERVICES" & is.na(charge_desc))                          ~ "OTHER",

    # UNKNOWN
    is.na(sentence_status)                                                                      ~ "UNKNOWN",

    TRUE ~ sentence_status)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, sentence_status, sentence_status_standard, release_type, booking_date, everything())

# create pretrial drug court and sentenced drug court variables
hillsborough_adm1 <- hillsborough_adm1 %>%
  mutate(drug_court_pretrial  = ifelse(sentence_status == "PRE TRIAL DRUG COURT (MANCH)" |
                                       sentence_status == "PRE TRIAL DRUG COURT (NASHUA)" |
                                       sentence_status == "PRETRIAL DRUG COURT SATCO" |
                                       (booking_type == "DRUG COURT" & sentence_status == "PRETRIAL")
                                        , 1, 0),
         drug_court_sentenced = ifelse(sentence_status == "SENTENCED-HSC NORTH DRUG COURT SANCTION" |
                                       (booking_type == "DRUG COURT" & sentence_status == "SENTENCED") |
                                       (booking_type == "DRUG COURT" & sentence_status == "SENTENCED WALK IN")
                                       , 1, 0))

################################################################################

# Charges

################################################################################













################################################################################

# Medicaid data file

################################################################################

# clean names
hillsborough_medicaid <- hillsborough_medicaid.xlsx %>%
  clean_names() %>%
  distinct() %>%
  rename(booking_date = bkg_date,
         release_date = rel_date,
         county = source_id)

# create a unique booking id per person per booking date
hillsborough_medicaid$booking_id <- hillsborough_medicaid %>% group_indices(unique_person_id, booking_date)
hillsborough_medicaid <- hillsborough_medicaid %>%
  mutate(booking_id = paste("Hillsborough", "booking", booking_id, sep = "_")) %>%
  select(unique_person_id, booking_id, everything())

# remove bookings before and after study dates
# July 1, 2018, to June 30, 2021
hillsborough_adm <- hillsborough_adm %>%
  filter(booking_date >= "2018-06-30" & booking_date < "2021-07-01")

# # Does the medicaid file have the same number of unique individuals as the adm? Off by 20
# length(unique(hillsborough_adm1$id)); length(unique(hillsborough_medicaid$unique_person_id))

################################################################################

# Save files

################################################################################

save(hillsborough_adm1, file=paste0(sp_data_path, "/Data/r_data/data_dictionaries_page/hillsborough_adm.Rda", sep = ""))
