############################################
# Project: JRI New Hampshire
# File: dataframes.R
# Last updated: October 25, 2022
# Author: Mari Roberts

# Generate tables for graphs and visualizations

# Creates data frames:

# adm_all            - this is broken down into the tables below
# bookings_entrances - used the most, admissions events (includes Coos and Strafford)
# booking_no_pc_hold - bookings without pc holds (no Strafford)
# entrances          - entrances, including pc holds (no Coos)

# charges            - charges
# release_types      - release types
# sentence_statuses  - sentence statuses
############################################

################################################################################

# Standardize data

################################################################################

# Load R files that import and standardize each jail file so the variables are the same.
source("data_cleaning/00_library.R")
source("data_cleaning/01_functions_visuals.R")
source("data_cleaning/01_functions.R")
source("data_cleaning/02_import.R")
source("data_cleaning/03_belknap.R")
source("data_cleaning/04_carroll.R")
source("data_cleaning/05_cheshire.R")
source("data_cleaning/06_coos.R")
source("data_cleaning/07_hillsborough.R")
source("data_cleaning/08_merrimack.R")
source("data_cleaning/09_rockingham.R")
source("data_cleaning/10_strafford.R")
source("data_cleaning/11_sullivan.R")

# Custom functions below creates the variables we need and relabels codes so they're consistent across counties.
# Creates booking_id, los, fy, num_entrances, high_utilizer_1_pct(y/n), high_utilizer_5_pct(y/n), high_utilizer_10_pct(y/n),
#    pc_hold_booking(y/n), pc_hold_charge(y/n), pc_hold_sentence(y/n), pc_hold_release(y/n),
#    pc_hold(y/n) which is the overall pc hold variable (if pc hold was indicated in other pc variables).
# Ignore warning messages.

# Note about LOS: some people can be booked on the same day for multiple charges.
# For example, someone could enter jail on a protective custody hold on 10/19 with a release
#   date of 10/20 but also be booked for a criminal charge on 10/19 with a release date of 10/26
#   For this reason, find the maximum release date for each booking id (created using id and booking_date).

belknap_adm      <- fnc_standardize_counties(belknap_adm_all,      "Belknap")
carroll_adm      <- fnc_standardize_counties(carroll_adm_all,      "Carroll")
cheshire_adm     <- fnc_standardize_counties(cheshire_adm_all,     "Cheshire")
coos_adm         <- fnc_standardize_counties(coos_adm_all,         "Coos")
hillsborough_adm <- fnc_standardize_counties(hillsborough_adm_all, "Hillsborough")
merrimack_adm    <- fnc_standardize_counties(merrimack_adm_all,    "Merrimack")
rockingham_adm   <- fnc_standardize_counties(rockingham_adm_all,   "Rockingham")
strafford_adm    <- fnc_standardize_counties(strafford_adm_all,    "Strafford")
sullivan_adm     <- fnc_standardize_counties(sullivan_adm_all,     "Sullivan")

################################################################################

# Remove LOS (keep los max) and release date due to release date differences by booking id.

# Standardize booking types across counties so they have these categories:
     # 1) PROTECTIVE CUSTODY
     # 2) PRETRIAL
     # 3) SENTENCED/SENTENCING
     # 4) UNKNOWN
     # 5) OTHER

# Felony drug court programs for adult offenders are available in
     # Belknap, Carroll, Cheshire, Coos, Grafton, Hillsborough, Merrimack, Rockingham, and Strafford

################################################################################

##########
# Belknap - meeting TBD
##########

booking_recordings_belknap <- fnc_investigate_booking_recordings(belknap_adm)
# Booking types:
    # ADMIN TRANSFER
    # DUAL
    # NH STATE PRISONER
    # OVERNIGHT HOLD
    # PRETRIAL
    # PROTECTIVE CUSTODY
    # SENTENCED

# If charge is present then it was a mistake to book them as a PC hold.
# Change to non-PC hold and the booking type (create new variable to preserve raw data) to unknown these since they aren't PC holds. Keep charge info though.
# Standardize booking info so it's consistent across counties

belknap_adm1 <- belknap_adm %>% select(-c(los, release_date)) %>% distinct() %>%

  mutate(pc_hold = as.character(pc_hold)) %>%
  mutate(pc_hold = case_when((charge_desc == "TEMPORARY REMOVAL OR TRANSFER" |
                              charge_desc == "RESIST ARREST OR DETENTION 642:2" |
                              charge_desc == "DISORDERLY CONDUCT 644:2" |
                              charge_desc == "DRIVING OR OPERATING UNDER THE INFLUENCE OF DRUGSOR LIQUOR 265-A:2" |
                              charge_desc == "RESISTING ARREST 594:5"|
                              charge_desc == "SIMPLE ASSAULT 631:2-A" |
                              charge_desc == "VIOLATION OF PROTECTIVE ORDER")
                              & booking_type == "PROTECTIVE CUSTODY"              ~ "Non-PC Hold",
                              TRUE                                                ~ pc_hold)) %>%

  mutate(charge_desc     = as.character(charge_desc),
         booking_type    = as.character(booking_type),
         release_type    = as.character(release_type),
         sentence_status = as.character(sentence_status),
         charge_desc     = toupper(charge_desc),
         booking_type    = toupper(booking_type),
         release_type    = toupper(release_type),
         sentence_status = toupper(sentence_status)) %>%

                                            # UNKNOWN, not PC holds
  mutate(booking_type_standard = case_when((charge_desc == "TEMPORARY REMOVAL OR TRANSFER" |
                                            charge_desc == "RESIST ARREST OR DETENTION 642:2" |
                                            charge_desc == "DISORDERLY CONDUCT 644:2" |
                                            charge_desc == "DRIVING OR OPERATING UNDER THE INFLUENCE OF DRUGSOR LIQUOR 265-A:2" |
                                            charge_desc == "RESISTING ARREST 594:5"|
                                            charge_desc == "SIMPLE ASSAULT 631:2-A" |
                                            charge_desc == "VIOLATION OF PROTECTIVE ORDER")
                                           & booking_type == "PROTECTIVE CUSTODY"                                              ~ "UNKNOWN",

                                           # PROTECTIVE CUSTODY
                                           str_detect("PROTECTIVE CUSTODY|PROTECTIVE CUSTODY/INTOXICATION", charge_desc)       ~ "PROTECTIVE CUSTODY",

                                           # ADMINISTRATIVE TRANSFER
                                           str_detect("ADMIN TRANSFER", booking_type)                                          ~ "TRANSFER",

                                           # OTHER
                                           str_detect("OVERNIGHT HOLD", booking_type)                                          ~ "DETAIN/HOLD/WARRANT",

                                           # DUAL - normally a sentence status
                                           str_detect("DUAL", booking_type)                                                    ~ "UNKNOWN",

                                           # NH STATE PRISONER - normally a sentence status
                                           str_detect("NH STATE PRISONER", booking_type)                                       ~ "UNKNOWN",

                                           # SENTENCED - normally a sentence status
                                           str_detect("SENTENCED", booking_type)                                               ~ "UNKNOWN",

                                           # PRETRIAL - normally a sentence status
                                           str_detect("PRETRIAL", booking_type)                                                ~ "UNKNOWN",


                                           TRUE ~ booking_type)) %>%

                                               # UNKNOWN
  mutate(sentence_status_standard = case_when((charge_desc == "TEMPORARY REMOVAL OR TRANSFER" |
                                                 charge_desc == "RESIST ARREST OR DETENTION 642:2" |
                                                 charge_desc == "DISORDERLY CONDUCT 644:2" |
                                                 charge_desc == "DRIVING OR OPERATING UNDER THE INFLUENCE OF DRUGSOR LIQUOR 265-A:2" |
                                                 charge_desc == "RESISTING ARREST 594:5"|
                                                 charge_desc == "SIMPLE ASSAULT 631:2-A" |
                                                 charge_desc == "VIOLATION OF PROTECTIVE ORDER")
                                              & booking_type == "PROTECTIVE CUSTODY"                                              ~ "UNKNOWN",

                                              # PROTECTIVE CUSTODY
                                              str_detect("PROTECTIVE CUSTODY|PROTECTIVE CUSTODY/INTOXICATION", charge_desc)       ~ "PROTECTIVE CUSTODY",

                                              # ADMINISTRATIVE TRANSFER - normally a booking type
                                              str_detect("ADMIN TRANSFER", booking_type)                                          ~ "UNKNOWN",

                                              # HOLD - normally a booking type
                                              str_detect("OVERNIGHT HOLD", booking_type)                                          ~ "UNKNOWN",

                                              # DUAL
                                              str_detect("DUAL", booking_type)                                                    ~ "DUAL",

                                              # NH STATE PRISONER
                                              str_detect("NH STATE PRISONER", booking_type)                                       ~ "NH STATE PRISONER",

                                              # SENTENCED
                                              str_detect("SENTENCED", booking_type)                                               ~ "SENTENCED",

                                              # PRETRIAL
                                              str_detect("PRETRIAL", booking_type)                                                ~ "PRETRIAL",


                                              TRUE ~ booking_type)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, booking_type_standard, sentence_status, sentence_status_standard, release_type, booking_date, everything())

temp <- fnc_investigate_booking_recordings_standard(belknap_adm1)
table(belknap_adm1$booking_type_standard)
table(belknap_adm1$sentence_status_standard)

##########
# Carroll
##########

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

  mutate(sentence_status_standard = case_when(# PRETRIAL
                                           str_detect("PRE-TRIAL", sentence_status)                                                                       ~ "PRETRIAL",
                                           str_detect("AWAITING TRIAL", sentence_status)                                                                  ~ "PRETRIAL",

                                           str_detect("BAIL SET", sentence_status)                                                                        ~ "DETAINED/HELD",  # ?
                                           str_detect("BOND DENIED", sentence_status)                                                                     ~ "DETAINED/HELD",  # ?
                                           str_detect("DETAINER", sentence_status)                                                                        ~ "DETAINED/HELD",
                                           str_detect("HELD", sentence_status)                                                                            ~ "DETAINED/HELD",

                                           str_detect("DISMISSED", sentence_status)                                                                       ~ "OTHER",
                                           str_detect("SENTENCE SUSPENDED", sentence_status)                                                              ~ "OTHER",

                                           str_detect("SENTENCED", sentence_status)                                                                       ~ "SENTENCED",
                                           str_detect("SENTENCED FINES", sentence_status)                                                                 ~ "SENTENCED",

                                           str_detect("STATE PRISONER", sentence_status)                                                                  ~ "NH STATE PRISONER",

                                           # PROTECTIVE CUSTODY
                                           str_detect("PROTECTIVE CUSTODY", charge_desc)                                                                  ~ "PROTECTIVE CUSTODY",
                                           str_detect("DETAINEE REQUEST", booking_type)               & str_detect("PROTECTIVE CUSTODY", sentence_status) ~ "PROTECTIVE CUSTODY",
                                           str_detect("INVOLUNTARY EMERGENCY ADMISSION", charge_desc) & str_detect("PROTECTIVE CUSTODY", sentence_status) ~ "PROTECTIVE CUSTODY",
                                           str_detect("DOMESTIC VIOLENCE OFFENSE", charge_desc)       & str_detect("PROTECTIVE CUSTODY", sentence_status) ~ "PROTECTIVE CUSTODY",

                                           TRUE ~ sentence_status)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type,
         #booking_type_standard,
         sentence_status,
         sentence_status_standard, release_type, booking_date, everything())

temp <- fnc_investigate_booking_recordings_standard(carroll_adm1)
table(carroll_adm1$booking_type_standard)
table(carroll_adm1$sentence_status_standard)

##########
# Cheshire
##########

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

  mutate(booking_type_standard = case_when(# DUAL
                                           str_detect("DUAL STATUS", sentence_status)                                       ~ "DUAL",

                                           # NH STATE PRISONER
                                           str_detect("FEDERAL INMATE", sentence_status)                                    ~ "NH STATE PRISONER",
                                           str_detect("HOLD FOR STATE PRISON", sentence_status)                             ~ "NH STATE PRISONER",
                                           (is.na(sentence_status) & str_detect("FEDERAL HOLD", booking_type))              ~ "NH STATE PRISONER",

                                           # DETAINED/HELD
                                           str_detect("HOLD FOR OTHER AGENCY", sentence_status)                             ~ "DETAINED/HELD",
                                           str_detect("DETAINEE REQUEST", sentence_status)                                  ~ "DETAINED/HELD",

                                           # PRETRIAL
                                           str_detect("PRE-TRIAL", sentence_status)                                         ~ "PRETRIAL",
                                           str_detect("PRE-TRIAL / DRUG COURT", sentence_status)                            ~ "PRETRIAL",
                                           str_detect("PRE-TRIAL / EM", sentence_status)                                    ~ "PRETRIAL",

                                           # PROTECTIVE CUSTODY
                                           str_detect("PROTECTIVE CUSTODY|PROTECTIVE CUSTODY - DRUGS", charge_desc)         ~ "PROTECTIVE CUSTODY",
                                           str_detect("PROTECTIVE CUSTODY", sentence_status)                                ~ "PROTECTIVE CUSTODY",

                                           # SENTENCED
                                           str_detect("SENTENCED", sentence_status)                                         ~ "SENTENCED",
                                           str_detect("SENTENCED / DRUG COURT", sentence_status)                            ~ "SENTENCED",
                                           str_detect("SENTENCED / EM", sentence_status)                                    ~ "SENTENCED",
                                           str_detect("SENTENCED / PROGRAM", sentence_status)                               ~ "SENTENCED",
                                           str_detect("SENTENCED / WEEKENDS", sentence_status)                              ~ "SENTENCED",
                                           str_detect("SENTENCED / WORK RELEASE", sentence_status)                          ~ "SENTENCED",

                                           # no data in sentence status but info in booking type
                                           (is.na(sentence_status) & str_detect("ADULT ORDER OF COMMITMENT", booking_type)) ~ "DETAINED/HELD",
                                           (is.na(sentence_status) & str_detect("DETAINEE REQUEST", booking_type))          ~ "DETAINED/HELD",
                                           (is.na(sentence_status) & str_detect("DETAINER", booking_type))                  ~ "DETAINED/HELD",
                                           (is.na(sentence_status) & str_detect("ELECTRONIC BENCH WARRANT", booking_type))  ~ "DETAINED/HELD",
                                           (sentence_status == "UNKNOWN" & str_detect("DETAINEE REQUEST", booking_type))    ~ "DETAINED/HELD",

                                           # OTHER
                                           (is.na(sentence_status) & str_detect("FELONY FIRST", booking_type))              ~ "OTHER",

                                           TRUE ~ booking_type)) %>%
  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, booking_type_standard, sentence_status, release_type, booking_date, everything())

temp <- fnc_investigate_booking_recordings_standard(cheshire_adm1)
temp

table(cheshire_adm1$booking_type_standard)

##########
# Coos
##########

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

  mutate(booking_type_standard = case_when(sentence_status == "DUAL STATUS" ~ "DUAL",
                                           sentence_status == "PRETRIAL"    ~ "PRETRIAL",
                                           sentence_status == "SENTENCED"   ~ "SENTENCED",

                                           TRUE ~ sentence_status)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, booking_type_standard, sentence_status, release_type, booking_date, everything())

table(coos_adm1$booking_type_standard)

##########
# Hillsborough
##########

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

  mutate(booking_type_standard = case_when(# SENTENCED
                                           str_detect("CONVICTED", sentence_status)                                                    ~ "SENTENCED",
                                           str_detect("CONVICTED ROCK", sentence_status)                                               ~ "SENTENCED",

                                           # PRETRIAL
                                           sentence_status == "PRETRIAL"                                                               ~ "PRETRIAL",
                                           sentence_status == "PRE TRIAL DRUG COURT (MANCH)"                                           ~ "PRETRIAL",
                                           sentence_status == "PRE TRIAL DRUG COURT (NASHUA)"                                          ~ "PRETRIAL",
                                           str_detect("PRE TRIAL ROCK SATCO", sentence_status)                                         ~ "PRETRIAL",
                                           str_detect("PRETRIAL BELKNAP", sentence_status)                                             ~ "PRETRIAL",
                                           str_detect("PRETRIAL CARROLL", sentence_status)                                             ~ "PRETRIAL",
                                           str_detect("PRETRIAL COOS", sentence_status)                                                ~ "PRETRIAL",
                                           str_detect("PRETRIAL DRUG COURT SATCO", sentence_status)                                    ~ "PRETRIAL",
                                           str_detect("PRETRIAL GRAFTON", sentence_status)                                             ~ "PRETRIAL",
                                           str_detect("PRETRIAL GRAFTON SATCO", sentence_status)                                       ~ "PRETRIAL",
                                           str_detect("PRETRIAL MERRIMACK", sentence_status)                                           ~ "PRETRIAL",
                                           str_detect("PRETRIAL ROCK", sentence_status)                                                ~ "PRETRIAL",
                                           str_detect("PRETRIAL SATCO", sentence_status)                                               ~ "PRETRIAL",
                                           str_detect("PRETRIAL SULLIVAN", sentence_status)                                            ~ "PRETRIAL",
                                           str_detect("PRETRIAL SULLIVAN", sentence_status)                                            ~ "PRETRIAL",
                                           str_detect("PRETRIAL SULLIVAN", sentence_status)                                            ~ "PRETRIAL",

                                           # PROTECTIVE CUSTODY
                                           str_detect("172B:1 XIII - PROTECTIVE CUSTODY 172-B:1 XIII", charge_desc)                    ~ "PROTECTIVE CUSTODY",
                                           str_detect("TREATMENT AND SERVICES", booking_type) & str_detect("PC RELEASE", release_type) ~ "PROTECTIVE CUSTODY",
                                           str_detect("NEW ARREST", booking_type) & str_detect("PC RELEASE", release_type)             ~ "PROTECTIVE CUSTODY",

                                           # SENTENCED
                                           str_detect("SENTENCED", sentence_status)                                                    ~ "SENTENCED",
                                           str_detect("SENTENCED-HSC NORTH DRUG COURT SANCTION", sentence_status)                      ~ "SENTENCED",
                                           str_detect("SENTENCED-HSC SOUTH DRUG COURT SANCTION", sentence_status)                      ~ "SENTENCED",
                                           str_detect("SENTENCED MERRIMACK", sentence_status)                                          ~ "SENTENCED",
                                           str_detect("SENTENCED ROCK", sentence_status)                                               ~ "SENTENCED",
                                           str_detect("SENTENCED ROCK SATCO", sentence_status)                                         ~ "SENTENCED",
                                           str_detect("SENTENCED SATCO", sentence_status)                                              ~ "SENTENCED",
                                           str_detect("SENTENCED SULLIVAN", sentence_status)                                           ~ "SENTENCED",
                                           str_detect("SENTENCED W/HOLD", sentence_status)                                             ~ "SENTENCED",
                                           str_detect("SENTENCED W/HOLD MERRIMACK", sentence_status)                                   ~ "SENTENCED",
                                           str_detect("SENTENCED W/HOLD ROCK", sentence_status)                                        ~ "SENTENCED",
                                           str_detect("SENTENCED W/HOLD SATCO", sentence_status)                                       ~ "SENTENCED",
                                           str_detect("SENTENCED WALK IN", sentence_status)                                            ~ "SENTENCED",
                                           str_detect("SENTENCED WEEKENDER", sentence_status)                                          ~ "SENTENCED",
                                           str_detect("SENTENCED WEEKENDER W/HOLD", sentence_status)                                   ~ "SENTENCED",

                                           # NH STATE PRISONER
                                           str_detect("STATE INMATE", sentence_status)                                                 ~ "NH STATE PRISONER",

                                           # OTHER
                                           (str_detect("TREATMENT AND SERVICES", sentence_status) & is.na(charge_desc))                ~ "OTHER",

                                           TRUE ~ sentence_status)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, booking_type_standard, sentence_status, release_type, booking_date, everything())

table(hillsborough_adm1$booking_type_standard)

##########
# Merrimack
##########

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

  mutate(booking_type_standard = case_when(#
                                           # PROTECTIVE CUSTODY
                                           str_detect("PROTECTIVE CUSTODY", charge_desc)                                                         ~ "PROTECTIVE CUSTODY",
                                           str_detect("PC-IEA", sentence_status)                                                                 ~ "PROTECTIVE CUSTODY",
                                           str_detect("DETAINEE REQUEST", booking_type) & str_detect("PROTECTIVE CUSTODY HOLD", sentence_status) ~ "PROTECTIVE CUSTODY",
                                           str_detect("ARREST WARRANT", booking_type) & str_detect("PROTECTIVE CUSTODY HOLD", sentence_status)   ~ "PROTECTIVE CUSTODY",

                                           TRUE ~ booking_type)) %>%
  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, booking_type_standard, sentence_status, release_type, booking_date, everything())

##########
# Rockingham
##########

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

  mutate(booking_type_standard = case_when(str_detect("PROTECTIVE CUSTODY", charge_desc) ~ "PROTECTIVE CUSTODY")) %>%
  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, booking_type_standard, sentence_status, release_type, booking_date, everything())

##########
# Strafford
##########

# Standardize booking info so it's consistent across counties
strafford_adm1 <- strafford_adm %>% select(-c(los, release_date)) %>% distinct() %>%

  mutate(booking_type_standard = "Unknown") %>%
  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, booking_type_standard, sentence_status, release_type, booking_date, everything())

# %>%
# filter(num_entrances < 72) # seems like an outlier or data entry issue but keep for now

##########
# Sullivan
##########

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

  mutate(booking_type_standard = case_when(# PROTECTIVE CUSTODY
                                           str_detect("PROTECTIVE CUSTODY", booking_type) ~ "PROTECTIVE CUSTODY",

                                           # PRETRIAL

                                           # SENTENCED





                                           TRUE ~ booking_type)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, booking_type_standard, sentence_status, release_type, booking_date, everything())

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# STATE-WIDE DATA
# Combine county data for large NH dataframe with all charge descriptions, booking types, etc.

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Combine jail data
adm_all <- rbind(belknap_adm1,
                 carroll_adm1,
                 cheshire_adm1,
                 coos_adm1,
                 hillsborough_adm1,
                 merrimack_adm1,
                 rockingham_adm1,
                 strafford_adm1,
                 sullivan_adm1)
# dim(adm_all) # 73183

# If race or gender are NA in some bookings but present in others, use the recorded race or gender.
# If races or genders are different for the same person, make NA since we don't know which is correct.
adm_all <- adm_all %>%

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
# Make all charges, booking types, release types, and sentence statuses uppercase
adm_all <- adm_all %>%
  mutate(los_max = ifelse(los_max == -Inf, NA, los_max)) %>%
  filter(los_max >= 0 | is.na(los_max))

# Create los categories.
adm_all <- adm_all %>%
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

# Remove rows with all missing data (37 entries).
# Find and remove bookings that have no information. These are likely errors. - CHECK WITH EACH JAIL.
# Don't remove Strafford since all of their info is blank except for dates.
all_nas <- adm_all %>%
  filter(county != "Strafford") %>%
  filter(is.na(charge_desc) &
         is.na(booking_type) &
         is.na(release_type) &
         is.na(sentence_status))
adm_all <- adm_all %>% anti_join(all_nas) %>% distinct()
# dim(adm_all); length(unique(adm_all$booking_id)); length(unique(adm_all$id)) # 73093 dim, 51545 bookings, 32177 individuals

################################################################################

# BOOKINGS & ENTRANCES (All counties)
# Includes Coos bookings even though we don't have PC hold data

################################################################################

# Remove charges, release types, and sentence statuses to get booking events/less rows.
# Create month year variables.
# There will not be one booking id per row because people can have multiple booking types per booking episode.
# This df includes pc holds.
bookings_entrances_all <- adm_all %>%
  dplyr::select(county,
                id,
                race,
                yob,
                age,
                age_category,
                gender,
                booking_id,
                los = los_max,
                los_category,
                booking_date,
                booking_type,
                booking_type_standard,
                fy,
                num_entrances,
                num_entrances_fy,
                high_utilizer_4_times,
                high_utilizer_1_pct,
                high_utilizer_5_pct,
                high_utilizer_10_pct,
                pc_hold_booking,
                pc_hold_charge,
                pc_hold_sentence,
                pc_hold) %>%
  mutate(month_year_text = format(as.Date(booking_date, "%d/%m/%Y"), "%b %Y"),
         month_year      = as.Date(as.yearmon(month_year_text))) %>%
  distinct()

# People can have multiple booking types when entering jail (some are PC hold + criminal charge)
# dups <- bookings_entrances_all[duplicated(bookings_entrances_all$booking_id)|duplicated(bookings_entrances_all$booking_id, fromLast=TRUE),] # 7932

##########

# Booking & Entrances

##########

# Some people can be booked for a criminal charge but also be held for protective custody.
# Determine if PC hold happened in booking event.
bookings_entrances <- bookings_entrances_all %>%
  dplyr::group_by(booking_id) %>%
  mutate(all_hold_types=paste(sort(unique(pc_hold)), collapse="&")) %>%
  mutate(pc_hold_in_booking = case_when(all_hold_types == 'Non-PC Hold&PC Hold' | all_hold_types == 'PC Hold' ~ "PC Hold",
                                        all_hold_types == "Non-PC Hold" ~ "Non-PC Hold")) %>%
  select(county:high_utilizer_10_pct, month_year_text:pc_hold_in_booking) %>%
  distinct()

# Combine booking types by booking id.
bookings_entrances <- bookings_entrances %>%
  group_by(booking_id) %>%
  mutate(all_booking_types=paste(sort(unique(booking_type_standard)), collapse=" & ")) %>%
  select(county: booking_type_standard, all_booking_types, everything()) %>%
  distinct()

# sep by fy year
bookings_entrances_19 <- bookings_entrances %>% distinct() %>% filter(fy == 2019)
bookings_entrances_20 <- bookings_entrances %>% distinct() %>% filter(fy == 2020)
bookings_entrances_21 <- bookings_entrances %>% distinct() %>% filter(fy == 2021)

##########

# ENTRANCES (No Coos) - not using for now.

##########

# Remove Coos.
entrances <- bookings_entrances_all %>%
  select(county,
         fy,
         id,
         booking_id,
         num_entrances,
         los,
         los_category,
         high_utilizer_4_times,
         high_utilizer_1_pct,
         high_utilizer_5_pct,
         high_utilizer_10_pct,
         month_year_text,
         month_year) %>%
  filter(county != "Coos") %>%
  droplevels() %>%
  distinct()

##########

# REMOVE PC HOLDS FROM BOOKINGS TO GET ACTUAL BOOKING NUMBERS (No Strafford)

##########

# remove Strafford, all Coos are non-PC holds
booking_no_pc_hold <- bookings_entrances_all %>%
  filter(pc_hold == "Non-PC Hold") %>%
  select(county,
         fy,
         id,
         booking_id,
         num_entrances,
         los,
         los_category,
         high_utilizer_4_times,
         high_utilizer_1_pct, high_utilizer_5_pct, high_utilizer_10_pct,
         month_year_text, month_year) %>%
  filter(county != "Strafford") %>%
  droplevels() %>%
  distinct()

################################################################################

# PC hold data frame

################################################################################

# instead of removing coos and strafford, I label their pc_hold_in_booking as NA to include them in tables
df_pch <- bookings_entrances %>%
  #filter(county != "Coos" & county != "Strafford") %>%
  select(fy,
         county,
         id,
         booking_id,
         booking_date,
         pc_hold_in_booking,
         month_year_text,
         month_year) %>%
  distinct() %>%
  mutate(pc_hold_in_booking = as.character(pc_hold_in_booking)) %>%
  mutate(pc_hold_in_booking = ifelse(county == "Coos", NA, pc_hold_in_booking)) %>%
  droplevels()

################################################################################

# Counties in data

################################################################################

# get list of counties
counties <- adm_all$county %>%
  unique() %>%
  sort()

################################################################################

# Charges dataframe - not using for now

################################################################################

# create dataframe that includes charge descriptions
charges <- adm_all %>%
  dplyr::select(county,
                id,
                race,
                yob,
                age,
                age_category,
                gender,
                booking_id,
                booking_date,
                charge_code,
                charge_desc,
                booking_type,
                booking_type_standard,
                release_type,
                sentence_status,
                los = los_max,
                los_category,
                fy,
                num_entrances,
                high_utilizer_4_times,
                high_utilizer_1_pct,
                high_utilizer_5_pct,
                high_utilizer_10_pct,
                pc_hold_booking,
                pc_hold_charge,
                pc_hold_sentence,
                pc_hold_release,
                pc_hold) %>%
  distinct()

# save booking dates
all_booking_dates <- bookings_entrances %>%
  select(county, id, booking_id, booking_date, month_year_text, month_year, fy) %>%
  distinct()

# save charges in a spreadsheet for manual work
manual_charge_categories <- charges %>% ungroup() %>%
  select(charge_code, charge_desc, booking_type, release_type, sentence_status) %>%
  distinct() %>%
  group_by(charge_code, charge_desc) %>%
  summarise(total = n())

# match descriptions without a code with charge codes - USEFUL?
charge_codes <- charge_codes.xlsx %>%
  clean_names() %>%
  mutate(descriptor = toupper(descriptor)) %>%
  select(smart_code, ctl_number, vis, descriptor, offense_statute, degree) %>%
  distinct()
manual_charge_categories_with_codes <- merge(manual_charge_categories, charge_codes, by.x = "charge_desc", by.y = "descriptor", all.x = TRUE)

# save charges in a spreadsheet for manual work
manual_charge_categories_with_booking_details <- charges %>% ungroup() %>%
  select(charge_code, charge_desc, booking_type, release_type, sentence_status) %>%
  distinct() %>%
  group_by(charge_code, charge_desc, booking_type, release_type, sentence_status) %>%
  summarise(total = n())

# save files to SP for collaboration
write.xlsx(manual_charge_categories,
           file=paste0(sp_data_path, "/Data/Offense Information/Manual_charge_categories.xlsx",                      sep = ""))
write.xlsx(manual_charge_categories_with_codes,
           file=paste0(sp_data_path, "/Data/Offense Information/Manual_charge_categories_with_codes.xlsx",           sep = ""))
write.xlsx(manual_charge_categories_with_booking_details,
           file=paste0(sp_data_path, "/Data/Offense Information/Manual_charge_categories_with_booking_details.xlsx", sep = ""))

################################################################################

# Release types data frame - not using for now

################################################################################

release_type <- adm_all %>%
  select(county,
         id,
         fy,
         booking_id,
         release_type) %>%
  distinct()

# sep by fy year
release_type_19 <- release_type %>% select(county, id, fy, booking_id, release_type) %>% distinct() %>% filter(fy == 2019)
release_type_20 <- release_type %>% select(county, id, fy, booking_id, release_type) %>% distinct() %>% filter(fy == 2020)
release_type_21 <- release_type %>% select(county, id, fy, booking_id, release_type) %>% distinct() %>% filter(fy == 2021)

################################################################################

# Sentence statuses data frame - not using for now

################################################################################

sentence_status <- adm_all %>%
  select(county,
         id,
         fy,
         booking_id,
         sentence_status) %>%
  distinct()

# sep by fy year
sentence_status_19 <- sentence_status %>% select(county, id, fy, booking_id, sentence_status) %>% distinct() %>% filter(fy == 2019)
sentence_status_20 <- sentence_status %>% select(county, id, fy, booking_id, sentence_status) %>% distinct() %>% filter(fy == 2020)
sentence_status_21 <- sentence_status %>% select(county, id, fy, booking_id, sentence_status) %>% distinct() %>% filter(fy == 2021)

################################################################################

# Save data to SP for data dictionaries Rmd

################################################################################

save(belknap_adm1,      file=paste0(sp_data_path, "/Data/r_data/data_dictionaries_page/belknap_adm.Rda",      sep = ""))
save(carroll_adm1,      file=paste0(sp_data_path, "/Data/r_data/data_dictionaries_page/carroll_adm.Rda",      sep = ""))
save(cheshire_adm1,     file=paste0(sp_data_path, "/Data/r_data/data_dictionaries_page/cheshire_adm.Rda",     sep = ""))
save(coos_adm1,         file=paste0(sp_data_path, "/Data/r_data/data_dictionaries_page/coos_adm.Rda",         sep = ""))
save(hillsborough_adm1, file=paste0(sp_data_path, "/Data/r_data/data_dictionaries_page/hillsborough_adm.Rda", sep = ""))
save(merrimack_adm1,    file=paste0(sp_data_path, "/Data/r_data/data_dictionaries_page/merrimack_adm.Rda",    sep = ""))
save(rockingham_adm1,   file=paste0(sp_data_path, "/Data/r_data/data_dictionaries_page/rockingham_adm.Rda",   sep = ""))
save(strafford_adm1,    file=paste0(sp_data_path, "/Data/r_data/data_dictionaries_page/strafford_adm.Rda",    sep = ""))
save(sullivan_adm1,     file=paste0(sp_data_path, "/Data/r_data/data_dictionaries_page/sullivan_adm.Rda",     sep = ""))
save(adm_all,           file=paste0(sp_data_path, "/Data/r_data/data_dictionaries_page/adm_all.Rda",          sep = ""))
