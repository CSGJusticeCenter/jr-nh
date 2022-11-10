############################################
# Project: JRI New Hampshire
# File: standardize_counties.R
# Last updated: November 10, 2022
# Author: Mari Roberts

# Standardize charges, booking types, and other variables by county
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

# Standardize sentence statuses across counties so they have these categories:
      # 1) PROTECTIVE CUSTODY
      # 2) PRETRIAL
      # 3) SENTENCED
      # 4) NH STATE PRISONER
      # 4) UNKNOWN
      # 5) OTHER

# Felony drug court programs for adult offenders are available in
     # Belknap, Carroll, Cheshire, Coos, Grafton, Hillsborough, Merrimack, Rockingham, and Strafford

################################################################################

##########
# Belknap
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

  mutate(sentence_status_standard = case_when(# PROTECTIVE CUSTODY
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

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, sentence_status, sentence_status_standard, release_type, booking_date, everything())

# View new variable
# Reminder: no charge descriptions are in this table, which explains some PROTECTIVE CUSTODY in booking type and sentence status
temp <- fnc_investigate_booking_recordings_standard(belknap_adm1)

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

  mutate(sentence_status_standard = case_when(# PROTECTIVE CUSTODY
                                              str_detect("PROTECTIVE CUSTODY", charge_desc)                                                                  ~ "PROTECTIVE CUSTODY",
                                              str_detect("DETAINEE REQUEST", booking_type)               & str_detect("PROTECTIVE CUSTODY", sentence_status) ~ "PROTECTIVE CUSTODY",
                                              str_detect("INVOLUNTARY EMERGENCY ADMISSION", charge_desc) & str_detect("PROTECTIVE CUSTODY", sentence_status) ~ "PROTECTIVE CUSTODY",
                                              str_detect("DOMESTIC VIOLENCE OFFENSE", charge_desc)       & str_detect("PROTECTIVE CUSTODY", sentence_status) ~ "PROTECTIVE CUSTODY",

                                              # PRETRIAL
                                              str_detect("PRE-TRIAL", sentence_status)                                                                       ~ "PRETRIAL",
                                              str_detect("AWAITING TRIAL", sentence_status)                                                                  ~ "PRETRIAL",

                                              # SENTENCED
                                              str_detect("SENTENCED", sentence_status)                                                                       ~ "SENTENCED",
                                              str_detect("SENTENCED FINES", sentence_status)                                                                 ~ "SENTENCED",

                                              # NH STATE PRISONER
                                              str_detect("STATE PRISONER", sentence_status)                                                                  ~ "NH STATE PRISONER",

                                              # OTHER
                                              str_detect("BAIL SET", sentence_status)                                                                        ~ "OTHER",
                                              str_detect("BOND DENIED", sentence_status)                                                                     ~ "OTHER",
                                              str_detect("DETAINER", sentence_status)                                                                        ~ "OTHER",
                                              str_detect("HELD", sentence_status)                                                                            ~ "OTHER",
                                              str_detect("DISMISSED", sentence_status)                                                                       ~ "OTHER",
                                              str_detect("SENTENCE SUSPENDED", sentence_status)                                                              ~ "OTHER",

                                              TRUE ~ sentence_status)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, sentence_status, sentence_status_standard, release_type, booking_date, everything())

temp <- fnc_investigate_booking_recordings_standard(carroll_adm1)

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

temp <- fnc_investigate_booking_recordings_standard(cheshire_adm1)

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

  mutate(sentence_status_standard = case_when(sentence_status == "PRETRIAL"    ~ "PRETRIAL",
                                              sentence_status == "DUAL STATUS" ~ "OTHER",
                                              sentence_status == "SENTENCED"   ~ "SENTENCED",
                                              is.na(sentence_status)           ~ "UNKNOWN",

                                           TRUE ~ sentence_status)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, sentence_status, sentence_status_standard, release_type, booking_date, everything())

temp <- fnc_investigate_booking_recordings_standard(coos_adm1)

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

  mutate(sentence_status_standard = case_when(# PROTECTIVE CUSTODY
                                              str_detect("172B:1 XIII - PROTECTIVE CUSTODY 172-B:1 XIII", charge_desc)                    ~ "PROTECTIVE CUSTODY",
                                              str_detect("TREATMENT AND SERVICES", booking_type) & str_detect("PC RELEASE", release_type) ~ "PROTECTIVE CUSTODY",
                                              str_detect("NEW ARREST", booking_type) & str_detect("PC RELEASE", release_type)             ~ "PROTECTIVE CUSTODY",

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
                                              str_detect("CONVICTED", sentence_status)                                                    ~ "SENTENCED", # ?
                                              str_detect("CONVICTED ROCK", sentence_status)                                               ~ "SENTENCED", # ?

                                              # NH STATE PRISONER
                                              str_detect("STATE INMATE", sentence_status)                                                 ~ "NH STATE PRISONER",

                                              # OTHER
                                              (str_detect("TREATMENT AND SERVICES", sentence_status) & is.na(charge_desc))                ~ "OTHER",

                                              # UNKNOWN
                                              is.na(sentence_status)                                                                      ~ "UNKNOWN",

                                              TRUE ~ sentence_status)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, sentence_status, sentence_status_standard, release_type, booking_date, everything())

temp <- fnc_investigate_booking_recordings_standard(hillsborough_adm1)

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

  mutate(sentence_status_standard = case_when(# PROTECTIVE CUSTODY
                                              str_detect("PROTECTIVE CUSTODY", booking_type) & str_detect("24 HOUR HOLD", sentence_status)          ~ "PROTECTIVE CUSTODY",
                                              str_detect("PROTECTIVE CUSTODY", charge_desc)                                                         ~ "PROTECTIVE CUSTODY",
                                              str_detect("PROTECTIVE CUSTODY", sentence_status)                                                     ~ "PROTECTIVE CUSTODY",
                                              str_detect("PC-IEA", sentence_status)                                                                 ~ "PROTECTIVE CUSTODY",
                                              str_detect("DETAINEE REQUEST", booking_type) & str_detect("PROTECTIVE CUSTODY HOLD", sentence_status) ~ "PROTECTIVE CUSTODY",
                                              str_detect("ARREST WARRANT", booking_type) & str_detect("PROTECTIVE CUSTODY HOLD", sentence_status)   ~ "PROTECTIVE CUSTODY",
                                              is.na(booking_type) & str_detect("PROTECTIVE CUSTODY HOLD", sentence_status)                          ~ "PROTECTIVE CUSTODY",

                                              # PRETRIAL
                                              str_detect("PRE-TRIAL FELONY", sentence_status)                                                       ~ "PRETRIAL",
                                              str_detect("PRE-TRIAL MISDEMEANOR", sentence_status)                                                  ~ "PRETRIAL",

                                              # SENTENCED
                                              str_detect("SENTENCED FELONY", sentence_status)                                                       ~ "SENTENCED",
                                              str_detect("SENTENCED MISDEMEANOR", sentence_status)                                                  ~ "SENTENCED",
                                              str_detect("PRE-TRIAL MISDEMEANOR", sentence_status)                                                  ~ "PRETRIAL",

                                              # NH STATE PRISONER

                                              # OTHER
                                              (str_detect("24 HOUR HOLD", sentence_status) & booking_type != "PROTECTIVE CUSTODY")                  ~ "OTHER",
                                              (str_detect("24 HOUR HOLD", sentence_status) & is.na(booking_type))                                   ~ "OTHER",
                                              str_detect("72 HOUR HOLD", sentence_status)                                                           ~ "OTHER",
                                              str_detect("DUAL STATUS", sentence_status)                                                            ~ "OTHER",
                                              str_detect("PAROLE VIOLATION", sentence_status)                                                       ~ "OTHER",
                                              str_detect("PRE-TRIAL MISDEMEANOR", sentence_status)                                                  ~ "OTHER",

                                              # UNKNOWN
                                              is.na(sentence_status)                                                                                ~ "UNKNOWN",

                                              TRUE ~ sentence_status)) %>%

  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, sentence_status, sentence_status_standard, release_type, booking_date, everything())

temp <- fnc_investigate_booking_recordings_standard(merrimack_adm1)

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

temp <- fnc_investigate_booking_recordings_standard(rockingham_adm1)

##########
# Strafford
##########

# Standardize booking info so it's consistent across counties
strafford_adm1 <- strafford_adm %>% select(-c(los, release_date)) %>% distinct() %>%

  mutate(sentence_status_standard = "UNKNOWN") %>%
  select(county, fy, id, inmate_id, booking_id, charge_code, charge_desc, booking_type, sentence_status, sentence_status_standard, release_type, booking_date, everything())

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

temp <- fnc_investigate_booking_recordings_standard(sullivan_adm1)

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
