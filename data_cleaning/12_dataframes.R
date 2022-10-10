############################################
# Project: JRI New Hampshire
# File: dataframes.R
# Last updated: August 31, 2022
# Author: Mari Roberts

# Generate tables for state overview

# Creates data frames:
# nh_adm_all
# nh_charges
# nh_booking - used the most, booking events
# nh_release_types
# nh_sentence_statuses
############################################

##########
# Standardize data
##########

# load R files and standardize county data variables
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

# custom function that creates the variables we need and relabels codes so they're consistent across counties
# creates booking_id, los, fy, num_bookings,
# high_utilizer_1_pct(y/n), high_utilizer_3_pct(y/n), high_utilizer_5_pct(y/n),
# pc_hold_booking(y/n), pc_hold_charge(y/n), pc_hold_sentence(y/n), pc_hold_release(y/n),
# pc_hold(y/n) which is the overall pc hold variable (if pc hold was indicated in other variables)

belknap_adm      <- fnc_standardize_counties(belknap_adm_all,      "Belknap")
carroll_adm      <- fnc_standardize_counties(carroll_adm_all,      "Carroll")
cheshire_adm     <- fnc_standardize_counties(cheshire_adm_all,     "Cheshire")
coos_adm         <- fnc_standardize_counties(coos_adm_all,         "Coos")
hillsborough_adm <- fnc_standardize_counties(hillsborough_adm_all, "Hillsborough")
merrimack_adm    <- fnc_standardize_counties(merrimack_adm_all,    "Merrimack")
rockingham_adm   <- fnc_standardize_counties(rockingham_adm_all,   "Rockingham")
strafford_adm    <- fnc_standardize_counties(strafford_adm_all,    "Strafford")
sullivan_adm     <- fnc_standardize_counties(sullivan_adm_all,     "Sullivan")

# removes duplicates with strafford where race is indicated in some booking ids but NA in others
dups <- strafford_adm[duplicated(strafford_adm$booking_id)|duplicated(strafford_adm$booking_id, fromLast=TRUE),]
temp <- strafford_adm %>% anti_join(dups)
dups <- dups %>% group_by(booking_id) %>% filter(!is.na(race)) %>% droplevels() %>% distinct()
strafford_adm <- rbind(temp, dups)

##################################################
# remove LOS and release date duplicates due to release date issues
# manually fix PC hold recordings if needed - based off of jail discussions
##################################################

##########
# Belknap
##########

# if charge is present then it was a mistake to book them as a PC hold
# change booking type to unknown for these since they aren't PC holds
belknap_adm1 <- belknap_adm %>% select(-c(los, release_date)) %>% distinct() %>%
  mutate(pc_hold = ifelse((charge_desc == "TEMPORARY REMOVAL OR TRANSFER" |
                           charge_desc == "RESIST ARREST OR DETENTION 642:2" |
                           charge_desc == "DISORDERLY CONDUCT 644:2" |
                           charge_desc == "DRIVING OR OPERATING UNDER THE INFLUENCE OF DRUGSOR LIQUOR 265-A:2" |
                           charge_desc == "RESISTING ARREST 594:5"|
                           charge_desc == "SIMPLE ASSAULT 631:2-A" |
                           charge_desc == "VIOLATION OF PROTECTIVE ORDER")
                          & booking_type == "PROTECTIVE CUSTODY", "Non-PC Hold", pc_hold)) %>%

  mutate(booking_type = ifelse((charge_desc == "TEMPORARY REMOVAL OR TRANSFER" |
                                charge_desc == "RESIST ARREST OR DETENTION 642:2" |
                                charge_desc == "DISORDERLY CONDUCT 644:2" |
                                charge_desc == "DRIVING OR OPERATING UNDER THE INFLUENCE OF DRUGSOR LIQUOR 265-A:2" |
                                charge_desc == "RESISTING ARREST 594:5"|
                                charge_desc == "VIOLATION OF PROTECTIVE ORDER")
                               & booking_type == "PROTECTIVE CUSTODY", "Unknown", booking_type))

# fix coding for pc holds
belknap_adm1 <- belknap_adm1 %>%
  mutate(pc_hold = as.character(pc_hold)) %>%
  mutate(pc_hold = case_when(pc_hold == "2" ~ "PC Hold",
                             pc_hold == "1" ~ "Non-PC Hold",
                             pc_hold == "Non-PC Hold" ~ "Non-PC Hold"))

##########
# Carroll
##########

carroll_adm1 <- carroll_adm %>% select(-c(los, release_date)) %>% distinct()

##########
# Cheshire
##########

# if charge is temporary removal or transfer and sentence status indicates PC hold then it isn't a PC hold
# change sentence status to unknown for these since they aren't PC holds
cheshire_adm1 <- cheshire_adm %>% select(-c(los, release_date)) %>% distinct() %>%
  mutate(pc_hold         = ifelse(charge_desc == "TEMPORARY REMOVAL OR TRANSFER" & sentence_status == "PROTECTIVE CUSTODY", "Non-PC Hold", pc_hold)) %>%
  mutate(sentence_status = ifelse(charge_desc == "TEMPORARY REMOVAL OR TRANSFER" & sentence_status == "PROTECTIVE CUSTODY", "Unknown", sentence_status))
cheshire_adm1 <- cheshire_adm1 %>%
  mutate(pc_hold = as.character(pc_hold)) %>%
  mutate(pc_hold = case_when(pc_hold == "2" ~ "PC Hold",
                             pc_hold == "1" ~ "Non-PC Hold",
                             pc_hold == "Non-PC Hold" ~ "Non-PC Hold"))

##########
# Coos
##########

coos_adm1 <- coos_adm %>% select(-c(los, release_date)) %>% distinct()

##########
# Hillsborough
##########

hillsborough_adm1 <- hillsborough_adm %>% select(-c(los, release_date)) %>% distinct()

##########
# Merrimack
##########

merrimack_adm1 <- merrimack_adm %>% select(-c(los, release_date)) %>% distinct()

##########
# Rockingham
##########

rockingham_adm1 <- rockingham_adm %>% select(-c(los, release_date)) %>% distinct()

##########
# Strafford
##########

strafford_adm1 <- strafford_adm %>% select(-c(los, release_date)) %>% distinct()

##########
# Sullivan
##########

sullivan_adm1 <- sullivan_adm %>% select(-c(los, release_date)) %>% distinct()

##########
# save data to SP for data dictionaries
##########

save(belknap_adm1,      file=paste0(sp_data_path, "/Data/r_data/belknap_adm.Rda",      sep = ""))
save(carroll_adm1,      file=paste0(sp_data_path, "/Data/r_data/carroll_adm.Rda",      sep = ""))
save(cheshire_adm1,     file=paste0(sp_data_path, "/Data/r_data/cheshire_adm.Rda",     sep = ""))
save(coos_adm1,         file=paste0(sp_data_path, "/Data/r_data/coos_adm.Rda",         sep = ""))
save(hillsborough_adm1, file=paste0(sp_data_path, "/Data/r_data/hillsborough_adm.Rda", sep = ""))
save(merrimack_adm1,    file=paste0(sp_data_path, "/Data/r_data/merrimack_adm.Rda",    sep = ""))
save(rockingham_adm1,   file=paste0(sp_data_path, "/Data/r_data/rockingham_adm.Rda",   sep = ""))
save(strafford_adm1,    file=paste0(sp_data_path, "/Data/r_data/strafford_adm.Rda",    sep = ""))
save(sullivan_adm1,     file=paste0(sp_data_path, "/Data/r_data/sullivan_adm.Rda",     sep = ""))

####################################################

# STATE-WIDE DATA
# combine county data for large NH dataframe with all charge descriptions, booking types, etc.

####################################################

# combine jail data
nh_adm_all <- rbind(belknap_adm1,
                    carroll_adm1,
                    cheshire_adm1,
                    coos_adm1,
                    hillsborough_adm1,
                    merrimack_adm1,
                    rockingham_adm1,
                    strafford_adm1,
                    sullivan_adm1)

# fix los_max issues
# remove negatives because of data entry issues with booking and release dates
# if release date is missing, then change to NA
# make all booking types uppercase
nh_adm_all <- nh_adm_all %>%
  mutate(los_max = ifelse(los_max == -Inf, NA, los_max)) %>%
  filter(los_max >= 0 | is.na(los_max)) %>%
  mutate(charge_desc     = toupper(charge_desc),
         booking_type    = toupper(booking_type),
         release_type    = toupper(release_type),
         sentence_status = toupper(sentence_status),
         charge_desc = as.character(charge_desc))

# Some bookings have unknown race but their race was recorded in other bookings, use this race
# temp <- nh_charges %>% select(booking_id, race) %>% distinct()
# dim(temp); length(unique(temp$booking_id))
# temp <- temp %>% group_by(booking_id) %>% summarise(n = n())
nh_adm_all <- nh_adm_all %>%
  mutate(race = as.character(race)) %>%
  mutate(race = case_when(booking_id == "Strafford_booking_2169"    ~ "Unknown",
                          booking_id == "Carroll_booking_3050"      ~ "White",
                          booking_id == "Hillsborough_booking_6957" ~ "Black",
                          booking_id == "Hillsborough_booking_7548" ~ "Black",
                          TRUE ~ race)) %>% distinct()

##########
# Standardize booking types for PC holds
##########

# if PC hold is in charge description and it has been confirmed that the booking type is a PC hold,
# change the booking_type to a PC hold - gets a more accurate count of other booking types
nh_adm_all <- nh_adm_all %>%
  mutate(booking_type_withpcs =
  case_when(county == "Belknap"      & str_detect("PROTECTIVE CUSTODY|PROTECTIVE CUSTODY/INTOXICATION", charge_desc) ~ "PROTECTIVE CUSTODY",

            county == "Carroll"      & str_detect("PROTECTIVE CUSTODY", charge_desc)                                 ~ "PROTECTIVE CUSTODY",

            county == "Carroll"      & str_detect("DETAINEE REQUEST", booking_type) &
                                       str_detect("PROTECTIVE CUSTODY", sentence_status)                             ~ "PROTECTIVE CUSTODY",

            county == "Carroll"      & str_detect("INVOLUNTARY EMERGENCY ADMISSION", charge_desc) &
                                       str_detect("PROTECTIVE CUSTODY", sentence_status)                             ~ "PROTECTIVE CUSTODY",

            county == "Carroll"      & str_detect("DOMESTIC VIOLENCE OFFENSE", charge_desc) &
                                       str_detect("PROTECTIVE CUSTODY", sentence_status)                             ~ "PROTECTIVE CUSTODY",

            county == "Cheshire"     & str_detect("PROTECTIVE CUSTODY|PROTECTIVE CUSTODY - DRUGS", charge_desc)      ~ "PROTECTIVE CUSTODY",

            county == "Cheshire"     & str_detect("DETAINEE REQUEST", booking_type) &
                                       str_detect("PROTECTIVE CUSTODY", sentence_status)                        ~ "PROTECTIVE CUSTODY",

            #county == "Coos"        no info

            county == "Hillsborough" & str_detect("172B:1 XIII - PROTECTIVE CUSTODY 172-B:1 XIII", charge_desc)      ~ "PROTECTIVE CUSTODY",

            county == "Hillsborough" & str_detect("TREATMENT AND SERVICES", booking_type) &
                                       str_detect("PC RELEASE", release_type)                                        ~ "PROTECTIVE CUSTODY",

            county == "Hillsborough" & str_detect("NEW ARREST", booking_type) &
                                       str_detect("PC RELEASE", release_type)                                        ~ "PROTECTIVE CUSTODY",

            county == "Merrimack"    & str_detect("PROTECTIVE CUSTODY", charge_desc)                                 ~ "PROTECTIVE CUSTODY",

            county == "Merrimack"    & str_detect("DETAINEE REQUEST", booking_type) &
                                       str_detect("PROTECTIVE CUSTODY HOLD", sentence_status)                        ~ "PROTECTIVE CUSTODY",

            county == "Merrimack"    & str_detect("ARREST WARRANT", booking_type) &
                                       str_detect("PROTECTIVE CUSTODY HOLD", sentence_status)                        ~ "PROTECTIVE CUSTODY",

            county == "Rockingham"   & str_detect("PROTECTIVE CUSTODY", charge_desc)                                 ~ "PROTECTIVE CUSTODY",

            #county == "Strafford"   no data

            county == "Sullivan"     & str_detect("TREATMENT AND SERVICES: PROTECTIVE CUSTODY", charge_desc)         ~ "PROTECTIVE CUSTODY",

            TRUE ~ booking_type)) %>%
  select(id, booking_id, county, charge_desc, booking_type, booking_type_withpcs, sentence_status, release_type, everything())

# temp <- nh_adm_all %>% filter(pc_hold == "PC Hold") %>% group_by(county, charge_desc, booking_type, booking_type_withpcs, sentence_status, release_type, pc_hold) %>% summarise(total = n())

# combine some booking types together (some are the same or it makes sense to group them)
nh_adm_all <- nh_adm_all %>%
  mutate(booking_type_standard =
  case_when(booking_type_withpcs == "DETAINEE REQUEST" |
            booking_type_withpcs == "DETAINER"         |
            booking_type_withpcs == "DETENTION ORDER"  |
            booking_type_withpcs == "DETAINERS,WARRANTS,HOLDS" |

            booking_type_withpcs == "ARREST WARRANT" |
            booking_type_withpcs == "ELECTRONIC BENCH WARRANT" |
            booking_type_withpcs == "SUPERIOR COURT ARREST WARRANT" |
            booking_type_withpcs == "WARRANT ARREST" |

            booking_type_withpcs == "FEDERAL HOLD" |
            booking_type_withpcs == "HOLD FOR ANOTHER AGENCY" |
            booking_type_withpcs == "HOLD SHEET" |
            booking_type_withpcs == "OVERNIGHT HOLD" |
            booking_type_withpcs == "ADULT ORDER OF COMMITMENT"       ~ "DETAINERS, WARRANTS, HOLDS",

            booking_type_withpcs == "ADMIN TRANSFER" |
            booking_type_withpcs == "ADMINISTRATIVE TRANSFER"         ~ "ADMINISTRATIVE TRANSFER",

            booking_type_withpcs == "BAIL ORDER" |
            booking_type_withpcs == "CIRCUIT COURT BAIL ORDER" |
            booking_type_withpcs == "SUPERIOR COURT BAIL ORDER"       ~ "BAIL ORDER (CIRCUIT/SUPERIOR)",

            booking_type_withpcs == "DRUG COURT" |
            booking_type_withpcs == "DRUG COURT SENTENCING ORDER"     ~ "DRUG COURT",

            booking_type_withpcs == "PROBATION" |
            booking_type_withpcs == "PROBATION DETENTION ORDER" |
            booking_type_withpcs == "PROBATION/PAROLE VIOLATION" |
            booking_type_withpcs == "PAROLE" |
            booking_type_withpcs == "VIOLATION OF PAROLE" |
            booking_type_withpcs == "VIOLATION OF PROBATION"          ~ "PROBATION/PAROLE (VIOLATION/DETENTION ORDER)",

            booking_type_withpcs == "SENTENCED" |
            booking_type_withpcs == "SENTENCING" |
            booking_type_withpcs == "WALK IN-SENTENCED"               ~ "SENTENCED/SENTENCING",

            TRUE ~ booking_type_withpcs)) %>%

  select(id, booking_id, county, charge_desc, booking_type, booking_type_withpcs, booking_type_standard, sentence_status, release_type, everything())

# Change all "Unknown" to NA
nh_adm_all$booking_type_standard[nh_adm_all$booking_type_standard == "UNKNOWN"] <- NA
nh_adm_all$sentence_status[nh_adm_all$sentence_status             == "UNKNOWN"] <- NA
nh_adm_all$release_type[nh_adm_all$release_type                   == "UNKNOWN"] <- NA

# temp <- nh_adm_all %>% filter(pc_hold == "PC Hold") %>% group_by(county, charge_desc, booking_type, booking_type_withpcs, sentence_status, release_type, pc_hold) %>% summarise(total = n())

####################################################
# Charges dataframe
####################################################

# create dataframe that includes charge descriptions
nh_charges <- nh_adm_all %>%
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
                fy,
                num_bookings,
                high_utilizer_1_pct,
                high_utilizer_3_pct,
                high_utilizer_5_pct,
                pc_hold_booking,
                pc_hold_charge,
                pc_hold_sentence,
                pc_hold) %>%
  distinct()
dim(nh_charges) # 73124

####################################################
# Booking type dataframe
####################################################

# remove charges, relese types, and sentence statuses to get booking events/less rows
# create month year variables
# there will not be one booking id per row because people can have multiple booking types per booking episode
nh_booking <- nh_adm_all %>%
  dplyr::select(county,
                id,
                race,
                yob,
                age,
                age_category,
                gender,
                booking_id,
                los = los_max,
                booking_date,
                booking_type,
                booking_type_standard,
                fy,
                num_bookings,
                high_utilizer_1_pct,
                high_utilizer_3_pct,
                high_utilizer_5_pct,
                pc_hold_booking,
                pc_hold_charge,
                pc_hold_sentence,
                pc_hold) %>%
  mutate(month_year_text = format(as.Date(booking_date, "%d/%m/%Y"), "%b %Y"),
         month_year      = as.Date(as.yearmon(month_year_text))) %>%
  distinct()

# determine if PC hold happened in booking event
# detach(package:plyr)
nh_booking <- nh_booking %>%
  dplyr::group_by(booking_id) %>%
  mutate(all_hold_types=paste(sort(unique(pc_hold)), collapse="&")) %>%
  mutate(pc_hold_in_booking = case_when(all_hold_types == 'Non-PC Hold&PC Hold' | all_hold_types == 'PC Hold' ~ "PC Hold Booking",
                                        all_hold_types == "Non-PC Hold" ~ "Non-PC Hold Booking")) %>%
  select(county:high_utilizer_5_pct, month_year_text:pc_hold_in_booking) %>%
  distinct()

dim(nh_booking)                       # 53658 = booking_type_standard, 54813 = booking_type, 55126 = booking_type_standard/booking_type
length(unique(nh_booking$booking_id)) # 51575
dups <- nh_booking[duplicated(nh_booking$booking_id)|duplicated(nh_booking$booking_id, fromLast=TRUE),] # 5963

# combine booking types by booking id
nh_booking <- nh_booking %>%
  group_by(booking_id) %>%
  mutate(all_booking_types=paste(sort(unique(booking_type)), collapse=" & ")) %>%
  select(county: booking_type, all_booking_types, everything()) %>%
  distinct()

# sep by fy year
nh_booking_19 <- nh_booking %>% distinct() %>% filter(fy == 2019)
nh_booking_20 <- nh_booking %>% distinct() %>% filter(fy == 2020)
nh_booking_21 <- nh_booking %>% distinct() %>% filter(fy == 2021)

########################################################################################################
# Release types
########################################################################################################

nh_release_type <- nh_adm_all %>%
  select(county,
         id,
         fy,
         booking_id,
         release_type) %>%
  distinct()

# sep by fy year
nh_release_type_19 <- nh_release_type %>% select(county, id, fy, booking_id, release_type) %>% distinct() %>% filter(fy == 2019)
nh_release_type_20 <- nh_release_type %>% select(county, id, fy, booking_id, release_type) %>% distinct() %>% filter(fy == 2020)
nh_release_type_21 <- nh_release_type %>% select(county, id, fy, booking_id, release_type) %>% distinct() %>% filter(fy == 2021)

########################################################################################################
# Sentence statuses
########################################################################################################

nh_sentence_status <- nh_adm_all %>%
  select(county,
         id,
         fy,
         booking_id,
         sentence_status) %>%
  distinct()

# sep by fy year
nh_sentence_status_19 <- nh_sentence_status %>% select(county, id, fy, booking_id, sentence_status) %>% distinct() %>% filter(fy == 2019)
nh_sentence_status_20 <- nh_sentence_status %>% select(county, id, fy, booking_id, sentence_status) %>% distinct() %>% filter(fy == 2020)
nh_sentence_status_21 <- nh_sentence_status %>% select(county, id, fy, booking_id, sentence_status) %>% distinct() %>% filter(fy == 2021)

########################################################################################################
# PC hold data
########################################################################################################

nh_pch <- nh_booking %>%
  filter(county != "Coos" & county != "Strafford") %>%
  select(county, id, booking_id, pc_hold_in_booking) %>%
  distinct() %>%
  droplevels()

dim(nh_pch); length(unique(nh_pch$booking_id)) # 38671

########################################################################################################
# Counties in data
########################################################################################################

# get list of counties
counties <- nh_adm_all$county %>%
  unique() %>%
  sort()
