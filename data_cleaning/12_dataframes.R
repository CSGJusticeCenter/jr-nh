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
# Manually fix PC hold recordings and booking types if needed - based off of jail discussions.

################################################################################

##########
# Belknap
##########

# If charge is present then it was a mistake to book them as a PC hold.
# Change to non-PC hold and the booking type to unknown these since they aren't PC holds. Keep charge info though.
belknap_adm1 <- belknap_adm %>% select(-c(los, release_date)) %>% distinct() %>%
  mutate(pc_hold = as.character(pc_hold)) %>%
  mutate(pc_hold = case_when(( charge_desc == "TEMPORARY REMOVAL OR TRANSFER" |
                               charge_desc == "RESIST ARREST OR DETENTION 642:2" |
                               charge_desc == "DISORDERLY CONDUCT 644:2" |
                               charge_desc == "DRIVING OR OPERATING UNDER THE INFLUENCE OF DRUGSOR LIQUOR 265-A:2" |
                               charge_desc == "RESISTING ARREST 594:5"|
                               charge_desc == "SIMPLE ASSAULT 631:2-A" |
                               charge_desc == "VIOLATION OF PROTECTIVE ORDER")
                              & booking_type == "PROTECTIVE CUSTODY"              ~ "Non-PC Hold",
                              TRUE                                                ~ pc_hold)) %>%
  mutate(booking_type = as.character(booking_type)) %>%
  mutate(booking_type = case_when(( charge_desc == "TEMPORARY REMOVAL OR TRANSFER" |
                                    charge_desc == "RESIST ARREST OR DETENTION 642:2" |
                                    charge_desc == "DISORDERLY CONDUCT 644:2" |
                                    charge_desc == "DRIVING OR OPERATING UNDER THE INFLUENCE OF DRUGSOR LIQUOR 265-A:2" |
                                    charge_desc == "RESISTING ARREST 594:5"|
                                    charge_desc == "SIMPLE ASSAULT 631:2-A" |
                                    charge_desc == "VIOLATION OF PROTECTIVE ORDER")
                                   & booking_type == "PROTECTIVE CUSTODY"         ~ "Unknown",
                                  TRUE ~ booking_type))

##########
# Carroll
##########

carroll_adm1 <- carroll_adm %>% select(-c(los, release_date)) %>% distinct()

##########
# Cheshire
##########

# If charge is temporary removal or transfer and sentence status indicates PC hold then it isn't a PC hold.
# Change sentence status to unknown for these since they aren't PC holds.
cheshire_adm1 <- cheshire_adm %>%
  select(-c(los, release_date)) %>%
  distinct() %>%
  mutate(pc_hold = as.character(pc_hold)) %>%
  mutate(sentence_status = as.character(sentence_status)) %>%

  mutate(pc_hold = case_when(charge_desc == "TEMPORARY REMOVAL OR TRANSFER" & sentence_status == "PROTECTIVE CUSTODY" ~ "Non-PC Hold", TRUE ~ pc_hold)) %>%
  mutate(sentence_status = case_when(charge_desc == "TEMPORARY REMOVAL OR TRANSFER" & sentence_status == "PROTECTIVE CUSTODY" ~ "Unknown", TRUE ~ sentence_status))

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
# %>%
# filter(num_entrances < 72) # seems like an outlier or data entry issue but keep for now

##########
# Sullivan
##########

sullivan_adm1 <- sullivan_adm %>% select(-c(los, release_date)) %>% distinct()

##########
# save data to SP for data dictionaries Rmd
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

  # race
  dplyr::group_by(id) %>%
  fill(race, .direction = "downup") %>%
  distinct() %>%
  group_by(id) %>%
  mutate(different_race_recorded = n_distinct(race) == 1) %>%
  mutate(race = ifelse(different_race_recorded == FALSE, NA, race)) %>%
  distinct() %>%

  # gender
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
  filter(los_max >= 0 | is.na(los_max)) %>%
  mutate(charge_desc     = toupper(charge_desc),
         booking_type    = toupper(booking_type),
         release_type    = toupper(release_type),
         sentence_status = toupper(sentence_status),
         charge_desc     = as.character(charge_desc))

##########
# Standardize booking types for PC holds
##########

# If PC hold is in charge description and it has been confirmed that the booking type is a PC hold,
#    change the booking_type (new variable, booking_type_withpcs, to preserve raw booking type) to a PC hold.
# Gets a more accurate count of other booking types and the number of PC holds.

adm_all <- adm_all %>%

  mutate(booking_type_withpcs =

            ###########
            # Change booking type to PC Hold
            ###########

  case_when(county == "Belknap"      & str_detect("PROTECTIVE CUSTODY|PROTECTIVE CUSTODY/INTOXICATION", charge_desc) ~ "PROTECTIVE CUSTODY",


            county == "Carroll"      & str_detect("PROTECTIVE CUSTODY", charge_desc)                                 ~ "PROTECTIVE CUSTODY",

            county == "Carroll"      & str_detect("DETAINEE REQUEST",   booking_type) &
                                       str_detect("PROTECTIVE CUSTODY", sentence_status)                             ~ "PROTECTIVE CUSTODY",

            county == "Carroll"      & str_detect("INVOLUNTARY EMERGENCY ADMISSION", charge_desc) &
                                       str_detect("PROTECTIVE CUSTODY",              sentence_status)                ~ "PROTECTIVE CUSTODY",

            county == "Carroll"      & str_detect("DOMESTIC VIOLENCE OFFENSE", charge_desc) &
                                       str_detect("PROTECTIVE CUSTODY",        sentence_status)                      ~ "PROTECTIVE CUSTODY",

            county == "Cheshire"     & str_detect("PROTECTIVE CUSTODY|PROTECTIVE CUSTODY - DRUGS", charge_desc)      ~ "PROTECTIVE CUSTODY",

            county == "Cheshire"     & str_detect("DETAINEE REQUEST",   booking_type) &
                                       str_detect("PROTECTIVE CUSTODY", sentence_status)                             ~ "PROTECTIVE CUSTODY",

            #county == "Coos"        no data

            county == "Hillsborough" & str_detect("172B:1 XIII - PROTECTIVE CUSTODY 172-B:1 XIII", charge_desc)      ~ "PROTECTIVE CUSTODY",

            county == "Hillsborough" & str_detect("TREATMENT AND SERVICES", booking_type) &
                                       str_detect("PC RELEASE",             release_type)                            ~ "PROTECTIVE CUSTODY",

            county == "Hillsborough" & str_detect("NEW ARREST", booking_type) &
                                       str_detect("PC RELEASE", release_type)                                        ~ "PROTECTIVE CUSTODY",

            county == "Merrimack"    & str_detect("PROTECTIVE CUSTODY", charge_desc)                                 ~ "PROTECTIVE CUSTODY",

            county == "Merrimack"    & str_detect("PC-IEA", sentence_status)                                         ~ "PROTECTIVE CUSTODY",

            county == "Merrimack"    & str_detect("DETAINEE REQUEST",        booking_type) &
                                       str_detect("PROTECTIVE CUSTODY HOLD", sentence_status)                        ~ "PROTECTIVE CUSTODY",

            county == "Merrimack"    & str_detect("ARREST WARRANT",          booking_type) &
                                       str_detect("PROTECTIVE CUSTODY HOLD", sentence_status)                        ~ "PROTECTIVE CUSTODY",

            county == "Rockingham"   & str_detect("PROTECTIVE CUSTODY", charge_desc)                                 ~ "PROTECTIVE CUSTODY",

            #county == "Strafford"   no data

            county == "Sullivan"     & str_detect("TREATMENT AND SERVICES: PROTECTIVE CUSTODY", charge_desc)         ~ "PROTECTIVE CUSTODY",

            ###########
            # Combine some booking types together
            ###########

            county == "Hillsborough" & str_detect("BOARDED", booking_type)
                                     & str_detect("CONVICTED|CONVICTED ROCK", sentence_status)                                              ~ "CONVICTED",

            county == "Hillsborough" & str_detect("BOARDED", booking_type)
                                     & str_detect("PRE TRIAL ROCK SATCO|PRETRIAL|PRETRIAL ROCK|PRETRIAL SULLIVAN", sentence_status)         ~ "PRETRIAL",

            county == "Hillsborough" & str_detect("BOARDED", booking_type)
                                     & str_detect("SENTENCED|SENTENCED ROCK|SENTENCED ROCK SATCO|SENTENCED W/HOLD MERRIMACK|SENTENCED W/HOLD ROCK", sentence_status) ~ "SENTENCED",

            TRUE ~ booking_type)) %>%
  select(id, booking_id, county, charge_desc, booking_type, booking_type_withpcs, sentence_status, release_type, everything())

# Combine some booking types together (some are the same or it makes sense to group them).
adm_all <- adm_all %>%
  mutate(booking_type_standard =
  case_when(booking_type_withpcs == "DETAINEE REQUEST" |
            booking_type_withpcs == "DETAINER"         |
            booking_type_withpcs == "DETENTION ORDER"  |
            booking_type_withpcs == "DETAINERS,WARRANTS,HOLDS" |

            booking_type_withpcs == "ARREST WARRANT" |
            booking_type_withpcs == "ELECTRONIC BENCH WARRANT" |
            booking_type_withpcs == "SUPERIOR COURT ARREST WARRANT" |
            booking_type_withpcs == "WARRANT ARREST" |
            booking_type_withpcs == "CAPIAS" |

            booking_type_withpcs == "24 HOUR DETENTION REQUEST" |
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

            booking_type_withpcs == "CONVICTED" |      # not sure if this should be here
            booking_type_withpcs == "CONVICTED ROCK" | # not sure if this should be here
            booking_type_withpcs == "SENTENCED" |
            booking_type_withpcs == "SENTENCING" |
            booking_type_withpcs == "WALK IN-SENTENCED"               ~ "SENTENCED/SENTENCING",

            TRUE ~ booking_type_withpcs)) %>%

  select(id, booking_id, county, charge_desc, booking_type, booking_type_withpcs, booking_type_standard, sentence_status, release_type, everything())

# Change all "Unknown" to NA for booking types, sentence statuses, and release types
adm_all$booking_type[adm_all$booking_type                   == "UNKNOWN"] <- NA
adm_all$booking_type_standard[adm_all$booking_type_standard == "UNKNOWN"] <- NA
adm_all$sentence_status[adm_all$sentence_status             == "UNKNOWN"] <- NA
adm_all$release_type[adm_all$release_type                   == "UNKNOWN"] <- NA

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
  mutate(all_booking_types=paste(sort(unique(booking_type)), collapse=" & ")) %>%
  select(county: booking_type, all_booking_types, everything()) %>%
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
