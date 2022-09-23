############################################
# Project: JRI New Hampshire
# File: dataframes.R
# Last updated: August 31, 2022
# Author: Mari Roberts

# Generate tables for state overview

# Creates:
# nh_adm_all
# nh_charges, nh_charges_19, nh_charges_20, nh_charges_21
# nh_booking, nh_booking_19, nh_booking_20, nh_booking_21
############################################

######
# Standardize and save data
######

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
belknap_adm      <- fnc_standardize_counties(belknap_adm_all,      "Belknap")
carroll_adm      <- fnc_standardize_counties(carroll_adm_all,      "Carroll")
cheshire_adm     <- fnc_standardize_counties(cheshire_adm_all,     "Cheshire")
coos_adm         <- fnc_standardize_counties(coos_adm_all,         "Coos")
hillsborough_adm <- fnc_standardize_counties(hillsborough_adm_all, "Hillsborough")
merrimack_adm    <- fnc_standardize_counties(merrimack_adm_all,    "Merrimack")
rockingham_adm   <- fnc_standardize_counties(rockingham_adm_all,   "Rockingham")
strafford_adm    <- fnc_standardize_counties(strafford_adm_all,    "Strafford")
sullivan_adm     <- fnc_standardize_counties(sullivan_adm_all,     "Sullivan")

# fix booking_id issue with strafford
# 25 people have two release dates but the same booking date, use the max release date.
dups <- strafford_adm[duplicated(strafford_adm$booking_id)|duplicated(strafford_adm$booking_id, fromLast=TRUE),]
temp <- strafford_adm %>% anti_join(dups)
dups <- dups %>% group_by(booking_id) %>% filter(!is.na(race)) %>% droplevels() %>% distinct()
strafford_adm <- rbind(temp, dups)

# save data to SP
save(belknap_adm,      file=paste0(sp_data_path, "/Data/r_data/belknap_adm.Rda",      sep = ""))
save(carroll_adm,      file=paste0(sp_data_path, "/Data/r_data/carroll_adm.Rda",      sep = ""))
save(cheshire_adm,     file=paste0(sp_data_path, "/Data/r_data/cheshire_adm.Rda",     sep = ""))
save(coos_adm,         file=paste0(sp_data_path, "/Data/r_data/coos_adm.Rda",         sep = ""))
save(hillsborough_adm, file=paste0(sp_data_path, "/Data/r_data/hillsborough_adm.Rda", sep = ""))
save(merrimack_adm,    file=paste0(sp_data_path, "/Data/r_data/merrimack_adm.Rda",    sep = ""))
save(rockingham_adm,   file=paste0(sp_data_path, "/Data/r_data/rockingham_adm.Rda",   sep = ""))
save(strafford_adm,    file=paste0(sp_data_path, "/Data/r_data/strafford_adm.Rda",    sep = ""))
save(sullivan_adm,     file=paste0(sp_data_path, "/Data/r_data/sullivan_adm.Rda",     sep = ""))

######
# STATE-WIDE DATA
# combine county data or large NH dataframe with all charge descriptions
######

nh_adm_all <- rbind(belknap_adm,
                    carroll_adm,
                    cheshire_adm,
                    coos_adm,
                    hillsborough_adm,
                    merrimack_adm,
                    rockingham_adm,
                    strafford_adm,
                    sullivan_adm)
dim(nh_adm_all)

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
                gender,
                booking_id,
                booking_date,
                charge_code,
                charge_desc,
                booking_type,
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
dim(nh_charges) # 73186

####################################################
# Booking type dataframe
####################################################

# remove charge codes and duplicates to get picture of cohort
# remove sentence status
# create month year variables
# there will not be one booking id per row because people can have multiple booking types per booking episode

nh_booking <- nh_adm_all %>%
  dplyr::select(county,
                id,
                race,
                yob,
                age,
                gender,
                booking_id,
                los = los_max,
                booking_date,
                booking_type,
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

# replace "NA" with actual NA ??????????????????????????????????
nh_booking <- nh_booking %>%
  mutate(pc_hold = ifelse(pc_hold == "NA", NA, pc_hold)) %>%
  mutate(pc_hold = case_when(pc_hold == 2 ~ "PC Hold",
                             pc_hold == 1 ~ "Non-PC Hold"))

# make all booking types uppercase to remove differences in case
nh_booking <- nh_booking %>%
  mutate(booking_type = toupper(booking_type))

dim(nh_booking)                       # 55828
length(unique(nh_booking$booking_id)) # 51581

# determine if PC hold happened in booking event
detach(package:plyr)
nh_booking <- nh_booking %>%
  dplyr::group_by(booking_id) %>%
  mutate(all_hold_types=paste(sort(unique(pc_hold)), collapse="&")) %>%
  mutate(pc_hold_in_booking = case_when(all_hold_types == 'Non-PC Hold&PC Hold' | all_hold_types == 'PC Hold' ~ "PC Hold Booking",
                                        all_hold_types == "Non-PC Hold" ~ "Non-PC Hold Booking")) %>%
  select(county:high_utilizer_5_pct, month_year_text:pc_hold_in_booking) %>%
  distinct()

dim(nh_booking)                       # 54821
length(unique(nh_booking$booking_id)) # 51581
dups <- nh_booking[duplicated(nh_booking$booking_id)|duplicated(nh_booking$booking_id, fromLast=TRUE),] # 5966

# there will not be one booking id per row because people can have multiple booking types per booking episode
# Belknap      Carroll     Cheshire         Coos Hillsborough    Merrimack   Rockingham    Strafford     Sullivan
# 4          925          459            0           10         1995         2334            2          237

# combine booking types by booking id
nh_booking <- nh_booking %>%
  group_by(booking_id) %>%
  mutate(all_booking_types=paste(sort(unique(booking_type)), collapse=" & ")) %>%
  select(county: booking_type, all_booking_types, everything()) %>%
  distinct()






# if race is unknown for one booking but is present in another, add the race
nh_booking <- nh_booking %>%
  mutate(race = case_when(id == #???????????????????????????







########################################################################################################
# PC hold data
########################################################################################################

nh_pch <- nh_booking %>%
  filter(county != "Coos" & county != "Strafford") %>%
  select(county, id, booking_id, pc_hold_in_booking) %>%
  distinct() %>%
  droplevels()

dim(nh_pch); length(unique(nh_pch$booking_id)) # 38677

########################################################################################################
# Counties in data
########################################################################################################

# get list of counties
counties <- nh_adm_all$county %>%
  unique() %>%
  sort()
