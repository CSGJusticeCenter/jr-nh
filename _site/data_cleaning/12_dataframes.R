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

# custom function that creates the variables we need and relabels codes so they're consistent across counties
belknap_adm      <- fnc_standardize_counties(belknap_adm_all,      "Belknap")
carroll_adm      <- fnc_standardize_counties(carroll_adm_all,      "Carroll")
cheshire_adm     <- fnc_standardize_counties(cheshire_adm_all,     "Cheshire")
coos_adm         <- fnc_standardize_counties(coos_adm_all,         "Coos")
hillsborough_adm <- fnc_standardize_counties(hillsborough_adm_all, "Hillsborough")
merrimack_adm    <- fnc_standardize_counties(merrimack_adm_all,    "Merrimack")
rockingham_adm   <- fnc_standardize_counties(rockingham_adm_all,   "Rockingham")
sullivan_adm     <- fnc_standardize_counties(sullivan_adm_all,     "Sullivan")

# save data to SP
save(belknap_adm,    file=paste0(sp_data_path, "/Data/r_data/belknap_adm.Rda",    sep = ""))
save(carroll_adm,    file=paste0(sp_data_path, "/Data/r_data/carroll_adm.Rda",    sep = ""))
save(cheshire_adm,   file=paste0(sp_data_path, "/Data/r_data/cheshire_adm.Rda",   sep = ""))
save(coos_adm,       file=paste0(sp_data_path, "/Data/r_data/coos_adm.Rda",       sep = ""))
save(merrimack_adm,  file=paste0(sp_data_path, "/Data/r_data/merrimack_adm.Rda",  sep = ""))
save(rockingham_adm, file=paste0(sp_data_path, "/Data/r_data/rockingham_adm.Rda", sep = ""))
save(sullivan_adm,   file=paste0(sp_data_path, "/Data/r_data/sullivan_adm.Rda",   sep = ""))

######
# STATE-WIDE DATA
# combine county data or large NH dataframe with all charge descriptions
# missing strafford, hillsborough for now
######

nh_adm_all <- rbind(belknap_adm,
                    carroll_adm,
                    cheshire_adm,
                    coos_adm,
                    hillsborough_adm,
                    merrimack_adm,
                    rockingham_adm,
                    #strafford_adm,
                    sullivan_adm
                    )

####################################################
# Charges dataframe
####################################################

# remove charge codes and duplicates
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
dim(nh_charges) # 60121

####################################################
# Booking type dataframe
####################################################

# remove charge codes and duplicates to get picture of cohort
# remove sentence status
# create month year variables
nh_booking <- nh_adm_all %>%
  dplyr::select(county,
                id,
                race,
                yob,
                age,
                gender,
                booking_id,
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
dim(nh_booking) # 43593

# replace "NA" with actual NA ??????????????????????????????????
nh_booking <- nh_booking %>%
  mutate(pc_hold = ifelse(pc_hold == "NA", NA, pc_hold)) %>%
  mutate(pc_hold = case_when(pc_hold == 2 ~ "PC Hold",
                             pc_hold == 1 ~ "Non-PC Hold"))

# make all booking types uppercase to remove differences in case
nh_booking <- nh_booking %>%
  mutate(booking_type = toupper(booking_type))

########################################################################################################
# Overall NH dataframes separated by fiscal year
########################################################################################################

# sep by fiscal year
nh_charges_19 <- nh_charges %>% filter(fy == 2019)
nh_charges_20 <- nh_charges %>% filter(fy == 2020)
nh_charges_21 <- nh_charges %>% filter(fy == 2021)

# # sep by fy year
# nh_booking_19 <- nh_booking %>% filter(fy == 2019)
# nh_booking_20 <- nh_booking %>% filter(fy == 2020)
# nh_booking_21 <- nh_booking %>% filter(fy == 2021)

# get list of counties
counties <- nh_adm_all$county %>%
  unique() %>%
  sort()
