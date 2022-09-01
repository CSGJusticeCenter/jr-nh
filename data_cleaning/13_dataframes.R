############################################
# Project: JRI New Hampshire
# File: dataframes.R
# Last updated: August 31, 2022
# Author: Mari Roberts

# Generate tables for state overview

# Creates:
# nh_adm_all
# nh_sentence, nh_sentence_19, nh_sentence_20, nh_sentence_21
# nh_booking, nh_booking_19, nh_booking_20, nh_booking_21
############################################

######
# Standardize and save data
######

# custom function that creates the variables we need and relabels codes so they're consistent across counties
belknap_adm    <- fnc_standardize_counties(belknap_adm_all)
carroll_adm    <- fnc_standardize_counties(carroll_adm_all)
cheshire_adm   <- fnc_standardize_counties(cheshire_adm_all)
coos_adm       <- fnc_standardize_counties(coos_adm_all)
merrimack_adm  <- fnc_standardize_counties(merrimack_adm_all)
rockingham_adm <- fnc_standardize_counties(rockingham_adm_all)
sullivan_adm   <- fnc_standardize_counties(sullivan_adm_all)

# save data to SP
save(belknap_adm,    file=paste0(sp_data_path, "/Data/r_data/belknap_adm_all.Rda",    sep = ""))
save(carroll_adm,    file=paste0(sp_data_path, "/Data/r_data/carroll_adm_all.Rda",    sep = ""))
save(cheshire_adm,   file=paste0(sp_data_path, "/Data/r_data/cheshire_adm_all.Rda",   sep = ""))
save(coos_adm,       file=paste0(sp_data_path, "/Data/r_data/coos_adm_all.Rda",       sep = ""))
save(merrimack_adm,  file=paste0(sp_data_path, "/Data/r_data/merrimack_adm_all.Rda",  sep = ""))
save(rockingham_adm, file=paste0(sp_data_path, "/Data/r_data/rockingham_adm_all.Rda", sep = ""))
save(sullivan_adm,   file=paste0(sp_data_path, "/Data/r_data/sullivan_adm_all.Rda",   sep = ""))

######
# STATE-WIDE DATA
# combine county data or large NH dataframe with all charge descriptions
# missing strafford, hillsborough for now
######

nh_adm_all <- do.call("rbind", list(belknap_adm_all,
                                    carroll_adm_all,
                                    cheshire_adm_all,
                                    coos_adm_all,
                                    #hillsborough_adm_all,
                                    merrimack_adm_all,
                                    rockingham_adm_all,
                                    #strafford_adm_all,
                                    sullivan_adm_all
                                    ))

####################################################
# Sentence status dataframe
####################################################

# remove charge codes and duplicates
# keep sentence status
nh_sentence <- nh_adm_all %>%
  dplyr::select(inmate_id,
                race = race_label,
                yob,
                age,
                gender = sex,
                sentence_status,
                booking_date,
                fy,
                num_bookings,
                high_utilizer,
                pc_hold,
                county) %>%
  distinct()
dim(nh_sentence)

####################################################
# Booking type dataframe
####################################################

# remove charge codes and duplicates to get picture of cohort
# remove sentence status
# create month year variables
nh_booking <- nh_adm_all %>%
  dplyr::select(inmate_id,
                race_code,
                race = race_label,
                yob,
                age,
                gender = sex,
                booking_date,
                booking_type,
                fy,
                num_bookings,
                high_utilizer,
                pc_hold,
                county) %>%
  mutate(month_year_text = format(as.Date(booking_date, "%d/%m/%Y"), "%b %Y"),
         month_year      = as.Date(as.yearmon(month_year_text))) %>%
  distinct()
dim(nh_booking)

# make all booking types uppercase to remove differences in case
nh_booking <- nh_booking %>%
  mutate(booking_type = toupper(booking_type))

########################################################################################################
# Overall NH dataframes separated by fiscal year
########################################################################################################

# sep by fiscal year
nh_sentence_19 <- nh_sentence %>% filter(fy == 2019)
nh_sentence_20 <- nh_sentence %>% filter(fy == 2020)
nh_sentence_21 <- nh_sentence %>% filter(fy == 2021)

# sep by fy year
nh_booking_19 <- nh_booking %>% filter(fy == 2019)
nh_booking_20 <- nh_booking %>% filter(fy == 2020)
nh_booking_21 <- nh_booking %>% filter(fy == 2021)

# get list of counties
counties <- nh_adm_all$county %>%
  unique() %>%
  sort()
