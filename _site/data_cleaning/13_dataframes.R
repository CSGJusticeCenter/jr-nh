############################################
# Project: JRI New Hampshire
# File: dataframes.R
# Last updated: November 10, 2022
# Author: Mari Roberts

# Combine county data and create dataframes used in analyses and visualizations
############################################

source("data_cleaning/03_belknap.R")
source("data_cleaning/04_carroll.R")
source("data_cleaning/05_cheshire.R")
source("data_cleaning/06_coos.R")
source("data_cleaning/07_hillsborough.R")
source("data_cleaning/08_merrimack.R")
source("data_cleaning/09_rockingham.R")
source("data_cleaning/10_strafford.R")
source("data_cleaning/11_sullivan.R")
source("data_cleaning/12_charges.R")

# Combine jail data
adm_all <- plyr::rbind.fill(belknap_adm_charge_clean_final,
                            carroll_adm_charge_clean_final,
                            cheshire_adm_charge_clean_final,
                            coos_adm_charge_clean_final,
                            hillsborough_adm_charge_clean_final,
                            merrimack_adm_charge_clean_final,
                            rockingham_adm_charge_clean_final,
                            strafford_adm, # no charge file
                            sullivan_adm_charge_clean_final)
dim(adm_all); length(unique(adm_all$booking_id)); length(unique(adm_all$id)) # 51555 dim, 51545 bookings, 32177 individuals

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
                sentence_status_standard,
                fy,
                num_entrances,
                num_entrances_fy,
                high_utilizer_4_times,
                high_utilizer_1_pct,
                high_utilizer_5_pct,
                high_utilizer_10_pct,
                pc_hold) %>%
  mutate(month_year_text = format(as.Date(booking_date, "%d/%m/%Y"), "%b %Y"),
         month_year      = as.Date(as.yearmon(month_year_text))) %>%
  distinct()

# People can have multiple booking types when entering jail (some are PC hold + criminal charge)
# dups <- bookings_entrances_all[duplicated(bookings_entrances_all$booking_id)|duplicated(bookings_entrances_all$booking_id, fromLast=TRUE),] # 7932

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
  mutate(all_sentence_statuses=paste(sort(unique(sentence_status_standard)), collapse=" & ")) %>%
  select(county:sentence_status_standard, all_sentence_statuses, everything()) %>%
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

# Save data to sharepoint. Not Medicaid data.

################################################################################

save(adm_all,            file=paste0(sp_data_path, "/Data/r_data/adm_all.Rda",            sep = ""))
save(bookings_entrances, file=paste0(sp_data_path, "/Data/r_data/bookings_entrances.Rda", sep = ""))
save(entrances,          file=paste0(sp_data_path, "/Data/r_data/entrances.Rda",          sep = ""))
save(booking_no_pc_hold, file=paste0(sp_data_path, "/Data/r_data/booking_no_pc_hold.Rda", sep = ""))
save(df_pch,             file=paste0(sp_data_path, "/Data/r_data/df_pch.Rda",             sep = ""))
