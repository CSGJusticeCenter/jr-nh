############################################
# Project: JRI New Hampshire
# File: dataframes.R
# Last updated: November 10, 2022
# Author: Mari Roberts

# Combine county data and create dataframes used in analyses and visualizations
############################################

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
adm_all <- adm_all %>%
  mutate(los_max = ifelse(los_max == -Inf, NA, los_max)) %>%
  filter(los_max >= 0 | is.na(los_max))

# Create los categories
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
                sentence_status_standard,
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
                sentence_status_standard,
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

# Save data

################################################################################

save(adm_all, file=paste0(sp_data_path, "/Data/r_data/data_dictionaries_page/adm_all.Rda", sep = ""))
