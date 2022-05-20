############################################
# Format Cheshire County data
# Last updated: May 12, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

source("data_cleaning/00_library.R")
source("data_cleaning/01_functions.R")

###################
# Cheshire County
###################

# clean variable names
cheshire_adm_all <- clean_names(cheshire_adm.xlsx)

# create FY year variable
# will be able to filter by CY later
cheshire_adm_all <- cheshire_adm_all %>%
  dplyr::rename(charge_desc = charged_offense_code_description_including_technical_violations_of_supervision,
                housing = housing_instability_or_homelessness_indicator,
                charge_offense_code = charge_offence_code) %>%
  mutate(fy = case_when(booking_date >= "2018-07-01" & booking_date <= "2019-06-30" ~ 2019,
                        booking_date >= "2019-07-01" & booking_date <= "2020-06-30" ~ 2020,
                        booking_date >= "2020-07-01" & booking_date <= "2021-06-30" ~ 2021),
         charge = case_when(charge_desc == "PROTECTIVE CUSTODY" | charge_desc == "PROTECTIVE CUSTODY - DRUGS" ~ "PROTECTIVE CUSTODY", TRUE ~ charge_desc),
         age = fy - yob,
         los = difftime(as.POSIXct(release_date), as.POSIXct(booking_date, tz="UTC"), units="days"),
         race = case_when(race == "A" ~ "Asian or Pacific Islander",
                          race == "B" ~ "Black",
                          race == "H" ~ "Hispanic or Latino",
                          race == "I" ~ "American Indian or Alaskan Native",
                          race == "L" ~ "Hispanic or Latino",
                          race == "P" ~ "Asian or Pacific Islander",
                          race == "U" ~ "Unknown",
                          race == "W" ~ "White")) %>%
  # remove booking outside of study timeframe
  filter(!is.na(fy))

# organize variables
cheshire_adm_all <- cheshire_adm_all %>%
  dplyr::select(id,
         inmate_id,
         yob,
         race,
         sex,
         housing,
         charge_offense_code,
         charge_desc,
         charge,
         booking_date,
         booking_type,
         release_date,
         release_type,
         sentence_status,
         everything())

# save long file that includes all charges
cheshire_adm_charges <- cheshire_adm_all

# remove charge codes and duplicates to get picture of cohort
cheshire_adm <- cheshire_adm_all %>%
  dplyr::select(inmate_id, race, yob, age, sex,
                housing, sentence_status,
                booking_date, fy) %>%
  distinct()

# remove charge codes and duplicates to get picture of cohort
cheshire_booking <- cheshire_adm_all %>%
  dplyr::select(inmate_id, race, yob, age, sex,
                housing, booking_date, booking_type, fy) %>%
  distinct()

# sep by fiscal year
cheshire_adm_19 <- cheshire_adm %>% filter(fy == 2019)
cheshire_adm_20 <- cheshire_adm %>% filter(fy == 2020)
cheshire_adm_21 <- cheshire_adm %>% filter(fy == 2021)

# sep by fy year
cheshire_booking_19 <- cheshire_booking %>% filter(fy == 2019)
cheshire_booking_20 <- cheshire_booking %>% filter(fy == 2020)
cheshire_booking_21 <- cheshire_booking %>% filter(fy == 2021)

######
# Race
######

# custom function to create race table
cheshire_race <- fnc_race_table(cheshire_adm_19, cheshire_adm_20, cheshire_adm_21)

######
# Sex
######

# custom function to create sex table
cheshire_sex <- fnc_sex_table(cheshire_adm_19, cheshire_adm_20, cheshire_adm_21)

######
# Data for booking heatmap
######

# create data for heatmap showing the number of bookings by month and year
df <- cheshire_adm
df$ymd <- lubridate::ymd_hms(as.character(df$booking_date))
df$month <- lubridate::month(df$ymd, label = TRUE)
df$year <- lubridate::year(df$ymd)
df$wday <- lubridate::wday(df$ymd, label = TRUE)
df$hour <- lubridate::hour(df$ymd)

cheshire_heatmap <- ddply(df, c("year", "month"), summarise, N = length(ymd))

#reverse order of months for easier graphing
cheshire_heatmap$month <- factor(cheshire_heatmap$month, levels=rev(levels(cheshire_heatmap$month)))

######
# Booking Types
######

# custom function to create booking type table
cheshire_booking <- fnc_booking_table(cheshire_booking_19, cheshire_booking_20, cheshire_booking_21)

######
# Sentence Statuses
######

# custom function to create sentence status table
cheshire_sentence <- fnc_sentence_table(cheshire_adm_19, cheshire_adm_20, cheshire_adm_21)

##############################################################################
# High Utilizers - more than 2 bookings in a year?
##############################################################################

# custom function to create high utilizers dataframe
cheshire_high_utilizers <- fnc_hu_setup(cheshire_adm)

# sep by fiscal year
cheshire_high_utilizers_19 <- cheshire_high_utilizers %>% filter(fy == 2019)
cheshire_high_utilizers_20 <- cheshire_high_utilizers %>% filter(fy == 2020)
cheshire_high_utilizers_21 <- cheshire_high_utilizers %>% filter(fy == 2021)

######
# Booking Types for High Utilizers
######

# custom function to create booking type table of high utilizers
# cheshire_hu_booking <- fnc_booking_table(cheshire_high_utilizers_19, cheshire_high_utilizers_20, cheshire_high_utilizers_21)

######
# Sentence Types for High Utilizers
######

# custom function to create sentence status table of high utilizers
cheshire_hu_sentence <- fnc_sentence_table(cheshire_high_utilizers_19, cheshire_high_utilizers_20, cheshire_high_utilizers_21)

######
# Save data
######

# save R data to load in Rmd files
save(cheshire_adm,            file="C:/Users/mroberts/The Council of State Governments/JC Research - JR_NH/Data/Cheshire County/Converted Data/cheshire_adm.Rda")
save(cheshire_booking,        file="C:/Users/mroberts/The Council of State Governments/JC Research - JR_NH/Data/Cheshire County/Converted Data/cheshire_booking.Rda")
save(cheshire_sentence,       file="C:/Users/mroberts/The Council of State Governments/JC Research - JR_NH/Data/Cheshire County/Converted Data/cheshire_sentence.Rda")
save(cheshire_race,           file="C:/Users/mroberts/The Council of State Governments/JC Research - JR_NH/Data/Cheshire County/Converted Data/cheshire_race.Rda")
save(cheshire_sex,            file="C:/Users/mroberts/The Council of State Governments/JC Research - JR_NH/Data/Cheshire County/Converted Data/cheshire_sex.Rda")
save(cheshire_heatmap,        file="C:/Users/mroberts/The Council of State Governments/JC Research - JR_NH/Data/Cheshire County/Converted Data/cheshire_heatmap.Rda")
# save(cheshire_hu_booking,     file="C:/Users/mroberts/The Council of State Governments/JC Research - JR_NH/Data/Cheshire County/Converted Data/cheshire_hu_booking.Rda")
save(cheshire_hu_sentence,    file="C:/Users/mroberts/The Council of State Governments/JC Research - JR_NH/Data/Cheshire County/Converted Data/cheshire_hu_sentence.Rda")
