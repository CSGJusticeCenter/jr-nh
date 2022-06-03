############################################
# Project: JRI New Hampshire
# File: merrimack.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

# load packages and custom functions
setwd(L_PATH)
source("data_cleaning/00_library.R")
source("data_cleaning/01_functions.R")

###################
# Merrimack County
###################

# clean variable names
merrimack_adm_all <- clean_names(merrimack_adm.xlsx)

# create FY year variable
# will be able to filter by CY later
# WHAT DOES RACE X STAND FOR??????
merrimack_adm_all <- merrimack_adm_all %>%
  dplyr::rename(id = uniq_id,
                charge_desc = charges,
                housing = housing_instability,
                inmate_id = im_id,
                release_date = rel_date) %>%
  mutate(fy = case_when(booking_date >= "2018-07-01" & booking_date <= "2019-06-30" ~ 2019,
                        booking_date >= "2019-07-01" & booking_date <= "2020-06-30" ~ 2020,
                        booking_date >= "2020-07-01" & booking_date <= "2021-06-30" ~ 2021),
         age = fy - yob,
         los = difftime(as.POSIXct(release_date), as.POSIXct(booking_date, tz="UTC"), units="days"),
         race = case_when(race == "A" ~ "AAPI",
                          race == "B" ~ "Black",
                          race == "H" ~ "Hispanic or Latino",
                          race == "I" ~ "American Indian or Alaskan Native",
                          race == "O" ~ "Other",
                          race == "P" ~ "AAPI",
                          race == "U" ~ "Unknown",
                          race == "W" ~ "White",
                          race == 'X' ~ "Unknown")
         ) %>%
  # remove booking outside of study timeframe
  filter(!is.na(fy))

# organize variables
merrimack_adm_all <- merrimack_adm_all %>%
  dplyr::select(id,
                inmate_id,
                yob,
                race,
                sex,
                housing,
                charge_desc,
                booking_date,
                booking_type,
                release_date,
                sentence_status,
                everything())

# save long file that includes all charges
merrimack_adm_charges <- merrimack_adm_all

# remove charge codes and duplicates to get picture of cohort
merrimack_adm <- merrimack_adm_all %>%
  dplyr::select(inmate_id, race, yob, age, sex,
                housing, sentence_status,
                booking_date, fy) %>%
  distinct()

# remove charge codes and duplicates to get picture of cohort
merrimack_booking_all <- merrimack_adm_all %>%
  dplyr::select(inmate_id, race, yob, age, sex,
                housing, booking_date, booking_type, fy) %>%
  distinct()

# sep by fiscal year
merrimack_adm_19 <- merrimack_adm %>% filter(fy == 2019)
merrimack_adm_20 <- merrimack_adm %>% filter(fy == 2020)
merrimack_adm_21 <- merrimack_adm %>% filter(fy == 2021)

# sep by fy year
merrimack_booking_19 <- merrimack_booking_all %>% filter(fy == 2019)
merrimack_booking_20 <- merrimack_booking_all %>% filter(fy == 2020)
merrimack_booking_21 <- merrimack_booking_all %>% filter(fy == 2021)

######
# Race
######

# custom function to create table
merrimack_race <- fnc_race_table(merrimack_adm_19, merrimack_adm_20, merrimack_adm_21)

######
# Sex
######

# custom function to create table
merrimack_sex <- fnc_sex_table(merrimack_adm_19, merrimack_adm_20, merrimack_adm_21)

######
# Data for booking heatmap
######

# create data for heatmap showing the number of bookings by month and year
df <- merrimack_adm
df$ymd <- lubridate::ymd_hms(as.character(df$booking_date))
df$month <- lubridate::month(df$ymd, label = TRUE)
df$year <- lubridate::year(df$ymd)
df$wday <- lubridate::wday(df$ymd, label = TRUE)
df$hour <- lubridate::hour(df$ymd)

merrimack_heatmap <- ddply(df, c("year", "month"), summarise, N = length(ymd))

#reverse order of months for easier graphing
merrimack_heatmap$month <- factor(merrimack_heatmap$month, levels=rev(levels(merrimack_heatmap$month)))

######
# Booking Types
######

# custom function to create table
merrimack_booking <- fnc_booking_table(merrimack_booking_19, merrimack_booking_20, merrimack_booking_21)

######
# Sentence Statuses
######

# custom function to create table
merrimack_sentence <- fnc_sentence_table(merrimack_adm_19, merrimack_adm_20, merrimack_adm_21)

######
# Length of Stay
######

# custom function to create table
merrimack_los <- fnc_los_table(merrimack_adm_19, merrimack_adm_20, merrimack_adm_21)

##############################################################################
# High Utilizers - more than 2 bookings in a year?
##############################################################################

# custom function to create high utilizers dataframe
merrimack_high_utilizers_sentence <- fnc_hu_setup(merrimack_adm)
merrimack_high_utilizers_booking  <- fnc_hu_setup(merrimack_booking_all)

# sep by fiscal year
merrimack_high_utilizers_sentence_19 <- merrimack_high_utilizers_sentence %>% filter(fy == 2019)
merrimack_high_utilizers_sentence_20 <- merrimack_high_utilizers_sentence %>% filter(fy == 2020)
merrimack_high_utilizers_sentence_21 <- merrimack_high_utilizers_sentence %>% filter(fy == 2021)

# sep by fiscal year
merrimack_high_utilizers_booking_19 <- merrimack_high_utilizers_booking %>% filter(fy == 2019)
merrimack_high_utilizers_booking_20 <- merrimack_high_utilizers_booking %>% filter(fy == 2020)
merrimack_high_utilizers_booking_21 <- merrimack_high_utilizers_booking %>% filter(fy == 2021)

######
# Demographics of High Utilizers
######

# custom function to create table
merrimack_hu_race <- fnc_race_table(merrimack_high_utilizers_booking_19, merrimack_high_utilizers_booking_20, merrimack_high_utilizers_booking_21)
merrimack_hu_sex  <- fnc_sex_table(merrimack_high_utilizers_booking_19, merrimack_high_utilizers_booking_20, merrimack_high_utilizers_booking_21)

######
# Booking Types for High Utilizers
######

# custom function to create table
merrimack_hu_booking <- fnc_booking_table(merrimack_high_utilizers_booking_19, merrimack_high_utilizers_booking_20, merrimack_high_utilizers_booking_21)

######
# Sentence Statuses for High Utilizers
######

# custom function to create table
merrimack_hu_sentence <- fnc_sentence_table(merrimack_high_utilizers_sentence_19, merrimack_high_utilizers_sentence_20, merrimack_high_utilizers_sentence_21)

######
# Create data dictionary
######

# change data types
merrimack_adm_all$id <- as.factor(merrimack_adm_all$id)
merrimack_adm_all$inmate_id <- as.character(merrimack_adm_all$inmate_id)
merrimack_adm_all$yob <- as.numeric(merrimack_adm_all$yob)
merrimack_adm_all$race <- as.factor(merrimack_adm_all$race)
merrimack_adm_all$sex <- as.factor(merrimack_adm_all$sex)
merrimack_adm_all$housing <- as.factor(merrimack_adm_all$housing)
merrimack_adm_all$charge_desc <- as.factor(merrimack_adm_all$charge_desc)
merrimack_adm_all$booking_type <- as.factor(merrimack_adm_all$booking_type)
merrimack_adm_all$sentence_status <- as.factor(merrimack_adm_all$sentence_status)
merrimack_adm_all$fy <- as.factor(merrimack_adm_all$fy)
merrimack_adm_all$age <- as.numeric(merrimack_adm_all$age)
merrimack_adm_all$los <- as.numeric(merrimack_adm_all$los)

# data labels
var.labels <- c(id = "Unique ID",
                inmate_id = "Inmate ID",
                yob  = "Year of birth",
                race  = "Race",
                sex = "Sex",
                housing = "Housing indicator",
                charge_desc = "Charge description",
                booking_date = "Booking date",
                booking_type = "Booking type",
                release_date = "Release date",
                sentence_status = "Sentence status",
                fy = "Fiscal year",
                age = "Age (years)",
                los = "Length of stay (days)"
)

# add labels to data
merrimack_adm_all <- labelled::set_variable_labels(merrimack_adm_all, .labels = var.labels)

######
# Save data
######

# save data to sharepoint
save(merrimack_adm_all,     file=paste0(CSG_SP_PATH, "/Data/r_data/merrimack_adm_all.rds", sep = ""))
save(merrimack_adm,         file=paste0(CSG_SP_PATH, "/Data/r_data/merrimack_adm.rds", sep = ""))
save(merrimack_booking,     file=paste0(CSG_SP_PATH, "/Data/r_data/merrimack_booking.rds", sep = ""))
save(merrimack_sentence,    file=paste0(CSG_SP_PATH, "/Data/r_data/merrimack_sentence.rds", sep = ""))
save(merrimack_race,        file=paste0(CSG_SP_PATH, "/Data/r_data/merrimack_race.rds", sep = ""))
save(merrimack_sex,         file=paste0(CSG_SP_PATH, "/Data/r_data/merrimack_sex.rds", sep = ""))
save(merrimack_heatmap,     file=paste0(CSG_SP_PATH, "/Data/r_data/merrimack_heatmap.rds", sep = ""))
save(merrimack_hu_booking,  file=paste0(CSG_SP_PATH, "/Data/r_data/merrimack_hu_booking.rds", sep = ""))
save(merrimack_hu_sentence, file=paste0(CSG_SP_PATH, "/Data/r_data/merrimack_hu_sentence.rds", sep = ""))
save(merrimack_hu_race,     file=paste0(CSG_SP_PATH, "/Data/r_data/merrimack_hu_race.rds", sep = ""))
save(merrimack_hu_sex,      file=paste0(CSG_SP_PATH, "/Data/r_data/merrimack_hu_sex.rds", sep = ""))

