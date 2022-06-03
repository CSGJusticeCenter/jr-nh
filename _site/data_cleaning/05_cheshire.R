############################################
# Project: JRI New Hampshire
# File: cheshire.R
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
# Cheshire County
###################

# clean variable names
cheshire_adm_all <- clean_names(cheshire_adm.xlsx)

# create FY year variable
# will be able to filter by CY later
cheshire_adm_all <- cheshire_adm_all %>%
  dplyr::rename(charge_desc = charged_offense_code_description_including_technical_violations_of_supervision,
                housing = housing_instability_or_homelessness_indicator,
                charge_code = charge_offence_code,
                transfer_type = transfer_type_if_applicable) %>%
  mutate(fy = case_when(booking_date >= "2018-07-01" & booking_date <= "2019-06-30" ~ 2019,
                        booking_date >= "2019-07-01" & booking_date <= "2020-06-30" ~ 2020,
                        booking_date >= "2020-07-01" & booking_date <= "2021-06-30" ~ 2021),
         charge_desc = case_when(charge_desc == "PROTECTIVE CUSTODY" | charge_desc == "PROTECTIVE CUSTODY - DRUGS" ~ "PROTECTIVE CUSTODY", TRUE ~ charge_desc),
         age = fy - yob,
         los = difftime(as.POSIXct(release_date), as.POSIXct(booking_date, tz="UTC"), units="days"),
         race = case_when(race == "A" ~ "AAPI",
                          race == "B" ~ "Black",
                          race == "H" ~ "Hispanic or Latino",
                          race == "I" ~ "American Indian or Alaskan Native",
                          race == "L" ~ "Hispanic or Latino",
                          race == "P" ~ "AAPI",
                          race == "U" ~ "Unknown",
                          race == "W" ~ "White")
         ) %>%
  # remove booking outside of study timeframe
  filter(!is.na(fy))

# organize variables
cheshire_adm_all <- cheshire_adm_all %>%
  dplyr::select(id,
         inmate_id,
         yob,
         race,
         ethnicity,
         sex,
         housing,
         charge_code,
         charge_desc,
         #charge,
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
                booking_date, los, fy) %>%
  distinct()

# remove charge codes and duplicates to get picture of cohort
cheshire_booking_all <- cheshire_adm_all %>%
  dplyr::select(inmate_id, race, yob, age, sex,
                housing, booking_date, booking_type, los, fy) %>%
  distinct()

# sep by fiscal year
cheshire_adm_19 <- cheshire_adm %>% filter(fy == 2019)
cheshire_adm_20 <- cheshire_adm %>% filter(fy == 2020)
cheshire_adm_21 <- cheshire_adm %>% filter(fy == 2021)

# sep by fy year
cheshire_booking_19 <- cheshire_booking_all %>% filter(fy == 2019)
cheshire_booking_20 <- cheshire_booking_all %>% filter(fy == 2020)
cheshire_booking_21 <- cheshire_booking_all %>% filter(fy == 2021)

######
# Race
######

# custom function to create table
cheshire_race <- fnc_race_table(cheshire_adm_19, cheshire_adm_20, cheshire_adm_21)

######
# Sex
######

# custom function to create table
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

# custom function to create table
cheshire_booking <- fnc_booking_table(cheshire_booking_19, cheshire_booking_20, cheshire_booking_21)

######
# Sentence Statuses
######

# custom function to create table
cheshire_sentence <- fnc_sentence_table(cheshire_adm_19, cheshire_adm_20, cheshire_adm_21)

######
# Length of Stay
######

# custom function to create table
cheshire_los <- fnc_los_table(cheshire_adm_19, cheshire_adm_20, cheshire_adm_21)

##############################################################################
# High Utilizers - more than 2 bookings in a year?
##############################################################################

# custom function to create high utilizers dataframe
cheshire_high_utilizers_sentence <- fnc_hu_setup(cheshire_adm)
cheshire_high_utilizers_booking  <- fnc_hu_setup(cheshire_booking_all)

# sep by fiscal year
cheshire_high_utilizers_sentence_19 <- cheshire_high_utilizers_sentence %>% filter(fy == 2019)
cheshire_high_utilizers_sentence_20 <- cheshire_high_utilizers_sentence %>% filter(fy == 2020)
cheshire_high_utilizers_sentence_21 <- cheshire_high_utilizers_sentence %>% filter(fy == 2021)

# sep by fiscal year
cheshire_high_utilizers_booking_19 <- cheshire_high_utilizers_booking %>% filter(fy == 2019)
cheshire_high_utilizers_booking_20 <- cheshire_high_utilizers_booking %>% filter(fy == 2020)
cheshire_high_utilizers_booking_21 <- cheshire_high_utilizers_booking %>% filter(fy == 2021)

######
# Demographics of High Utilizers
######

# custom function to create table
cheshire_hu_race <- fnc_race_table(cheshire_high_utilizers_booking_19, cheshire_high_utilizers_booking_20, cheshire_high_utilizers_booking_21)
cheshire_hu_sex  <- fnc_sex_table(cheshire_high_utilizers_booking_19, cheshire_high_utilizers_booking_20, cheshire_high_utilizers_booking_21)

######
# Booking Types for High Utilizers
######

# custom function to create table
cheshire_hu_booking <- fnc_booking_table(cheshire_high_utilizers_booking_19, cheshire_high_utilizers_booking_20, cheshire_high_utilizers_booking_21)

######
# Sentence Statuses for High Utilizers
######

# custom function to create table
cheshire_hu_sentence <- fnc_sentence_table(cheshire_high_utilizers_sentence_19, cheshire_high_utilizers_sentence_20, cheshire_high_utilizers_sentence_21)

######
# Create data dictionary
######

# change data types
cheshire_adm_all$id <- as.factor(cheshire_adm_all$id)
cheshire_adm_all$inmate_id <- as.character(cheshire_adm_all$inmate_id)
cheshire_adm_all$yob <- as.numeric(cheshire_adm_all$yob)
cheshire_adm_all$race <- as.factor(cheshire_adm_all$race)
cheshire_adm_all$ethnicity <- as.factor(cheshire_adm_all$ethnicity)
cheshire_adm_all$sex <- as.factor(cheshire_adm_all$sex)
cheshire_adm_all$housing <- as.factor(cheshire_adm_all$housing)
cheshire_adm_all$charge_code <- as.factor(cheshire_adm_all$charge_code)
cheshire_adm_all$charge_desc <- as.factor(cheshire_adm_all$charge_desc)
cheshire_adm_all$booking_type <- as.factor(cheshire_adm_all$booking_type)
cheshire_adm_all$release_type <- as.factor(cheshire_adm_all$release_type)
cheshire_adm_all$sentence_status <- as.factor(cheshire_adm_all$sentence_status)
cheshire_adm_all$transfer_type <- as.factor(cheshire_adm_all$transfer_type)
cheshire_adm_all$fy <- as.factor(cheshire_adm_all$fy)
cheshire_adm_all$age <- as.numeric(cheshire_adm_all$age)
cheshire_adm_all$los <- as.numeric(cheshire_adm_all$los)

# data labels
var.labels <- c(id = "Unique ID",
                inmate_id = "Inmate ID",
                yob  = "Year of birth",
                race  = "Race",
                ethnicity  = "Ethnicity",
                sex = "Sex",
                housing = "Housing indicator",
                charge_code = "Charge code",
                charge_desc = "Charge description",
                booking_date = "Booking date",
                booking_type = "Booking type",
                release_date = "Release date",
                release_type = "Release type",
                sentence_status = "Sentence status",
                transfer_type = "Transfer type",
                fy = "Fiscal year",
                age = "Age (years)",
                los = "Length of stay (days)"
)

# add labels to data
cheshire_adm_all <- labelled::set_variable_labels(cheshire_adm_all, .labels = var.labels)

######
# Save data
######

# save data to sharepoint
save(cheshire_adm_all,     file=paste0(CSG_SP_PATH, "/Data/r_data/cheshire_adm_all.rds", sep = ""))
save(cheshire_adm,         file=paste0(CSG_SP_PATH, "/Data/r_data/cheshire_adm.rds", sep = ""))
save(cheshire_booking,     file=paste0(CSG_SP_PATH, "/Data/r_data/cheshire_booking.rds", sep = ""))
save(cheshire_sentence,    file=paste0(CSG_SP_PATH, "/Data/r_data/cheshire_sentence.rds", sep = ""))
save(cheshire_race,        file=paste0(CSG_SP_PATH, "/Data/r_data/cheshire_race.rds", sep = ""))
save(cheshire_sex,         file=paste0(CSG_SP_PATH, "/Data/r_data/cheshire_sex.rds", sep = ""))
save(cheshire_heatmap,     file=paste0(CSG_SP_PATH, "/Data/r_data/cheshire_heatmap.rds", sep = ""))
save(cheshire_hu_booking,  file=paste0(CSG_SP_PATH, "/Data/r_data/cheshire_hu_booking.rds", sep = ""))
save(cheshire_hu_sentence, file=paste0(CSG_SP_PATH, "/Data/r_data/cheshire_hu_sentence.rds", sep = ""))
save(cheshire_hu_race,     file=paste0(CSG_SP_PATH, "/Data/r_data/cheshire_hu_race.rds", sep = ""))
save(cheshire_hu_sex,      file=paste0(CSG_SP_PATH, "/Data/r_data/cheshire_hu_sex.rds", sep = ""))
