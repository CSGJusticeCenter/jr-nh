############################################
# Project: JRI New Hampshire
# File: belknap.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

# load packages and custom functions
source("data_cleaning/00_library.R")
source("data_cleaning/01_functions.R")

###################
# Belknap County
###################

# clean variable names
belknap_adm_all <- clean_names(belknap_adm.xlsx)

# rename variables for consistency
belknap_adm_all <- belknap_adm_all %>%
  mutate(charge_code = NA) %>%
  dplyr::select(id = unique_person_id,
                inmate_id,
                yob = year_of_birth,
                race,
                sex,
                housing = housing_instability_or_homelessness_indicator,
                charge_code,
                charge_desc = charged_offense_code_description_including_technical_violations_of_supervision,
                booking_date,
                booking_type,
                release_date,
                release_type,
                sentence_status = sentencing_status)

# change date formats
belknap_adm_all$booking_date <- as.Date(belknap_adm_all$booking_date, format = "%m/%d/%Y")
belknap_adm_all$release_date <- as.Date(belknap_adm_all$release_date, format = "%m/%d/%Y")

# create FY year variable
# will be able to filter by CY later
belknap_adm_all <- belknap_adm_all %>%
  mutate(fy = case_when(booking_date >= "2018-07-01" & booking_date <= "2019-06-30" ~ 2019,
                        booking_date >= "2019-07-01" & booking_date <= "2020-06-30" ~ 2020,
                        booking_date >= "2020-07-01" & booking_date <= "2021-06-30" ~ 2021),
         age = fy - yob,
         los = difftime(as.POSIXct(release_date), as.POSIXct(booking_date, tz="UTC"), units="days"),
         race = case_when(race == "A"  ~ "AAPI",
                          race == "B"  ~ "Black",
                          race == "C"  ~ "AAPI",
                          race == "H"  ~ "Hispanic or Latino",
                          race == "I"  ~ "AAPI",
                          race == "NH" ~ "Unknown",
                          race == "O"  ~ "Other",
                          race == "P"  ~ "Other",
                          race == "U"  ~ "Unknown",
                          race == "W"  ~ "White",
                          race == "X"  ~ "Unknown")
  ) %>%
  # remove booking outside of study timeframe
  filter(!is.na(fy))

# organize variables
belknap_adm_all <- belknap_adm_all %>%
  dplyr::select(id,
                inmate_id,
                yob,
                race,
                sex,
                housing,
                charge_code,
                charge_desc,
                booking_date,
                booking_type,
                release_date,
                release_type,
                sentence_status,
                everything())

# create high utilizer variable
belknap_bookings <- fnc_create_hu_variable(belknap_adm_all)

# merge data back
belknap_adm_all <- left_join(belknap_adm_all, belknap_bookings, by = c("inmate_id", "fy"))

# create a PC hold variable
belknap_adm_all <- belknap_adm_all %>%
  mutate(pc_hold = ifelse(charge_desc == "PROTECTIVE CUSTODY" | charge_desc == "PROTECTIVE CUSTODY/INTOXICATION", 1, 0))

# remove charge codes and duplicates to get picture of cohort
# keep sentence status - more rows for each charge and sentence status
belknap_adm <- belknap_adm_all %>%
  dplyr::select(inmate_id,
                race,
                yob,
                age,
                sex,
                housing,
                sentence_status,
                booking_date,
                los,
                fy,
                num_bookings,
                high_utilizer,
                pc_hold) %>%
  distinct()

# remove charge codes and duplicates to get picture of cohort
# remove sentence status - less rows because each row is a booking event
belknap_booking <- belknap_adm_all %>%
  dplyr::select(inmate_id,
                race,
                yob,
                age,
                sex,
                housing,
                booking_date,
                booking_type,
                release_date,
                los,
                fy,
                num_bookings,
                high_utilizer,
                pc_hold) %>%
  distinct()

# sep by fiscal year
belknap_adm_19 <- belknap_adm %>% filter(fy == 2019)
belknap_adm_20 <- belknap_adm %>% filter(fy == 2020)
belknap_adm_21 <- belknap_adm %>% filter(fy == 2021)

# sep by fy year
belknap_booking_19 <- belknap_booking %>% filter(fy == 2019)
belknap_booking_20 <- belknap_booking %>% filter(fy == 2020)
belknap_booking_21 <- belknap_booking %>% filter(fy == 2021)

######
# High Utilizer proportion
######

# custom function to create table
belknap_hu <- fnc_hu_table(belknap_adm_19, belknap_adm_20, belknap_adm_21)

######
# Race
######

# custom function to create table
belknap_race <- fnc_race_table(belknap_adm_19, belknap_adm_20, belknap_adm_21)

######
# Sex
######

# custom function to create table
belknap_sex <- fnc_sex_table(belknap_adm_19, belknap_adm_20, belknap_adm_21)

######
# Data for booking heatmap
######

# create data for heatmap showing the number of bookings by month and year
df <- belknap_adm
df$ymd <- lubridate::ymd(as.character(df$booking_date))
df$month <- lubridate::month(df$ymd, label = TRUE)
df$year <- lubridate::year(df$ymd)
df$wday <- lubridate::wday(df$ymd, label = TRUE)
df$hour <- lubridate::hour(df$ymd)

belknap_heatmap <- ddply(df, c("year", "month"), summarise, N = length(ymd))

#reverse order of months for easier graphing
belknap_heatmap$month <- factor(belknap_heatmap$month, levels=rev(levels(belknap_heatmap$month)))

######
# Booking Types
######

# custom function to create table
belknap_bookings <- fnc_booking_table(belknap_booking_19, belknap_booking_20, belknap_booking_21)

######
# Sentence Statuses
######

# custom function to create table
belknap_sentence <- fnc_sentence_table(belknap_adm_19, belknap_adm_20, belknap_adm_21)

######
# Length of Stay
######

# # custom function to create table
# belknap_los <- fnc_los_table(belknap_adm_19, belknap_adm_20, belknap_adm_21)

##############################################################################
# High Utilizers - more than 3 bookings in a year?
##############################################################################

# custom function to create high utilizers dataframe
belknap_high_utilizers_sentence <- fnc_hu_setup(belknap_adm)
belknap_high_utilizers_booking  <- fnc_hu_setup(belknap_booking)

# sep by fiscal year
belknap_high_utilizers_sentence_19 <- belknap_high_utilizers_sentence %>% filter(fy == 2019)
belknap_high_utilizers_sentence_20 <- belknap_high_utilizers_sentence %>% filter(fy == 2020)
belknap_high_utilizers_sentence_21 <- belknap_high_utilizers_sentence %>% filter(fy == 2021)

# sep by fiscal year
belknap_high_utilizers_booking_19 <- belknap_high_utilizers_booking %>% filter(fy == 2019)
belknap_high_utilizers_booking_20 <- belknap_high_utilizers_booking %>% filter(fy == 2020)
belknap_high_utilizers_booking_21 <- belknap_high_utilizers_booking %>% filter(fy == 2021)

######
# Demographics of High Utilizers
######

# custom function to create table
belknap_hu_race <- fnc_race_table(belknap_high_utilizers_booking_19, belknap_high_utilizers_booking_20, belknap_high_utilizers_booking_21)
belknap_hu_sex  <- fnc_sex_table(belknap_high_utilizers_booking_19, belknap_high_utilizers_booking_20, belknap_high_utilizers_booking_21)

######
# Booking Types for High Utilizers
######

# custom function to create table
belknap_hu_booking <- fnc_booking_table(belknap_high_utilizers_booking_19, belknap_high_utilizers_booking_20, belknap_high_utilizers_booking_21)

######
# Sentence Statuses for High Utilizers
######

# custom function to create table
belknap_hu_sentence <- fnc_sentence_table(belknap_high_utilizers_sentence_19, belknap_high_utilizers_sentence_20, belknap_high_utilizers_sentence_21)

######
# Create data dictionary
######

# change data types
belknap_adm_all$id              <- as.factor(belknap_adm_all$id)
belknap_adm_all$inmate_id       <- as.character(belknap_adm_all$inmate_id)
belknap_adm_all$yob             <- as.numeric(belknap_adm_all$yob)
belknap_adm_all$race            <- as.factor(belknap_adm_all$race)
belknap_adm_all$sex             <- as.factor(belknap_adm_all$sex)
belknap_adm_all$housing         <- as.factor(belknap_adm_all$housing)
belknap_adm_all$charge_desc     <- as.factor(belknap_adm_all$charge_desc)
belknap_adm_all$booking_type    <- as.factor(belknap_adm_all$booking_type)
belknap_adm_all$release_type    <- as.factor(belknap_adm_all$release_type)
belknap_adm_all$sentence_status <- as.factor(belknap_adm_all$sentence_status)
belknap_adm_all$fy              <- as.factor(belknap_adm_all$fy)
belknap_adm_all$high_utilizer   <- as.factor(belknap_adm_all$high_utilizer)
belknap_adm_all$pc_hold         <- as.factor(belknap_adm_all$pc_hold)
belknap_adm_all$age             <- as.numeric(belknap_adm_all$age)
belknap_adm_all$los             <- as.numeric(belknap_adm_all$los)

# data labels
var.labels <- c(id              = "Unique ID",
                inmate_id       = "Inmate ID",
                yob             = "Year of birth",
                race            = "Race",
                sex             = "Sex",
                housing         = "Housing indicator",
                charge_code     = "Charge code",
                charge_desc     = "Charge description",
                booking_date    = "Booking date",
                booking_type    = "Booking type",
                release_date    = "Release date",
                release_type    = "Release type",
                sentence_status = "Sentence status",
                fy              = "Fiscal year",
                age             = "Age (years)",
                los             = "Length of stay (days)",
                num_bookings    = "Number of booking events in the fiscal year",
                high_utilizer   = "Is a high utilizer",
                pc_hold         = "Protective custody hold"
)

# add labels to data
belknap_adm_all <- labelled::set_variable_labels(belknap_adm_all, .labels = var.labels)

##############################################################################
# Highcharts
##############################################################################

######
# PC holds over time
######

# create month year variables
belknap_booking$month_year_text <- format(as.Date(belknap_booking$booking_date, "%d/%m/%Y"), "%b %Y")
belknap_booking$month_year      <- as.Date(as.yearmon(belknap_booking$month_year_text))

# custom function to generate a highchart showing pc hold bookings over time (month and year)
belknap_pch_time_highchart <- fnc_pch_time_highchart(belknap_booking)

######
# Save data
######

# save data to sharepoint
save(belknap_adm_all,      file=paste0(sp_data_path, "/Data/r_data/belknap_adm_all.rds", sep = ""))
save(belknap_adm,          file=paste0(sp_data_path, "/Data/r_data/belknap_adm.rds", sep = ""))
save(belknap_bookings,     file=paste0(sp_data_path, "/Data/r_data/belknap_bookings.rds", sep = ""))
save(belknap_sentence,     file=paste0(sp_data_path, "/Data/r_data/belknap_sentence.rds", sep = ""))
save(belknap_hu,           file=paste0(sp_data_path, "/Data/r_data/belknap_hu.rds", sep = ""))
save(belknap_race,         file=paste0(sp_data_path, "/Data/r_data/belknap_race.rds", sep = ""))
save(belknap_sex,          file=paste0(sp_data_path, "/Data/r_data/belknap_sex.rds", sep = ""))
save(belknap_heatmap,      file=paste0(sp_data_path, "/Data/r_data/belknap_heatmap.rds", sep = ""))
save(belknap_hu_booking,   file=paste0(sp_data_path, "/Data/r_data/belknap_hu_booking.rds", sep = ""))
save(belknap_hu_sentence,  file=paste0(sp_data_path, "/Data/r_data/belknap_hu_sentence.rds", sep = ""))
save(belknap_hu_race,      file=paste0(sp_data_path, "/Data/r_data/belknap_hu_race.rds", sep = ""))
save(belknap_hu_sex,       file=paste0(sp_data_path, "/Data/r_data/belknap_hu_sex.rds", sep = ""))

# save plots to sharepoint
save(belknap_pch_time_highchart, file=paste0(sp_data_path, "/Data/r_data/belknap_pch_time_highchart.rds", sep = ""))
