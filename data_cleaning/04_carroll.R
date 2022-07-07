############################################
# Project: JRI New Hampshire
# File: carroll.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

# load packages and custom functions
source("data_cleaning/00_library.R")
source("data_cleaning/01_functions.R")

###################
# Carroll County
###################

# clean variable names
carroll_releases <- clean_names(carroll_releases.xlsx)
carroll_bookings <- clean_names(carroll_bookings.xlsx)

# merge two adm files together
carroll_adm_all <- merge(carroll_releases, carroll_bookings, by = c("inmate_id", "release_dt_tm"), all.x = TRUE, all.y = TRUE)

# change date formats
carroll_adm_all$booking_dt_tm <- .POSIXct(carroll_adm_all$booking_dt_tm, tz="UTC")
carroll_adm_all$release_dt_tm <- .POSIXct(carroll_adm_all$release_dt_tm, tz="UTC")
carroll_adm_all$booking_dt_tm <- format(carroll_adm_all$booking_dt_tm, "%m/%d/%Y")
carroll_adm_all$release_dt_tm <- format(carroll_adm_all$release_dt_tm, "%m/%d/%Y")
carroll_adm_all$booking_dt_tm <- as.Date(carroll_adm_all$booking_dt_tm, format = "%m/%d/%Y")
carroll_adm_all$release_dt_tm <- as.Date(carroll_adm_all$release_dt_tm, format = "%m/%d/%Y")

# rename variables for consistency
carroll_adm_all <- carroll_adm_all %>%
  dplyr::select(id = unique_person_id,
                inmate_id,
                yob,
                race,
                sex,
                housing,
                charge_code,
                charge_desc = charge,
                booking_date = booking_dt_tm,
                booking_type,
                release_date = release_dt_tm,
                release_type,
                sentence_status = sentencing_status)

# create FY year variable
# will be able to filter by CY later
carroll_adm_all <- carroll_adm_all %>%
  mutate(fy = case_when(booking_date >= "2018-07-01" & booking_date <= "2019-06-30" ~ 2019,
                        booking_date >= "2019-07-01" & booking_date <= "2020-06-30" ~ 2020,
                        booking_date >= "2020-07-01" & booking_date <= "2021-06-30" ~ 2021),
         age = fy - yob,
         los = difftime(as.POSIXct(release_date), as.POSIXct(booking_date, tz="UTC"), units="days"),
         race = case_when(race == "A" ~ "AAPI",
                          race == "B" ~ "Black",
                          race == "I" ~ "American Indian or Alaskan Native",
                          race == "U" ~ "Unknown",
                          race == "W" ~ "White")
         ) %>%
  # remove booking outside of study timeframe
  filter(!is.na(fy))

# organize variables
carroll_adm_all <- carroll_adm_all %>%
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
carroll_bookings <- fnc_create_hu_variable(carroll_adm_all)

# merge data back
carroll_adm_all <- left_join(carroll_adm_all, carroll_bookings, by = c("inmate_id", "fy"))

# create a PC hold variable
carroll_adm_all <- carroll_adm_all %>%
  mutate(pc_hold = ifelse(sentence_status == "PROTECTIVE CUSTODY", 1, 0))

# remove charge codes and duplicates to get picture of cohort
# keep sentence status - more rows for each charge and sentence status
carroll_adm <- carroll_adm_all %>%
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
carroll_booking <- carroll_adm_all %>%
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
carroll_adm_19 <- carroll_adm %>% filter(fy == 2019)
carroll_adm_20 <- carroll_adm %>% filter(fy == 2020)
carroll_adm_21 <- carroll_adm %>% filter(fy == 2021)

# sep by fy year
carroll_booking_19 <- carroll_booking %>% filter(fy == 2019)
carroll_booking_20 <- carroll_booking %>% filter(fy == 2020)
carroll_booking_21 <- carroll_booking %>% filter(fy == 2021)

######
# High Utilizer proportion
######

# custom function to create table
carroll_hu <- fnc_hu_table(carroll_adm_19, carroll_adm_20, carroll_adm_21)

######
# Race
######

# custom function to create table
carroll_race <- fnc_race_table(carroll_adm_19, carroll_adm_20, carroll_adm_21)

######
# Sex
######

# custom function to create table
carroll_sex <- fnc_sex_table(carroll_adm_19, carroll_adm_20, carroll_adm_21)

######
# Data for booking heatmap
######

# create data for heatmap showing the number of bookings by month and year
df <- carroll_adm
df$ymd <- lubridate::ymd(as.character(df$booking_date))
df$month <- lubridate::month(df$ymd, label = TRUE)
df$year <- lubridate::year(df$ymd)
df$wday <- lubridate::wday(df$ymd, label = TRUE)
df$hour <- lubridate::hour(df$ymd)

carroll_heatmap <- ddply(df, c("year", "month"), summarise, N = length(ymd))

#reverse order of months for easier graphing
carroll_heatmap$month <- factor(carroll_heatmap$month, levels=rev(levels(carroll_heatmap$month)))

######
# Booking Types
######

# custom function to create table
carroll_bookings <- fnc_booking_table(carroll_booking_19, carroll_booking_20, carroll_booking_21)

######
# Sentence Statuses
######

# custom function to create table
carroll_sentence <- fnc_sentence_table(carroll_adm_19, carroll_adm_20, carroll_adm_21)

######
# Length of Stay
######

# # custom function to create table
# carroll_los <- fnc_los_table(carroll_adm_19, carroll_adm_20, carroll_adm_21)

##############################################################################
# High Utilizers - more than 3 bookings in a year?
##############################################################################

# custom function to create high utilizers dataframe
carroll_high_utilizers_sentence <- fnc_hu_setup(carroll_adm)
carroll_high_utilizers_booking  <- fnc_hu_setup(carroll_booking)

# sep by fiscal year
carroll_high_utilizers_sentence_19 <- carroll_high_utilizers_sentence %>% filter(fy == 2019)
carroll_high_utilizers_sentence_20 <- carroll_high_utilizers_sentence %>% filter(fy == 2020)
carroll_high_utilizers_sentence_21 <- carroll_high_utilizers_sentence %>% filter(fy == 2021)

# sep by fiscal year
carroll_high_utilizers_booking_19 <- carroll_high_utilizers_booking %>% filter(fy == 2019)
carroll_high_utilizers_booking_20 <- carroll_high_utilizers_booking %>% filter(fy == 2020)
carroll_high_utilizers_booking_21 <- carroll_high_utilizers_booking %>% filter(fy == 2021)

######
# Demographics of High Utilizers
######

# custom function to create table
carroll_hu_race <- fnc_race_table(carroll_high_utilizers_booking_19, carroll_high_utilizers_booking_20, carroll_high_utilizers_booking_21)
carroll_hu_sex  <- fnc_sex_table(carroll_high_utilizers_booking_19, carroll_high_utilizers_booking_20, carroll_high_utilizers_booking_21)

######
# Booking Types for High Utilizers
######

# custom function to create table
carroll_hu_booking <- fnc_booking_table(carroll_high_utilizers_booking_19, carroll_high_utilizers_booking_20, carroll_high_utilizers_booking_21)

######
# Sentence Statuses for High Utilizers
######

# custom function to create table
carroll_hu_sentence <- fnc_sentence_table(carroll_high_utilizers_sentence_19, carroll_high_utilizers_sentence_20, carroll_high_utilizers_sentence_21)

######
# Create data dictionary
######

# change data types
carroll_adm_all$id              <- as.factor(carroll_adm_all$id)
carroll_adm_all$inmate_id       <- as.character(carroll_adm_all$inmate_id)
carroll_adm_all$yob             <- as.numeric(carroll_adm_all$yob)
carroll_adm_all$race            <- as.factor(carroll_adm_all$race)
carroll_adm_all$sex             <- as.factor(carroll_adm_all$sex)
carroll_adm_all$housing         <- as.factor(carroll_adm_all$housing)
carroll_adm_all$charge_desc     <- as.factor(carroll_adm_all$charge_desc)
carroll_adm_all$booking_type    <- as.factor(carroll_adm_all$booking_type)
carroll_adm_all$release_type    <- as.factor(carroll_adm_all$release_type)
carroll_adm_all$sentence_status <- as.factor(carroll_adm_all$sentence_status)
carroll_adm_all$fy              <- as.factor(carroll_adm_all$fy)
carroll_adm_all$pc_hold         <- as.factor(carroll_adm_all$pc_hold)
carroll_adm_all$age             <- as.numeric(carroll_adm_all$age)
carroll_adm_all$los             <- as.numeric(carroll_adm_all$los)

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
carroll_adm_all <- labelled::set_variable_labels(carroll_adm_all, .labels = var.labels)

##############################################################################
# Highcharts
##############################################################################

######
# PC holds over time
######

# create month year variables
carroll_booking$month_year_text <- format(as.Date(carroll_booking$booking_date, "%d/%m/%Y"), "%b %Y")
carroll_booking$month_year      <- as.Date(as.yearmon(carroll_booking$month_year_text))

# custom function to generate a highchart showing pc hold bookings over time (month and year)
carroll_pch_time_highchart <- fnc_pch_time_highchart(carroll_booking)

######
# Save data
######

# save data to sharepoint
save(carroll_adm_all,      file=paste0(sp_data_path, "/Data/r_data/carroll_adm_all.rds", sep = ""))
save(carroll_adm,          file=paste0(sp_data_path, "/Data/r_data/carroll_adm.rds", sep = ""))
save(carroll_bookings,     file=paste0(sp_data_path, "/Data/r_data/carroll_bookings.rds", sep = ""))
save(carroll_sentence,     file=paste0(sp_data_path, "/Data/r_data/carroll_sentence.rds", sep = ""))
save(carroll_hu,           file=paste0(sp_data_path, "/Data/r_data/carroll_hu.rds", sep = ""))
save(carroll_race,         file=paste0(sp_data_path, "/Data/r_data/carroll_race.rds", sep = ""))
save(carroll_sex,          file=paste0(sp_data_path, "/Data/r_data/carroll_sex.rds", sep = ""))
save(carroll_heatmap,      file=paste0(sp_data_path, "/Data/r_data/carroll_heatmap.rds", sep = ""))
save(carroll_hu_booking,   file=paste0(sp_data_path, "/Data/r_data/carroll_hu_booking.rds", sep = ""))
save(carroll_hu_sentence,  file=paste0(sp_data_path, "/Data/r_data/carroll_hu_sentence.rds", sep = ""))
save(carroll_hu_race,      file=paste0(sp_data_path, "/Data/r_data/carroll_hu_race.rds", sep = ""))
save(carroll_hu_sex,       file=paste0(sp_data_path, "/Data/r_data/carroll_hu_sex.rds", sep = ""))

# save plots to sharepoint
save(carroll_pch_time_highchart, file=paste0(sp_data_path, "/Data/r_data/carroll_pch_time_highchart.rds", sep = ""))
