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

# create a PC hold variable and county variable
carroll_adm_all <- carroll_adm_all %>%
  mutate(pc_hold = ifelse(charge_desc == "PROTECTIVE CUSTODY", 1, 0),
         county = "Carroll")

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
carroll_adm_all$high_utilizer   <- as.factor(carroll_adm_all$high_utilizer)
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
