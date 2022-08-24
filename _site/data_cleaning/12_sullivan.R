############################################
# Project: JRI New Hampshire
# File: sullivan.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

# load packages and custom functions
source("data_cleaning/00_library.R")
source("data_cleaning/01_functions.R")

###################
# Sullivan County
###################

# clean variable names
sullivan_adm_all <- clean_names(sullivan_adm.xlsx)

# rename variables for consistency
sullivan_adm_all <- sullivan_adm_all %>%
  mutate(release_type = NA) %>%
  dplyr::select(id = id_number_inmate_number,
                inmate_id = inmate_num,
                yob = year,
                race,
                sex = gender,
                housing,
                charge_code = charge_id,
                charge_desc = charge,
                booking_date = booking_date_time,
                booking_type = detention_type,
                release_date = release_date_time,
                release_type,
                sentence_status)

# fix date formats
sullivan_adm_all$booking_date <- as.POSIXct(sullivan_adm_all$booking_date, format = '%m/%d/%Y %H:%M')
sullivan_adm_all$booking_date <- format(sullivan_adm_all$booking_date, "%m/%d/%Y")
sullivan_adm_all$booking_date <- as.Date(sullivan_adm_all$booking_date, format = "%m/%d/%Y")
sullivan_adm_all$release_date <- as.POSIXct(sullivan_adm_all$release_date, format = '%m/%d/%Y %H:%M')
sullivan_adm_all$release_date <- format(sullivan_adm_all$release_date, "%m/%d/%Y")
sullivan_adm_all$release_date <- as.Date(sullivan_adm_all$release_date, format = "%m/%d/%Y")

# create FY year variable
# will be able to filter by CY later
sullivan_adm_all <- sullivan_adm_all %>%
  mutate(fy = case_when(booking_date >= "2018-07-01" & booking_date <= "2019-06-30" ~ 2019,
                        booking_date >= "2019-07-01" & booking_date <= "2020-06-30" ~ 2020,
                        booking_date >= "2020-07-01" & booking_date <= "2021-06-30" ~ 2021),
         age = fy - yob,
         los = difftime(as.POSIXct(release_date), as.POSIXct(booking_date, tz="UTC"), units="days"),
         race = case_when(race == "A"  ~ "AAPI",
                          race == "B"  ~ "Black",
                          race == "H"  ~ "Hispanic",
                          race == "I"  ~ "American Indian Alaska Native",
                          race == "K"  ~ "AAPI",
                          race == "P"  ~ "AAPI",
                          race == "U"  ~ "Unknown",
                          race == "W"  ~ "White",
                          race == "X"  ~ "Unknown")
  ) %>%
  # remove booking outside of study timeframe
  filter(!is.na(fy))

# organize variables
sullivan_adm_all <- sullivan_adm_all %>%
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
sullivan_bookings <- fnc_create_hu_variable(sullivan_adm_all)

# merge data back
sullivan_adm_all <- left_join(sullivan_adm_all, sullivan_bookings, by = c("inmate_id", "fy"))

# create a PC hold variable and county variable
sullivan_adm_all <- sullivan_adm_all %>%
  mutate(pc_hold = ifelse(charge_desc == "PROTECTIVE CUSTODY", 1, 0),
         county = "sullivan")

######
# Create data dictionary
######

# change data types
sullivan_adm_all$id              <- as.factor(sullivan_adm_all$id)
sullivan_adm_all$inmate_id       <- as.character(sullivan_adm_all$inmate_id)
sullivan_adm_all$yob             <- as.numeric(sullivan_adm_all$yob)
sullivan_adm_all$race            <- as.factor(sullivan_adm_all$race)
sullivan_adm_all$sex             <- as.factor(sullivan_adm_all$sex)
sullivan_adm_all$housing         <- as.factor(sullivan_adm_all$housing)
sullivan_adm_all$charge_desc     <- as.factor(sullivan_adm_all$charge_desc)
sullivan_adm_all$booking_type    <- as.factor(sullivan_adm_all$booking_type)
sullivan_adm_all$release_type    <- as.factor(sullivan_adm_all$release_type)
sullivan_adm_all$sentence_status <- as.factor(sullivan_adm_all$sentence_status)
sullivan_adm_all$fy              <- as.factor(sullivan_adm_all$fy)
sullivan_adm_all$high_utilizer   <- as.factor(sullivan_adm_all$high_utilizer)
sullivan_adm_all$pc_hold         <- as.factor(sullivan_adm_all$pc_hold)
sullivan_adm_all$county          <- as.factor(sullivan_adm_all$county)
sullivan_adm_all$age             <- as.numeric(sullivan_adm_all$age)
sullivan_adm_all$los             <- as.numeric(sullivan_adm_all$los)

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
                pc_hold         = "Protective custody hold",
                county          = "County"
)

# add labels to data
sullivan_adm_all <- labelled::set_variable_labels(sullivan_adm_all, .labels = var.labels)
