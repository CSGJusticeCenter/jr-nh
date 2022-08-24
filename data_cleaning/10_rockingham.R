############################################
# Project: JRI New Hampshire
# File: rockingham.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

# load packages and custom functions
source("data_cleaning/00_library.R")
source("data_cleaning/01_functions.R")

###################
# Rockingham County
###################

# clean variable names
rockingham_adm_all <- clean_names(rockingham_adm.xlsx)

# rename variables for consistency
rockingham_adm_all <- rockingham_adm_all %>%
  mutate(release_type = NA, id = NA) %>%
  dplyr::select(id,
                inmate_id = inmate_id_number,
                yob,
                race,
                sex,
                housing = homelessness,
                charge_code = charge_id,
                charge_desc = charge_description,
                booking_date = arrival_date_and_time,
                booking_type = commitment_documentation,
                release_date = release_date_and_time,
                release_type,
                sentence_status)

# change date formats
rockingham_adm_all$booking_date <- as.Date(rockingham_adm_all$booking_date, format = "%m/%d/%Y")
rockingham_adm_all$release_date <- as.Date(rockingham_adm_all$release_date, format = "%m/%d/%Y")

# create FY year variable
# will be able to filter by CY later
rockingham_adm_all <- rockingham_adm_all %>%
  mutate(fy = case_when(booking_date >= "2018-07-01" & booking_date <= "2019-06-30" ~ 2019,
                        booking_date >= "2019-07-01" & booking_date <= "2020-06-30" ~ 2020,
                        booking_date >= "2020-07-01" & booking_date <= "2021-06-30" ~ 2021),
         age = fy - yob,
         los = difftime(as.POSIXct(release_date), as.POSIXct(booking_date, tz="UTC"), units="days"),
         race = case_when(race == "A"  ~ "AAPI",
                          race == "B"  ~ "Black",
                          race == "C"  ~ "AAPI",
                          race == "H"  ~ "Hispanic",
                          race == "I"  ~ "American Indian Alaska Native",
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
rockingham_adm_all <- rockingham_adm_all %>%
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
rockingham_bookings <- fnc_create_hu_variable(rockingham_adm_all)

# merge data back
rockingham_adm_all <- left_join(rockingham_adm_all, rockingham_bookings, by = c("inmate_id", "fy"))

# create a PC hold variable and county variable
rockingham_adm_all <- rockingham_adm_all %>%
  mutate(pc_hold = ifelse(charge_desc == "PROTECTIVE CUSTODY", 1, 0),
         county = "Rockingham")

######
# Create data dictionary
######

# change data types
rockingham_adm_all$id              <- as.factor(rockingham_adm_all$id)
rockingham_adm_all$inmate_id       <- as.character(rockingham_adm_all$inmate_id)
rockingham_adm_all$yob             <- as.numeric(rockingham_adm_all$yob)
rockingham_adm_all$race            <- as.factor(rockingham_adm_all$race)
rockingham_adm_all$sex             <- as.factor(rockingham_adm_all$sex)
rockingham_adm_all$housing         <- as.factor(rockingham_adm_all$housing)
rockingham_adm_all$charge_desc     <- as.factor(rockingham_adm_all$charge_desc)
rockingham_adm_all$booking_type    <- as.factor(rockingham_adm_all$booking_type)
rockingham_adm_all$release_type    <- as.factor(rockingham_adm_all$release_type)
rockingham_adm_all$sentence_status <- as.factor(rockingham_adm_all$sentence_status)
rockingham_adm_all$fy              <- as.factor(rockingham_adm_all$fy)
rockingham_adm_all$high_utilizer   <- as.factor(rockingham_adm_all$high_utilizer)
rockingham_adm_all$pc_hold         <- as.factor(rockingham_adm_all$pc_hold)
rockingham_adm_all$county         <- as.factor(rockingham_adm_all$county)
rockingham_adm_all$age             <- as.numeric(rockingham_adm_all$age)
rockingham_adm_all$los             <- as.numeric(rockingham_adm_all$los)

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
rockingham_adm_all <- labelled::set_variable_labels(rockingham_adm_all, .labels = var.labels)
