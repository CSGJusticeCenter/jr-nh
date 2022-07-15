############################################
# Project: JRI New Hampshire
# File: coos.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

# load packages and custom functions
source("data_cleaning/00_library.R")
source("data_cleaning/01_functions.R")

###################
# Coos County
###################

# clean variable names
coos_adm_all <- clean_names(coos_adm.xlsx)

# rename variables for consistency
coos_adm_all <- coos_adm_all %>%
  mutate(booking_type = NA, release_type = NA) %>%
  dplyr::select(id = unique_id,
                inmate_id,
                yob,
                race,
                sex,
                housing = homelessness_indicator,
                charge_code,
                charge_desc = charges,
                booking_date = booking_dt_tm,
                booking_type,
                release_date = release_dt_tm,
                release_type,
                sentence_status)

# change date formats
coos_adm_all$booking_date <- as.Date(coos_adm_all$booking_date, format = "%m/%d/%Y")
coos_adm_all$release_date <- as.Date(coos_adm_all$release_date, format = "%m/%d/%Y")

# create FY year variable
# will be able to filter by CY later
coos_adm_all <- coos_adm_all %>%
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
coos_adm_all <- coos_adm_all %>%
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
coos_bookings <- fnc_create_hu_variable(coos_adm_all)

# merge data back
coos_adm_all <- left_join(coos_adm_all, coos_bookings, by = c("inmate_id", "fy"))

# create a PC hold variable and county variable
coos_adm_all <- coos_adm_all %>%
  mutate(pc_hold = NA,
         county = "Coos")

######
# Create data dictionary
######

# change data types
coos_adm_all$id              <- as.factor(coos_adm_all$id)
coos_adm_all$inmate_id       <- as.character(coos_adm_all$inmate_id)
coos_adm_all$yob             <- as.numeric(coos_adm_all$yob)
coos_adm_all$race            <- as.factor(coos_adm_all$race)
coos_adm_all$sex             <- as.factor(coos_adm_all$sex)
coos_adm_all$housing         <- as.factor(coos_adm_all$housing)
coos_adm_all$charge_desc     <- as.factor(coos_adm_all$charge_desc)
coos_adm_all$booking_type    <- as.factor(coos_adm_all$booking_type)
coos_adm_all$release_type    <- as.factor(coos_adm_all$release_type)
coos_adm_all$sentence_status <- as.factor(coos_adm_all$sentence_status)
coos_adm_all$fy              <- as.factor(coos_adm_all$fy)
coos_adm_all$high_utilizer   <- as.factor(coos_adm_all$high_utilizer)
coos_adm_all$pc_hold         <- as.factor(coos_adm_all$pc_hold)
coos_adm_all$age             <- as.numeric(coos_adm_all$age)
coos_adm_all$los             <- as.numeric(coos_adm_all$los)

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
coos_adm_all <- labelled::set_variable_labels(coos_adm_all, .labels = var.labels)
