############################################
# Project: JRI New Hampshire
# File: merrimack.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

# load packages and custom functions
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
         charge_code = NA,
         release_type = NA,
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

# change date formats
merrimack_adm_all$booking_date <- format(merrimack_adm_all$booking_date, "%m/%d/%Y")
merrimack_adm_all$release_date <- format(merrimack_adm_all$release_date, "%m/%d/%Y")
merrimack_adm_all$booking_date <- as.Date(merrimack_adm_all$booking_date, format = "%m/%d/%Y")
merrimack_adm_all$release_date <- as.Date(merrimack_adm_all$release_date, format = "%m/%d/%Y")

# organize variables
merrimack_adm_all <- merrimack_adm_all %>%
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
merrimack_bookings <- fnc_create_hu_variable(merrimack_adm_all)

# merge data back
merrimack_adm_all <- left_join(merrimack_adm_all, merrimack_bookings, by = c("inmate_id", "fy"))

# create a PC hold variable and county variable
merrimack_adm_all <- merrimack_adm_all %>%
  mutate(pc_hold = ifelse(charge_desc == "PROTECTIVE CUSTODY" | charge_desc == "PROTECTIVE CUSTODY/INTOXICATION", 1, 0),
         county = "Merrimack")

######
# Create data dictionary
######

# change data types
merrimack_adm_all$id              <- as.factor(merrimack_adm_all$id)
merrimack_adm_all$inmate_id       <- as.character(merrimack_adm_all$inmate_id)
merrimack_adm_all$yob             <- as.numeric(merrimack_adm_all$yob)
merrimack_adm_all$race            <- as.factor(merrimack_adm_all$race)
merrimack_adm_all$sex             <- as.factor(merrimack_adm_all$sex)
merrimack_adm_all$housing         <- as.factor(merrimack_adm_all$housing)
merrimack_adm_all$charge_desc     <- as.factor(merrimack_adm_all$charge_desc)
merrimack_adm_all$booking_type    <- as.factor(merrimack_adm_all$booking_type)
merrimack_adm_all$release_type    <- as.factor(merrimack_adm_all$release_type)
merrimack_adm_all$sentence_status <- as.factor(merrimack_adm_all$sentence_status)
merrimack_adm_all$fy              <- as.factor(merrimack_adm_all$fy)
merrimack_adm_all$high_utilizer   <- as.factor(merrimack_adm_all$high_utilizer)
merrimack_adm_all$pc_hold         <- as.factor(merrimack_adm_all$pc_hold)
merrimack_adm_all$age             <- as.numeric(merrimack_adm_all$age)
merrimack_adm_all$los             <- as.numeric(merrimack_adm_all$los)

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
merrimack_adm_all <- labelled::set_variable_labels(merrimack_adm_all, .labels = var.labels)
