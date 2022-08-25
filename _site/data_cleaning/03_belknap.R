############################################
# Project: JRI New Hampshire
# File: belknap.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

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
belknap_hu <- fnc_create_hu_variable(belknap_adm_all)

# merge data back
belknap_adm_all <- left_join(belknap_adm_all, belknap_hu, by = c("inmate_id", "fy"))

# create a PC hold variable and county variable
belknap_adm_all <- belknap_adm_all %>%
  mutate(pc_hold = ifelse(charge_desc == "PROTECTIVE CUSTODY" | charge_desc == "PROTECTIVE CUSTODY/INTOXICATION", 1, 0),
         county = "Belknap")

# make sex codes consistent
belknap_adm_all <- belknap_adm_all %>%
  mutate(sex = case_when(
    sex == "M"     ~ "Male",
    sex == "F"     ~ "Female",
    sex == "T"     ~ "Transgender",
    is.na(sex)     ~ "Unknown")) %>%
  distinct() %>%
  select(-housing)

######
# Create data dictionary
######

# change data types
belknap_adm_all$id              <- as.factor(belknap_adm_all$id)
belknap_adm_all$inmate_id       <- as.character(belknap_adm_all$inmate_id)
belknap_adm_all$yob             <- as.numeric(belknap_adm_all$yob)
belknap_adm_all$race            <- as.factor(belknap_adm_all$race)
belknap_adm_all$sex             <- as.factor(belknap_adm_all$sex)
belknap_adm_all$charge_desc     <- as.factor(belknap_adm_all$charge_desc)
belknap_adm_all$booking_type    <- as.factor(belknap_adm_all$booking_type)
belknap_adm_all$release_type    <- as.factor(belknap_adm_all$release_type)
belknap_adm_all$sentence_status <- as.factor(belknap_adm_all$sentence_status)
belknap_adm_all$fy              <- as.factor(belknap_adm_all$fy)
belknap_adm_all$high_utilizer   <- as.factor(belknap_adm_all$high_utilizer)
belknap_adm_all$pc_hold         <- as.factor(belknap_adm_all$pc_hold)
belknap_adm_all$county          <- as.factor(belknap_adm_all$county)
belknap_adm_all$age             <- as.numeric(belknap_adm_all$age)
belknap_adm_all$los             <- as.numeric(belknap_adm_all$los)

# data labels
var.labels <- c(id              = "Unique ID",
                inmate_id       = "Inmate ID",
                yob             = "Year of birth",
                race            = "Race",
                sex             = "Sex",
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
belknap_adm_all <- labelled::set_variable_labels(belknap_adm_all, .labels = var.labels)

save(belknap_adm_all,    file=paste0(sp_data_path, "/Data/r_data/belknap_adm_all.Rda",    sep = ""))
