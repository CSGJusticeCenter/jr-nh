############################################
# Project: JRI New Hampshire
# File: functions.R
# Last updated: January 30, 2023
# Author: Mari Roberts

# Custom data cleaning and structure functions
############################################

# load packages
source("data_cleaning/00_library.R")

####################################

###########
# Create fy, age, los, recode race, and order variables
###########

# Create fy, age, los, and create age categories
# Organize variables
# Remove outliers for age
fnc_data_setup <- function(df){
  df1 <- df %>%
    mutate(fy = case_when(booking_date >= "2018-07-01" & booking_date <= "2019-06-30" ~ 2019,
                          booking_date >= "2019-07-01" & booking_date <= "2020-06-30" ~ 2020,
                          booking_date >= "2020-07-01" & booking_date <= "2021-06-30" ~ 2021),
           age = fy - yob,
           los = difftime(as.POSIXct(release_date), as.POSIXct(booking_date, tz="UTC"), units="days")) %>%
    filter(fy == 2019 | fy == 2020 | fy == 2021) %>%
    dplyr::select(id,
                  inmate_id,
                  yob,
                  race_code,
                  race = race_label,
                  gender = sex,
                  age,
                  charge_code,
                  charge_desc,
                  booking_date,
                  booking_type,
                  release_date,
                  release_type,
                  sentence_status,
                  los,
                  county,
                  fy) %>%
    mutate(race = ifelse(race == "Unknown", NA, race),
           age = as.numeric(age)) %>%
    filter(age >= 18) %>%
    mutate(age = ifelse(age > 100, NA, age))

  #  18–29, 30–39, and 40
  df1 <- df1 %>% mutate(age_category
                        = case_when(age <= 24 ~ "18-24 yo",
                                    age >= 25 & age <= 34 ~ "25-34 yo",
                                    age >= 35 & age <= 49 ~ "35-49 yo",
                                    age >= 50 ~ "50+ yo"))
}

# Create booking id
# Based on inmate id and booking date
fnc_booking_id <- function(df, county){
  df1 <- df %>%
    mutate(id = ifelse(is.na(id), inmate_id, id))
  df1$booking_id <- df1 %>% dplyr::group_indices(id, booking_date)
  df1 <- df1 %>%
    mutate(id = paste(county, id, sep = "_"),
           booking_id = paste(county, "booking", booking_id, sep = "_")) %>%
    select(id, inmate_id, booking_id, everything())
}

# Get maximum los by booking id
fnc_los <- function(df){
  df_new <- df %>%
    ungroup() %>%
    dplyr::group_by(booking_id) %>%
    dplyr::summarise(los_max = max(los, na.rm=TRUE))
  df_new <- merge(df, df_new, by = "booking_id")
}

###########
# Add high utilizer variables
###########

# Create flag for high utilizer for people in the top 1%, 5%, 10% percentile of entrances.
fnc_create_high_utilizer_variables <- function(df){

  #########
  # 3 yr HUs
  #########

  # Count number of entrances by id
  df_hus_3yrs <- df %>%
    dplyr::select(id, booking_id, booking_date) %>%
    dplyr::distinct() %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(num_entrances = n())

  df_hus_3yrs <- df_hus_3yrs %>%
    select(id, num_entrances) %>% distinct() %>%
    mutate(high_utilizer_4_times = num_entrances >= 4) %>%                                                # 4 or more times

    mutate(high_utilizer_1_pct   = quantile(df_hus_3yrs$num_entrances, probs = 0.99) < num_entrances) %>% # Top 1%

    mutate(high_utilizer_5_pct   = quantile(df_hus_3yrs$num_entrances, probs = 0.95) < num_entrances) %>% # Top 5%

    mutate(high_utilizer_10_pct  = quantile(df_hus_3yrs$num_entrances, probs = 0.90) < num_entrances) %>% # Top 10%

    mutate(high_utilizer_4_times = case_when(high_utilizer_4_times == TRUE  ~ "Yes",
                                             high_utilizer_4_times == FALSE ~ "No"),

           high_utilizer_1_pct  = case_when(high_utilizer_1_pct == TRUE     ~ "Yes",
                                            high_utilizer_1_pct == FALSE    ~ "No"),

           high_utilizer_5_pct  = case_when(high_utilizer_5_pct == TRUE     ~ "Yes",
                                            high_utilizer_5_pct == FALSE    ~ "No"),

           high_utilizer_10_pct = case_when(high_utilizer_10_pct == TRUE    ~ "Yes",
                                            high_utilizer_10_pct == FALSE   ~ "No"))

  return(df_hus_3yrs)
}

###########
# Add sex labels
###########

# Add sex labels depending on sex code
fnc_sex_labels <- function(df){
  df1 <- df %>%
    mutate(gender = case_when(
      gender == "M"      ~ "Male",
      gender == "Male"   ~ "Male",

      gender == "F"      ~ "Female",
      gender == "Female" ~ "Female",

      gender == "T"      ~ "Transgender",
      gender == "TF"     ~ "Transgender",
      gender == "TRANF"  ~ "Transgender",

      gender == "Not Specified" ~ "Unknown",
      gender == "U"             ~ "Unknown",
      is.na(gender)             ~ "Unknown",
      TRUE ~ gender)) %>%
    distinct()
  df1 <- df1 %>% mutate(gender = ifelse(gender == "Unknown", NA, gender))
}

###########
# Add data labels to county data
###########

# Add labels to data for data dictionaries
fnc_add_data_labels <- function(df){

  df1 <- df %>%
    select(id,
           inmate_id,
           booking_id,
           yob,
           race_code,
           race,
           gender,
           age,
           age_category,
           charge_code,
           charge_desc,
           booking_date,
           booking_type,
           release_date,
           release_type,
           sentence_status,
           sentence_status_standard,
           #los,
           los_max,
           county,
           fy,
           num_entrances,
           high_utilizer_4_times,
           high_utilizer_1_pct,
           high_utilizer_5_pct,
           high_utilizer_10_pct,
           pc_hold)

  # Change data types
  df1$id                       <- as.character(df1$id)
  df1$inmate_id                <- as.character(df1$inmate_id)
  df1$booking_id               <- as.character(df1$booking_id)
  df1$yob                      <- as.numeric(df1$yob)
  df1$race_code                <- as.factor(df1$race_code)
  df1$race                     <- as.character(df1$race)
  df1$gender                   <- as.factor(df1$gender)
  df1$age                      <- as.numeric(df1$age)
  df1$gender                   <- as.factor(df1$gender)
  df1$age_category             <- as.character(df1$age_category)
  df1$charge_code              <- as.character(df1$charge_code)
  df1$charge_desc              <- as.character(df1$charge_desc)
  df1$booking_type             <- as.character(df1$booking_type)
  df1$release_type             <- as.character(df1$release_type)
  df1$sentence_status          <- as.character(df1$sentence_status)
  df1$sentence_status_standard <- as.character(df1$sentence_status_standard)
  df1$los_max                  <- as.numeric(df1$los_max)
  df1$county                   <- as.character(df1$county)
  df1$fy                       <- as.numeric(df1$fy)
  df1$num_entrances            <- as.numeric(df1$num_entrances)

  df1$high_utilizer_4_times    <- as.character(df1$high_utilizer_4_times)
  df1$high_utilizer_1_pct      <- as.character(df1$high_utilizer_1_pct)
  df1$high_utilizer_5_pct      <- as.character(df1$high_utilizer_5_pct)
  df1$high_utilizer_10_pct     <- as.character(df1$high_utilizer_10_pct)

  df1$pc_hold             <- as.factor(df1$pc_hold)

  # Data labels
  var.labels <- c(id                       = "Unique ID",
                  inmate_id                = "Inmate ID",
                  booking_id               = "Booking id created by id and booking date",
                  yob                      = "Year of birth",
                  race_code                = "Original race code",
                  race                     = "Race",
                  gender                   = "Gender",
                  age                      = "Age (years)",
                  age_category             = "Age category",
                  charge_code              = "Charge code",
                  charge_desc              = "Charge description",
                  booking_date             = "Booking date",
                  booking_type             = "Booking type",
                  release_date             = "Release date",
                  release_type             = "Release type",
                  sentence_status          = "Sentence status",
                  sentence_status_standard = "Sentence status standardized",
                  los_max                  = "Maximum length of stay (days) to account for release date errors",
                  county                   = "County",
                  fy                       = "Fiscal year",
                  num_entrances            = "Number of booking events for all years",
                  high_utilizer_4_times    = "Is a high utilizer (entered jail 4 or more times for all 3 yrs)",
                  high_utilizer_1_pct      = "Is a high utilizer (in top 1% percentile of jail entrances for all 3 yrs)",
                  high_utilizer_5_pct      = "Is a high utilizer (in top 5% percentile of jail entrances for all 3 yrs)",
                  high_utilizer_10_pct     = "Is a high utilizer (in top 10% percentile of jail entrances for all 3 yrs)",
                  pc_hold                  = "Protective custody hold (in booking type, charge type, sentence status, or release type)")
  # Add labels to data
  df1 <- labelled::set_variable_labels(df1, .labels = var.labels)

}

# Extract levels of booking type, sentence status, and release type to understand booking information by county
fnc_investigate_booking_recordings <- function(df){
  df1 <- df %>%
    mutate(charge_desc     = as.character(charge_desc),
           booking_type    = as.character(booking_type),
           release_type    = as.character(release_type),
           sentence_status = as.character(sentence_status),
           charge_desc     = toupper(charge_desc),
           booking_type    = toupper(booking_type),
           release_type    = toupper(release_type),
           sentence_status = toupper(sentence_status)) %>%
    select(id, booking_id, charge_desc, booking_type, sentence_status, release_type) %>%
    distinct() %>%
    group_by(booking_type, sentence_status) %>%
    summarise(total = n())
}
