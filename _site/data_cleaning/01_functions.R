############################################
# Project: JRI New Hampshire
# File: functions.R
# Last updated: October 19, 2022
# Author: Mari Roberts

# Custom data cleaning and structure functions
############################################

# load packages
source("data_cleaning/00_library.R")

####################################

###########
# Create fy, age, los, recode race, and order variables
###########

# Add code to check for hispanic vs non hispanic variables by county????????????????????????????
# Create fy, age, los, and race variable
# Organize variables
# Remove outliers for age
fnc_data_setup <- function(df){
  df1 <- df %>%
    mutate(fy = case_when(booking_date >= "2018-07-01" & booking_date <= "2019-06-30" ~ 2019,
                          booking_date >= "2019-07-01" & booking_date <= "2020-06-30" ~ 2020,
                          booking_date >= "2020-07-01" & booking_date <= "2021-06-30" ~ 2021),
           age = fy - yob,
           los = difftime(as.POSIXct(release_date), as.POSIXct(booking_date, tz="UTC"), units="days"),
           race_label = case_when(race_code == "A"  ~ "AAPI",
                                  race_code == "C"  ~ "AAPI",
                                  race_code == "P"  ~ "AAPI",
                                  race_code == "K"  ~ "Black Hispanic",
                                  race_code == "Asian/Pacific Islander" ~ "AAPI",


                                  race_code == "B"  ~ "Black",
                                  race_code == "Black" ~ "Black",

                                  race_code == "H"  ~ "Hispanic",
                                  race_code == "L"  ~ "Hispanic",

                                  race_code == "I"  ~ "American Indian Alaska Native",
                                  race_code == "American Indian/Alaskan Native" ~ "American Indian Alaska Native",

                                  race_code == "U"  ~ "Unknown",
                                  race_code == "NH" ~ "Unknown",
                                  race_code == "N"  ~ "Unknown",
                                  race_code == "X"  ~ "Unknown",
                                  race_code == "Not Specified" ~ "Unknown",
                                  race_code == "Unknown" ~ "Unknown",

                                  race_code == "O"  ~ "Other",
                                  race_code == "P"  ~ "Other",

                                  race_code == "W"  ~ "White",
                                  race_code == "White" ~ "White")) %>%
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
    mutate(race = ifelse(race == "Unknown", NA, race), age = as.numeric(age)) %>%
    filter(age >= 18) %>%
    mutate(age = ifelse(age > 100, NA, age))

  #  18–29, 30–39, and 40
  df1 <- df1 %>% mutate(age_category
                        = case_when(age <= 29 ~ "18-29 yo",
                                    age >= 30 & age <= 39 ~ "30-39 yo",
                                    age >= 40 ~ "40+ yo"))
}

# Create booking id.
# Based on inmate id and booking date.
fnc_booking_id <- function(df, county){
  df1 <- df %>%
    mutate(id = ifelse(is.na(id), inmate_id, id))
  df1$booking_id <- df1 %>% group_indices(id, booking_date)
  df1 <- df1 %>%
    mutate(id = paste(county, id, sep = "_"),
           booking_id = paste(county, "booking", booking_id, sep = "_")) %>%
    select(id, inmate_id, booking_id, everything())
}

# Get maximum los by booking id.
fnc_los <- function(df){
  df_new <- df %>%
    ungroup() %>%
    dplyr::group_by(booking_id) %>%
    dplyr::summarise(los_max = max(los, na.rm=TRUE))
  df_new <- merge(df, df_new, by = "booking_id")
}

###########
# Create pc hold variables
###########

# Come PC holds are indicated in the charge description but labeled as pretrial in the booking type.
# Account for this by creating multiple pc hold variables (pc_hold_booking, pc_hold_charge, pc_hold_sentence, pc_hold_release).
# Create an overall pc_hold variable depending on any indication of pc hold in bookings, charges, release types, and sentence statuses.
fnc_pc_hold_variables <- function(df){
  df1 <- df %>%
    mutate(pc_hold_booking  = case_when(booking_type == "PROTECTIVE CUSTODY" ~ "PC Hold",
                                        is.na(booking_type) ~ "NA",
                                        TRUE ~ "Non-PC Hold"),

           pc_hold_charge   = case_when(charge_desc == "PROTECTIVE CUSTODY"         | charge_desc == "PROTECTIVE CUSTODY/INTOXICATION" |
                                        charge_desc == "PROTECTIVE CUSTODY - DRUGS" | charge_desc == "Treatment and Services: Protective Custody" |
                                        charge_desc == "172B:1 XIII - PROTECTIVE CUSTODY 172-B:1 XIII" ~ "PC Hold",
                                        is.na(charge_desc) ~ "NA",
                                        TRUE ~ "Non-PC Hold"),

           pc_hold_sentence = case_when(sentence_status == "PROTECTIVE CUSTODY" | sentence_status == "PROTECTIVE CUSTODY HOLD" | sentence_status == "PC-IEA" ~ "PC Hold",
                                        is.na(sentence_status) ~ "NA",
                                        TRUE ~ "Non-PC Hold"),

           pc_hold_release  = case_when(release_type == "PC Release" ~ "PC Hold",
                                        is.na(release_type) ~ "NA",
                                        TRUE ~ "Non-PC Hold"))

  df1 <- df1 %>%
    mutate(pc_hold          = case_when(pc_hold_booking == "PC Hold" | pc_hold_charge == "PC Hold"| pc_hold_sentence == "PC Hold" | pc_hold_release == "PC Hold" ~ "PC Hold",
                                        pc_hold_booking == "NA" & pc_hold_charge == "NA" & pc_hold_sentence == "NA" & pc_hold_release == "NA" ~ "NA",

                                        (pc_hold_booking == "Non-PC Hold" | pc_hold_charge == "Non-PC Hold"| pc_hold_sentence == "Non-PC Hold" | pc_hold_release == "Non-PC Hold") &
                                          (pc_hold_booking != "PC Hold" | pc_hold_charge != "PC Hold" | pc_hold_sentence != "PC Hold" | pc_hold_release != "PC Hold") ~ "Non-PC Hold",

                                        TRUE ~ "Non-PC Hold"))

}

###########
# Add high utilizer variables
###########

# Create flag for high utilizer for bookings in the top 1%, 5%, 10% percentile of entrances.
fnc_create_high_utilizer_variables <- function(df){

  #########
  # 3 yr HUs
  #########

  df_hus_3yrs <- df %>%
    dplyr::select(id, booking_id, booking_date, fy) %>%
    dplyr::distinct() %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(num_bookings = n())

  df_hus_3yrs <- df_hus_3yrs %>%
    select(id, num_bookings) %>% distinct() %>%
    mutate(high_utilizer_4_times = num_bookings >= 4) %>%
    mutate(high_utilizer_1_pct   = quantile(df_hus_3yrs$num_bookings, probs = 0.99) < num_bookings) %>%
    mutate(high_utilizer_5_pct   = quantile(df_hus_3yrs$num_bookings, probs = 0.95) < num_bookings) %>%
    mutate(high_utilizer_10_pct  = quantile(df_hus_3yrs$num_bookings, probs = 0.90) < num_bookings) %>%

    mutate(high_utilizer_4_times = case_when(high_utilizer_4_times == TRUE ~ "Yes",
                                             high_utilizer_4_times == FALSE ~ "No"),
           high_utilizer_1_pct  = case_when(high_utilizer_1_pct == TRUE ~ "Yes",
                                            high_utilizer_1_pct == FALSE ~ "No"),
           high_utilizer_5_pct  = case_when(high_utilizer_5_pct == TRUE ~ "Yes",
                                            high_utilizer_5_pct == FALSE ~ "No"),
           high_utilizer_10_pct = case_when(high_utilizer_10_pct == TRUE ~ "Yes",
                                            high_utilizer_10_pct == FALSE ~ "No"))

  #########
  # per FY HUs - not using anymore
  #########

  df_hus_fy <- df %>%
    dplyr::select(id, booking_id, booking_date, fy) %>%
    dplyr::distinct() %>%
    dplyr::group_by(id, fy) %>%
    dplyr::summarise(num_bookings_fy = n())

  df_hus_fy <- df_hus_fy %>%
    select(id, fy, num_bookings_fy) %>% distinct() %>%
    mutate(high_utilizer_4_times_fy = num_bookings_fy >= 8) %>%
    mutate(high_utilizer_1_pct_fy   = quantile(df_hus_fy$num_bookings_fy, probs = 0.99) < num_bookings_fy) %>%
    mutate(high_utilizer_5_pct_fy   = quantile(df_hus_fy$num_bookings_fy, probs = 0.95) < num_bookings_fy) %>%
    mutate(high_utilizer_10_pct_fy  = quantile(df_hus_fy$num_bookings_fy, probs = 0.90) < num_bookings_fy) %>%

    mutate(high_utilizer_4_times_fy = case_when(high_utilizer_4_times_fy == TRUE  ~ "Yes",
                                                high_utilizer_4_times_fy == FALSE ~ "No"),
           high_utilizer_1_pct_fy   = case_when(high_utilizer_1_pct_fy   == TRUE  ~ "Yes",
                                                high_utilizer_1_pct_fy   == FALSE ~ "No"),
           high_utilizer_5_pct_fy   = case_when(high_utilizer_5_pct_fy   == TRUE  ~ "Yes",
                                                high_utilizer_5_pct_fy   == FALSE ~ "No"),
           high_utilizer_10_pct_fy  = case_when(high_utilizer_10_pct_fy  == TRUE  ~ "Yes",
                                                high_utilizer_10_pct_fy  == FALSE ~ "No"))

  df_hus_all <- merge(df_hus_fy, df_hus_3yrs, by = c("id"))
  return(df_hus_all)
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
      gender == "U"      ~ "Unknown",
      is.na(gender)      ~ "Unknown",
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
           los,
           los_max,
           county,
           fy,
           num_bookings,
           num_bookings_fy,
           high_utilizer_4_times,
           high_utilizer_1_pct,
           high_utilizer_5_pct,
           high_utilizer_10_pct,
           high_utilizer_4_times_fy,
           high_utilizer_1_pct_fy,
           high_utilizer_5_pct_fy,
           high_utilizer_10_pct_fy,
           pc_hold_booking,
           pc_hold_charge,
           pc_hold_sentence,
           pc_hold_release,
           pc_hold)

  # Change data types
  df1$id                  <- as.character(df1$id)
  df1$inmate_id           <- as.character(df1$inmate_id)
  df1$yob                 <- as.numeric(df1$yob)
  df1$race_code           <- as.factor(df1$race_code)
  df1$race                <- as.character(df1$race)
  df1$gender              <- as.factor(df1$gender)
  df1$charge_code         <- as.character(df1$charge_code)
  df1$charge_desc         <- as.character(df1$charge_desc)
  df1$booking_type        <- as.character(df1$booking_type)
  df1$release_type        <- as.character(df1$release_type)
  df1$sentence_status     <- as.character(df1$sentence_status)
  df1$fy                  <- as.numeric(df1$fy)
  df1$high_utilizer_4_times    <- as.character(df1$high_utilizer_4_times)
  df1$high_utilizer_1_pct      <- as.character(df1$high_utilizer_1_pct)
  df1$high_utilizer_5_pct      <- as.character(df1$high_utilizer_5_pct)
  df1$high_utilizer_10_pct     <- as.character(df1$high_utilizer_10_pct)
  df1$high_utilizer_4_times_fy <- as.character(df1$high_utilizer_4_times_fy)
  df1$high_utilizer_1_pct_fy   <- as.character(df1$high_utilizer_1_pct_fy)
  df1$high_utilizer_5_pct_fy   <- as.character(df1$high_utilizer_5_pct_fy)
  df1$high_utilizer_10_pct_fy  <- as.character(df1$high_utilizer_10_pct_fy)
  df1$pc_hold_booking     <- as.factor(df1$pc_hold_booking)
  df1$pc_hold_charge      <- as.factor(df1$pc_hold_charge)
  df1$pc_hold_sentence    <- as.factor(df1$pc_hold_sentence)
  df1$pc_hold_release     <- as.factor(df1$pc_hold_release)
  df1$pc_hold             <- as.factor(df1$pc_hold)
  df1$county              <- as.character(df1$county)
  df1$age                 <- as.numeric(df1$age)
  df1$age_category        <- as.factor(df1$age_category)
  df1$los                 <- as.numeric(df1$los)
  df1$los_max             <- as.numeric(df1$los_max)

  # data labels
  var.labels <- c(id                  = "Unique ID",
                  inmate_id           = "Inmate ID",
                  booking_id          = "Booking id created by id and booking date",
                  yob                 = "Year of birth",
                  race_code           = "Original race code",
                  race                = "Race",
                  gender              = "Gender",
                  age                 = "Age (years)",
                  age_category        = "Age category",
                  charge_code         = "Charge code",
                  charge_desc         = "Charge description",
                  booking_date        = "Booking date",
                  booking_type        = "Booking type",
                  release_date        = "Release date",
                  release_type        = "Release type",
                  sentence_status     = "Sentence status",
                  los                 = "Length of stay (days)",
                  los_max             = "Maximum length of stay (days) to account for release date errors",
                  county              = "County",
                  fy                  = "Fiscal year",
                  num_bookings        = "Number of booking events for all years",
                  num_bookings_fy     = "Number of booking events in the fiscal year",

                  high_utilizer_4_times = "Is a high utilizer (entered jail 4 or more times for all 3 yrs)",
                  high_utilizer_1_pct   = "Is a high utilizer (in top 1% percentile of jail entrances for all 3 yrs)",
                  high_utilizer_5_pct   = "Is a high utilizer (in top 5% percentile of jail entrances for all 3 yrs)",
                  high_utilizer_10_pct  = "Is a high utilizer (in top 10% percentile of jail entrances for all 3 yrs)",
                  high_utilizer_4_times_fy = "Is a high utilizer (entered jail 4 or more times by FY)",
                  high_utilizer_1_pct_fy   = "Is a high utilizer (in top 1% percentile of jail entrances by FY)",
                  high_utilizer_5_pct_fy   = "Is a high utilizer (in top 5% percentile of jail entrances by FY)",
                  high_utilizer_10_pc_fyt  = "Is a high utilizer (in top 10% percentile of jail entrances by FY)",
                  pc_hold_booking     = "Protective custody hold (booking type)",
                  pc_hold_charge      = "Protective custody hold (charge type)",
                  pc_hold_sentence    = "Protective custody hold (sentence status)",
                  pc_hold_release     = "Protective custody hold (release type)",
                  pc_hold             = "Protective custody hold (in booking type, charge type, sentence status, or release type)")
  # add labels to data
  df1 <- labelled::set_variable_labels(df1, .labels = var.labels)

}

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Standardizing counties
# Uses functions above

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

fnc_standardize_counties <- function(df, county){
  # Create fy, age, los, recode race, and order variables
  df1 <- fnc_data_setup(df)

  # Add booking id by id and booking date
  df1 <- fnc_booking_id(df1, county)

  # Calculate los
  df1 <- fnc_los(df1)

  # Create high utilizer variable
  df_hu <- fnc_create_high_utilizer_variables(df1)
  df1 <- left_join(df1, df_hu, by = c("id", "fy"))

  # Create a PC hold variables
  # Some PC holds are indicated in the charge description but labeled as pretrial in the booking type
  # Account for this by creating multiple pc hold variables
  df1 <- fnc_pc_hold_variables(df1)

  # Custom function to add sex code labels
  df1 <- fnc_sex_labels(df1)

  # Custom function to add data label
  df1 <- fnc_add_data_labels(df1)

  # Remove duplicates bc of release date issues
  df1 <- df1 %>% distinct()
}

# Some people have two release dates but the same booking date, use the max release date. Most of the time the other los is zero.
fnc_max_release_date <- function(df){
  dups <- df[duplicated(df$booking_id)|duplicated(df$booking_id, fromLast=TRUE),]
  temp <- df %>% anti_join(dups)
  dups <- dups %>% group_by(booking_id) %>% top_n(1, release_date) %>% droplevels() %>% distinct()
  df1 <- rbind(temp, dups)
  return(df1)
}

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Content functions

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Replace NAs with blanks or no data
fnc_replace_nas <- function(df){
  df <- df %>%
    mutate_if(grepl('<NA>',.), ~replace(., grepl('<NA>', .), "NA")) %>%
    mutate_if(grepl('NA%',.), ~replace(., grepl('NA%', .), "-"))
}

# Get row and column totals for tables.
fnc_row_totals <- function(df){
  withnas <- df %>%
    adorn_totals("row") %>%
    mutate(total = count_19 + count_20 + count_21)
  nonas <- df %>%
    dplyr::filter(across(everything(), ~ !grepl("NA", .))) %>%
    dplyr::filter(across(everything(), ~ !grepl("Total", .))) %>%
    mutate(total = count_19 + count_20 + count_21) %>%
    mutate(freq = (total/sum(total, na.rm = TRUE))) %>%
    adorn_totals("row") %>%
    select(c(1, "freq"))
  df_freq <- merge(withnas, nonas, by.x = 1, by.y = 1, all.x = TRUE)
  return(df_freq)
}

###########
# Prop by fiscal year
###########

# Get prop of variable
fnc_variable_by_year <- function(df, variable_name){
  df$variable_name <- get(variable_name, df)
  df <- df %>% select(variable_name, booking_id) %>% distinct()
  df1 <- data.frame(summarytools::freq(df$variable_name, order = "freq", cum.percent = FALSE))
  df1 <- df1 %>% tibble::rownames_to_column("variable_name") %>%
    dplyr::select(variable_name,
                  count = Freq,
                  pct   = X..Valid)
}

###########
# Data descending
###########

# Arrange data in descending order
fnc_variable_table_desc <- function(df){
  df <- df %>% arrange(-count_19)
  df <- df[1:length(df)]
  df$row_num <- seq.int(nrow(df))
  total_num <- as.character(as.numeric(max(df$row_num, na.rm = TRUE)) + 2)
  na_num <- as.character(as.numeric(max(df$row_num, na.rm = TRUE)) + 1)
  df$row_num <- as.character(df$row_num)
  df <- df %>%
    mutate(row_num = case_when(variable_name == "Total" ~ total_num,
                               variable_name == "NA" ~ na_num,
                               TRUE ~ row_num))
  df$row_num <- as.numeric(df$row_num)
  df <- df %>% arrange(row_num) %>% dplyr::select(-row_num)
}


###########
# Combine fy data into one table
###########

fnc_variable_table <- function(df_19, df_20, df_21, variable_name){
  # Get count and prop of pc_hold by FY
  df_19_new <- fnc_variable_by_year(df_19, variable_name)
  df_20_new <- fnc_variable_by_year(df_20, variable_name)
  df_21_new <- fnc_variable_by_year(df_21, variable_name)

  # Rename variables for merging, indicate which year
  df_19_new <- df_19_new %>% dplyr::rename(count_19 = count,
                                           pct_19   = pct)
  df_20_new <- df_20_new %>% dplyr::rename(count_20 = count,
                                           pct_20   = pct)
  df_21_new <- df_21_new %>% dplyr::rename(count_21 = count,
                                           pct_21   = pct)

  # Join data
  df <- merge(df_19_new, df_20_new, by = "variable_name", all.x = TRUE, all.y = TRUE)
  df <- merge(df, df_21_new, by = "variable_name", all.x = TRUE, all.y = TRUE)

  # Create row totals and frequencies
  df[df == "NA%"] = NA
  df[is.na(df)] = 0
  df <- df %>%
    filter(variable_name != "Total")
  df <- fnc_replace_nas(df)

  # Get totals and frequencies without including NAs
  df <- fnc_row_totals(df)
  # df <- df %>%
  #   mutate(total = count_19 + count_20 + count_21) %>%
  #   mutate(freq = (total/sum(total, na.rm = TRUE)))

  # divide pcts by 100
  df <- df %>% mutate(pct_19 = pct_19/100,
                      pct_20 = pct_20/100,
                      pct_21 = pct_21/100)

  # arrange table data
  df <- fnc_variable_table_desc(df)
  df[is.na(df)] = 0

  return(df)
}

# Get counties in data
fnc_counties_in_data <- function(df){
  counties <- df %>%
    mutate(county = as.character(county))
  counties <- unique(counties$county); counties <- paste(counties,collapse=", ")
}

###########
# Get booking pattern info for certain flagged individuals, i.e. HU's
###########

# Calculate the average number of bookings per year (by HU for example)
fnc_avg_bookings_fy <- function(df, variable_name, logical){
  df$variable_name <- get(variable_name, df)
  df1 <- df %>%
    filter(variable_name == logical) %>%
    select(county, booking_id, num_bookings, variable_name, fy) %>%
    distinct() %>%
    group_by(fy) %>%
    # dplyr::summarise_at(vars(num_bookings), list(new_variable_name = mean))
    dplyr::summarize(new_variable_name = mean(num_bookings, na.rm=TRUE))
}

# Calculate the total number of bookings per year (by HU for example)#########################
fnc_num_bookings_fy <- function(df, variable_name, logical){
  df$variable_name <- get(variable_name, df)
  df <- df %>% select(variable_name, fy, booking_id) %>% distinct() # NEW, may cause issues
  df1 <- table(df$variable_name, df$fy)
  df1 <- as.data.frame(df1)
  df1 <- df1 %>% select(variable_name = Var1,
                        fy = Var2,
                        new_variable_name = Freq) %>%
    filter(variable_name == logical) %>%
    select(-variable_name) %>%
    mutate(fy = as.character(fy)) %>% mutate(fy = as.numeric(fy))
}

# Calculate the average number of bookings for all three years (by HU for example)
fnc_avg_bookings_3yr <- function(df, variable_name, logical){
  df$variable_name <- get(variable_name, df)
  df1 <- df %>%
    filter(variable_name == logical) %>%
    select(county, booking_id, num_bookings, variable_name, fy) %>%
    distinct() %>%
    group_by() %>%
    # dplyr::summarise_at(vars(num_bookings), list(new_variable_name = mean))
    dplyr::summarize(new_variable_name = mean(num_bookings, na.rm=TRUE))
}

# Calculate the total number of bookings for all three years (by HU for example)
fnc_num_bookings_3yr <- function(df, variable_name, logical){
  df$variable_name <- get(variable_name, df)
  df1 <- df %>%
    select(county, booking_id, num_bookings, variable_name, fy) %>%
    distinct() %>%
    filter(variable_name == logical) %>%
    group_by() %>%
    dplyr::summarise(new_variable_name = n())
}

# Calculate the average number of bookings per year and by county (by HU for example)
fnc_avg_bookings_fy_county <- function(df, variable_name, logical){
  df$variable_name <- get(variable_name, df)
  df1 <- df %>%
    filter(variable_name == logical) %>%
    droplevels() %>%
    select(county, booking_id, num_bookings, variable_name, fy) %>%
    distinct() %>%
    group_by(fy, county) %>%
    # dplyr::summarise_at(vars(num_bookings), list(new_variable_name = mean))
    dplyr::summarize(new_variable_name = mean(num_bookings, na.rm=TRUE))
}

# Calculate the total number of bookings per year and by county (by HU for example) ############################
fnc_num_bookings_fy_county <- function(df, variable_name, logical){
  df$variable_name <- get(variable_name, df)
  df <- df %>% select(variable_name, fy, county, booking_id) %>% distinct() # NEW, may cause issues
  df1 <- table(df$variable_name, df$fy, df$county)
  df1 <- as.data.frame(df1)
  df1 <- df1 %>% select(variable_name = Var1,
                        fy = Var2,
                        county = Var3,
                        new_variable_name = Freq) %>%
    filter(variable_name == logical) %>%
    select(-variable_name) %>%
    mutate(fy = as.character(fy)) %>% mutate(fy = as.numeric(fy))
}

# Calculate the average number of bookings for all three years and by county (by HU for example)
fnc_avg_bookings_3yr_county <- function(df, variable_name, logical){
  df$variable_name <- get(variable_name, df)
  df1 <- df %>%
    filter(variable_name == logical) %>%
    select(county, booking_id, num_bookings, variable_name, fy) %>%
    distinct() %>%
    group_by(county) %>%
    # dplyr::summarise_at(vars(num_bookings), list(new_variable_name = mean))
    dplyr::summarize(new_variable_name = mean(num_bookings, na.rm=TRUE))
}

# Calculate the total number of bookings for all three years and by county (by HU for example)############################
fnc_num_bookings_3yr_county <- function(df, variable_name, logical){
  df$variable_name <- get(variable_name, df)
  df1 <- df %>% dplyr::select(variable_name, county, booking_id) %>% dplyr::distinct()
  df1 <- table(df1$variable_name, df1$county)
  df1 <- as.data.frame(df1)
  df1 <- df1 %>% dplyr::select(variable_name = Var1,
                        county = Var2,
                        new_variable_name = Freq) %>%
    filter(variable_name == logical) %>%
    select(-variable_name)
}

# Basic summary info with total, min, median, mean, and max
fnc_summary <- function(df){
  #df$variable_name <- get(variable_name, df)
  df1 <- df %>%
    group_by() %>%
    summarise(
      total  = n(),
      min    = min(num_bookings, na.rm = T),
      median = median(num_bookings, na.rm = T),
      mean   = mean(c(num_bookings, na.rm = T)),
      max    = max(num_bookings, na.rm = T)
    ) %>%
    mutate(mean = round(mean, 1))
}

# Summary info by county
fnc_summary_county <- function(df){
  #df$variable_name <- get(variable_name, df)
  df1 <- df %>%
    group_by(county) %>%
    summarise(
      total  = n(),
      min    = min(num_bookings, na.rm = T),
      median = median(num_bookings, na.rm = T),
      mean   = mean(c(num_bookings, na.rm = T)),
      max    = max(num_bookings, na.rm = T)
    ) %>%
    arrange(county) %>%
    mutate(mean = round(mean, 1))
}

# Min, median, mean, and max of bookings/entrances
fnc_hus_descriptive_summary <- function(df, hu_variable_name, yesno, county_exclusion_text){

  df$hu_variable_name <- get(hu_variable_name, df)

  ##########
  # HU People
  ##########

  df_id <- df %>%
    ungroup() %>%
    filter(hu_variable_name == yesno) %>%
    select(county,
           id,
           num_bookings,
           hu_variable_name) %>%
    distinct() %>%
    group_by(county) %>%
    summarise(total_hu_people  = n()) %>%
    mutate(county = case_when(county == "Coos" ~ county_exclusion_text, TRUE ~ county))

  df_id_total <- df %>%
    ungroup() %>%
    filter(hu_variable_name == yesno) %>%
    select(county,
           id,
           num_bookings,
           hu_variable_name) %>%
    distinct() %>%
    group_by() %>%
    summarise(total_hu_people  = n()) %>%
    mutate(county = "State")

  df_id <- rbind(df_id, df_id_total)

  ##########
  # HU ENTRANCES
  ##########

  df_booking_id <- df %>%
    ungroup() %>%
    filter(hu_variable_name == yesno) %>%
    select(county,
           booking_id,
           num_bookings,
           hu_variable_name) %>%
    distinct() %>%
    group_by(county) %>%
    summarise(total_hu_entrances = n()) %>%
    mutate(county = case_when(county == "Coos" ~ county_exclusion_text, TRUE ~ county))

  df_booking_id_total <- df %>%
    ungroup() %>%
    filter(hu_variable_name == yesno) %>%
    select(county,
           booking_id,
           num_bookings,
           hu_variable_name) %>%
    distinct() %>%
    group_by() %>%
    summarise(total_hu_entrances = n()) %>%
    mutate(county = "State")

  df_booking_id <- rbind(df_booking_id, df_booking_id_total)

  ##########
  # HU min median mean max range
  ##########

  df_summary <- df %>%
    ungroup() %>%
    filter(hu_variable_name == yesno) %>%
    select(county, id, num_bookings) %>%
    distinct() %>%
    group_by(county) %>%
    summarise(min    = min(num_bookings, na.rm = T),
              median = median(num_bookings, na.rm = T),
              mean   = mean(num_bookings, na.rm = T),
              max    = max(num_bookings, na.rm = T)) %>%
    select(county, everything()) %>%
  mutate(county = case_when(county == "Coos" ~ county_exclusion_text, TRUE ~ county))


  df_summary_total <- df %>%
    ungroup() %>%
    filter(hu_variable_name == yesno) %>%
    select(id, num_bookings) %>%
    distinct() %>%
    group_by() %>%
    summarise(min    = min(num_bookings, na.rm = T),
              median = median(num_bookings, na.rm = T),
              mean   = mean(num_bookings, na.rm = T),
              max    = max(num_bookings, na.rm = T)) %>%
    mutate(county = "State") %>%
    select(county, everything())

  df_summary <- rbind(df_summary, df_summary_total)

  table_final <- df_booking_id %>%
    left_join(df_id, by = "county") %>%
    left_join(df_summary, by = "county") %>%
    arrange(county %in% "State")

}
