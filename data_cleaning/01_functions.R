############################################
# Project: JRI New Hampshire
# File: functions.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Custom data cleaning and structure functions
############################################

# load packages
source("data_cleaning/00_library.R")

####################################

###########
# Create fy, age, los, recode race, and order variables
###########

# add code to check for hispanic vs non hispanic variables by county????????????????????????????
# create fy, age, los, and race variabe
# organize variables
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
                  fy)
}

# create booking id to get a sense of how many booking events occurred
# based on inmate id and booking date
fnc_booking_id <- function(df, county){
  df1 <- df %>%
    mutate(id = ifelse(is.na(id), inmate_id, id))
  df1$booking_id <- df1 %>% group_indices(id, booking_date)
  df1 <- df1 %>%
    mutate(id = paste(county, id, sep = "_"),
           booking_id = paste(county, "booking", booking_id, sep = "_")) %>%
    select(id, inmate_id, booking_id, everything())
}

# calculate los
fnc_los <- function(df){
  df1 <- df %>%
    group_by(booking_id) %>%
    summarise(los_max = max(los, na.rm=TRUE))
  df1 <- merge(df, df1, by = "booking_id")
}

###########
# Create pc hold variables
###########

# some PC holds are indicated in the charge description but labeled as pretrial in the booking type
# account for this by creating multiple pc hold variables (pc_hold_booking, pc_hold_charge, pc_hold_sentence)
# create an overall pc_hold variable depending on any indication of pc hold in bookings, charges, and sentence statuses
fnc_pc_hold_variables <- function(df){
  df1 <- df %>%
    mutate(pc_hold_booking = case_when(booking_type == "PROTECTIVE CUSTODY" ~ "PC Hold",
                                       is.na(booking_type) ~ "NA",
                                       TRUE ~ "Non-PC Hold"),

           pc_hold_charge  = case_when(charge_desc == "PROTECTIVE CUSTODY"         | charge_desc == "PROTECTIVE CUSTODY/INTOXICATION" |
                                       charge_desc == "PROTECTIVE CUSTODY - DRUGS" | charge_desc == "Treatment and Services: Protective Custody" |
                                       charge_desc == "172B:1 XIII - PROTECTIVE CUSTODY 172-B:1 XIII" ~ "PC Hold",
                                       is.na(charge_desc) ~ "NA",
                                       TRUE ~ "Non-PC Hold"),

           pc_hold_sentence = case_when(sentence_status == "PROTECTIVE CUSTODY" | sentence_status == "PROTECTIVE CUSTODY HOLD" ~ "PC Hold",
                                        is.na(sentence_status) ~ "NA",
                                        TRUE ~ "Non-PC Hold"),

           pc_hold_release  = case_when(release_type == "PC Release" ~ "PC Hold",
                                        is.na(release_type) ~ "NA",
                                        TRUE ~ "Non-PC Hold")) %>%

    mutate(pc_hold          = case_when(pc_hold_booking == "PC Hold" | pc_hold_charge == "PC Hold"| pc_hold_sentence == "PC Hold" | pc_hold_release == "PC Hold" ~ "PC Hold",
                                        pc_hold_booking == "NA" & pc_hold_charge == "NA" & pc_hold_sentence == "NA" & pc_hold_release == "NA" ~ "NA",
                                        TRUE ~ "Non-PC Hold"))

}

###########
# Add high utilizer variables
###########

# create flag for high utilizer for bookings in the top 1%, 3%, 5% percentile of bookings
fnc_create_high_utilizer_variables <- function(df){
  df2 <- df %>%
    dplyr::select(id, booking_id, booking_date, fy) %>%
    dplyr::distinct() %>%
    dplyr::group_by(id, fy) %>%
    dplyr::summarise(num_bookings = n())
  df2 <- df2 %>%
    select(id, fy, num_bookings) %>% distinct() %>%
    mutate(high_utilizer_1_pct = quantile(df2$num_bookings, probs = 0.99) < num_bookings) %>%
    mutate(high_utilizer_3_pct = quantile(df2$num_bookings, probs = 0.97) < num_bookings) %>%
    mutate(high_utilizer_5_pct = quantile(df2$num_bookings, probs = 0.95) < num_bookings)
}

###########
# Add sex labels
###########

# add sex labels depending on sex code
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
}

###########
# Add data labels to county data
###########

# add labels to data for data dictionaries
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
           high_utilizer_1_pct,
           high_utilizer_3_pct,
           high_utilizer_5_pct,
           pc_hold_booking,
           pc_hold_charge,
           pc_hold_sentence,
           pc_hold_release,
           pc_hold)

  # change data types
  df1$id                  <- as.character(df1$id)
  df1$inmate_id           <- as.character(df1$inmate_id)
  df1$yob                 <- as.numeric(df1$yob)
  df1$race_code           <- as.factor(df1$race_code)
  df1$race                <- as.factor(df1$race)
  df1$gender              <- as.factor(df1$gender)
  df1$charge_code         <- as.factor(df1$charge_code)
  df1$charge_desc         <- as.factor(df1$charge_desc)
  df1$booking_type        <- as.factor(df1$booking_type)
  df1$release_type        <- as.factor(df1$release_type)
  df1$sentence_status     <- as.factor(df1$sentence_status)
  df1$fy                  <- as.factor(df1$fy)
  df1$high_utilizer_1_pct <- as.factor(df1$high_utilizer_1_pct)
  df1$high_utilizer_3_pct <- as.factor(df1$high_utilizer_3_pct)
  df1$high_utilizer_5_pct <- as.factor(df1$high_utilizer_5_pct)
  df1$pc_hold_booking     <- as.factor(df1$pc_hold_booking)
  df1$pc_hold_charge      <- as.factor(df1$pc_hold_charge)
  df1$pc_hold_sentence    <- as.factor(df1$pc_hold_sentence)
  df1$pc_hold_release     <- as.factor(df1$pc_hold_release)
  df1$pc_hold             <- as.factor(df1$pc_hold)
  df1$county              <- as.factor(df1$county)
  df1$age                 <- as.numeric(df1$age)
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
                  num_bookings        = "Number of booking events in the fiscal year",
                  high_utilizer_1_pct = "Is a high utilizer (in top 1% percentile)",
                  high_utilizer_3_pct = "Is a high utilizer (in top 3% percentile)",
                  high_utilizer_5_pct = "Is a high utilizer (in top 5% percentile)",
                  pc_hold_booking     = "Protective custody hold (booking type)",
                  pc_hold_charge      = "Protective custody hold (charge type)",
                  pc_hold_sentence    = "Protective custody hold (sentence status)",
                  pc_hold_release     = "Protective custody hold (release type)",
                  pc_hold             = "Protective custody hold (in booking type, charge type, sentence status, or release type)")
  # add labels to data
  df1 <- labelled::set_variable_labels(df1, .labels = var.labels)

}


###########################################################################################################################################

# Standardizing counties
# Uses functions above

###########################################################################################################################################

fnc_standardize_counties <- function(df, county){
  # Create fy, age, los, recode race, and order variables
  df1 <- fnc_data_setup(df)

  # add booking id by id and booking date
  df1 <- fnc_booking_id(df1, county)

  # calculate los
  df1 <- fnc_los(df1)

  # create high utilizer variable
  df_hu <- fnc_create_high_utilizer_variables(df1)
  df1 <- left_join(df1, df_hu, by = c("id", "fy"))

  # create a PC hold variables
  # some PC holds are indicated in the charge description but labeled as pretrial in the booking type
  # account for this by creating multiple pc hold variables
  df1 <- fnc_pc_hold_variables(df1)

  # custom function to add sex code labels
  df1 <- fnc_sex_labels(df1)

  # custom function to add data label
  df1 <- fnc_add_data_labels(df1)

  # remove duplicates bc of release date issues
  df1 <- df1 %>% distinct()
}

# some people have two release dates but the same booking date, use the max release date. most of the time the other los is zero
fnc_max_release_date <- function(df){
  dups <- df[duplicated(df$booking_id)|duplicated(df$booking_id, fromLast=TRUE),]
  temp <- df %>% anti_join(dups)
  dups <- dups %>% group_by(booking_id) %>% top_n(1, release_date) %>% droplevels() %>% distinct()
  df1 <- rbind(temp, dups)
  return(df1)
}

###########################################################################################################################################

# Content functions

###########################################################################################################################################

# replace NAs with blanks or no data
fnc_replace_nas <- function(df){
  df <- df %>%
    mutate_if(grepl('<NA>',.), ~replace(., grepl('<NA>', .), "NA")) %>%
    mutate_if(grepl('NA%',.), ~replace(., grepl('NA%', .), "-"))
}

# get row and column totals for tables
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

# get prop of variable by FY
fnc_variable_by_year <- function(df, variable_name){
  df$variable_name <- get(variable_name, df)
  df1 <- data.frame(summarytools::freq(df$variable_name, order = "freq", cum.percent = FALSE))
  df1 <- df1 %>% tibble::rownames_to_column("variable_name") %>%
    dplyr::select(variable_name,
                  count = Freq,
                  pct   = X..Valid)
}

###########
# Data descending
###########

# arrange data descending
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
  # get count and prop of pc_hold by FY
  df_19_new <- fnc_variable_by_year(df_19, variable_name)
  df_20_new <- fnc_variable_by_year(df_20, variable_name)
  df_21_new <- fnc_variable_by_year(df_21, variable_name)

  # rename variables for merging, indicate which year
  df_19_new <- df_19_new %>% dplyr::rename(count_19 = count,
                                           pct_19   = pct)
  df_20_new <- df_20_new %>% dplyr::rename(count_20 = count,
                                           pct_20   = pct)
  df_21_new <- df_21_new %>% dplyr::rename(count_21 = count,
                                           pct_21   = pct)

  # join data
  df <- merge(df_19_new, df_20_new, by = "variable_name", all.x = TRUE, all.y = TRUE)
  df <- merge(df, df_21_new, by = "variable_name", all.x = TRUE, all.y = TRUE)

  # create row totals and frequencies
  df[df == "NA%"] = NA
  df[is.na(df)] = 0
  df <- df %>%
    filter(variable_name != "Total")
  df <- fnc_replace_nas(df)

  # get totals and frequencies without including NAs
  df <- fnc_row_totals(df)

  # divide pcts by 100
  df <- df %>% mutate(pct_19 = pct_19/100,
                      pct_20 = pct_20/100,
                      pct_21 = pct_21/100)

  # arrange table data
  df <- fnc_variable_table_desc(df)
  df[is.na(df)] = 0

  return(df)
}

# custom function to get counties in data
fnc_counties_in_data <- function(df){
  counties <- df %>%
    mutate(county = as.character(county))
  counties <- unique(counties$county); counties <- paste(counties,collapse=", ")
}

