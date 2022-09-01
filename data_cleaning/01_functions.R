############################################
# Project: JRI New Hampshire
# File: functions.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Custom data cleaning and table functions
############################################

# load packages
source("data_cleaning/00_library.R")

####################################

###########
# Create fy, age, los, recode race, and order variables
###########

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
                                  race_code == "K"  ~ "AAPI",


                                  race_code == "B"  ~ "Black",

                                  race_code == "H"  ~ "Hispanic",
                                  race_code == "L"  ~ "Hispanic",

                                  race_code == "I"  ~ "American Indian Alaska Native",

                                  race_code == "U"  ~ "Unknown",
                                  race_code == "NH" ~ "Unknown",
                                  race_code == "N"  ~ "Unknown",
                                  race_code == "X"  ~ "Unknown",

                                  race_code == "O"  ~ "Other",
                                  race_code == "P"  ~ "Other",

                                  race_code == "W"  ~ "White")) %>%
    filter(fy == 2019 | fy == 2020 | fy == 2021) %>%
    dplyr::select(id,
                  inmate_id,
                  yob,
                  race_code,
                  race_label,
                  sex,
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

###########
# Create pc hold variables
###########

# some PC holds are indicated in the charge description but labeled as pretrial in the booking type
# account for this by creating multiple pc hold variables
fnc_pc_hold_variables <- function(df){
  df1 <- df %>%
    mutate(pc_hold_booking = ifelse(booking_type == "PROTECTIVE CUSTODY", 1, 0),

           pc_hold_charge  = ifelse(charge_desc == "PROTECTIVE CUSTODY"         | charge_desc == "PROTECTIVE CUSTODY/INTOXICATION" |
                                    charge_desc == "PROTECTIVE CUSTODY - DRUGS" | charge_desc == "Treatment and Services: Protective Custody", 1, 0),

           pc_hold_sentence = ifelse(sentence_status == "PROTECTIVE CUSTODY" | sentence_status == "PROTECTIVE CUSTODY HOLD", 1, 0)) %>%

    mutate(pc_hold         = ifelse(pc_hold_booking == 1 | pc_hold_charge == 1 | pc_hold_sentence == 1, 1, 0))
}

###########
# Add sex labels
###########

fnc_sex_labels <- function(df){
  df <- df %>%
    mutate(sex = case_when(
      sex == "M"     ~ "Male",
      sex == "F"     ~ "Female",
      sex == "T"     ~ "Transgender",
      sex == "TF"    ~ "Transgender",
      sex == "TRANF" ~ "Transgender",
      sex == "U"     ~ "Unknown",
      is.na(sex)     ~ "Unknown"),
      TRUE ~ sex) %>%
    distinct()
}

###########
# Add data labels to county data
###########

fnc_add_data_labels <- function(df){

  df1 <- df

  # change data types
  df1$id              <- as.factor(df1$id)
  df1$inmate_id       <- as.character(df1$inmate_id)
  df1$yob             <- as.numeric(df1$yob)
  df1$race_code       <- as.factor(df1$race_code)
  df1$race_label      <- as.factor(df1$race_label)
  df1$sex             <- as.factor(df1$sex)
  df1$charge_desc     <- as.factor(df1$charge_desc)
  df1$booking_type    <- as.factor(df1$booking_type)
  df1$release_type    <- as.factor(df1$release_type)
  df1$sentence_status <- as.factor(df1$sentence_status)
  df1$fy              <- as.factor(df1$fy)
  df1$high_utilizer   <- as.factor(df1$high_utilizer)
  df1$pc_hold_booking <- as.factor(df1$pc_hold_booking)
  df1$pc_hold_charge  <- as.factor(df1$pc_hold_charge)
  df1$pc_hold         <- as.factor(df1$pc_hold)
  df1$county          <- as.factor(df1$county)
  df1$age             <- as.numeric(df1$age)
  df1$los             <- as.numeric(df1$los)

  # data labels
  var.labels <- c(id              = "Unique ID",
                  inmate_id       = "Inmate ID",
                  yob             = "Year of birth",
                  race_code       = "Original race code",
                  race_label      = "Race",
                  sex             = "Sex",
                  age             = "Age (years)",
                  charge_code     = "Charge code",
                  charge_desc     = "Charge description",
                  booking_date    = "Booking date",
                  booking_type    = "Booking type",
                  release_date    = "Release date",
                  release_type    = "Release type",
                  sentence_status = "Sentence status",
                  los             = "Length of stay (days)",
                  county          = "County",
                  fy              = "Fiscal year",
                  num_bookings    = "Number of booking events in the fiscal year",
                  high_utilizer   = "Is a high utilizer",
                  pc_hold_booking = "Protective custody hold (booking type)",
                  pc_hold_charge  = "Protective custody hold (charge type)",
                  pc_hold         = "Protective custody hold (booking or charge type)")
  # add labels to data
  df1 <- labelled::set_variable_labels(df1, .labels = var.labels)

}


###########################################################################################################################################

# Standardizing counties
# Uses functions above

###########################################################################################################################################

fnc_standardize_counties <- function(df){
  # Create fy, age, los, recode race, and order variables
  df <- fnc_data_setup(df)

  # create high utilizer variable
  belknap_hu <- fnc_create_hu_variable(df)
  df <- left_join(df, belknap_hu, by = c("inmate_id", "fy"))

  # create a PC hold variables
  # some PC holds are indicated in the charge description but labeled as pretrial in the booking type
  # account for this by creating multiple pc hold variables
  df <- fnc_pc_hold_variables(df)

  # custom function to add sex code labels
  df <- fnc_sex_labels(df)

  # custom function to add data label
  df <- fnc_add_data_labels(df)
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

  return(df)
}


