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
    mutate(id = ifelse(is.na(id), inmate_id, id)) %>%
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

###########
# Create pc hold variables
###########

# some PC holds are indicated in the charge description but labeled as pretrial in the booking type
# account for this by creating multiple pc hold variables
fnc_pc_hold_variables <- function(df){
  df1 <- df %>%
    mutate(pc_hold_booking = case_when(booking_type == "PROTECTIVE CUSTODY" ~ "PC Hold",
                                       is.na(booking_type) ~ "NA",
                                       TRUE ~ "Non-PC Hold"),

           pc_hold_charge  = case_when(charge_desc == "PROTECTIVE CUSTODY"         | charge_desc == "PROTECTIVE CUSTODY/INTOXICATION" |
                                       charge_desc == "PROTECTIVE CUSTODY - DRUGS" | charge_desc == "Treatment and Services: Protective Custody" ~ "PC Hold",
                                       is.na(charge_desc) ~ "NA",
                                       TRUE ~ "Non-PC Hold"),

           pc_hold_sentence = case_when(sentence_status == "PROTECTIVE CUSTODY" | sentence_status == "PROTECTIVE CUSTODY HOLD" ~ "PC Hold",
                                        is.na(sentence_status) ~ "NA",
                                        TRUE ~ "Non-PC Hold")) %>%

    mutate(pc_hold          = case_when(pc_hold_booking == "PC Hold" | pc_hold_charge == "PC Hold"| pc_hold_sentence == "PC Hold" ~ "PC Hold",
                                        pc_hold_booking == "NA" & pc_hold_charge == "NA" & pc_hold_sentence == "NA" ~ "NA",
                                        TRUE ~ "Non-PC Hold"))

}

###########
# Add sex labels
###########

fnc_sex_labels <- function(df){
  df <- df %>%
    mutate(gender = case_when(
      gender == "M"     ~ "Male",
      gender == "F"     ~ "Female",
      gender == "T"     ~ "Transgender",
      gender == "TF"    ~ "Transgender",
      gender == "TRANF" ~ "Transgender",
      gender == "U"     ~ "Unknown",
      is.na(gender)     ~ "Unknown"),
      TRUE ~ gender) %>%
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
  df1$race            <- as.factor(df1$race)
  df1$gender          <- as.factor(df1$gender)
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
                  race            = "Race",
                  gender          = "Gender",
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
                  pc_hold_sentence= "Protective custody hold (sentence status)",
                  pc_hold         = "Protective custody hold (in booking type, charge type, or sentence status)")
  # add labels to data
  df1 <- labelled::set_variable_labels(df1, .labels = var.labels)

}


###########################################################################################################################################

# Standardizing counties
# Uses functions above

###########################################################################################################################################

fnc_standardize_counties <- function(df){
  # Create fy, age, los, recode race, and order variables
  df1 <- fnc_data_setup(df)

  # create high utilizer variable
  df_hu <- fnc_create_hu_variable(df1)
  df1 <- left_join(df1, df_hu, by = c("inmate_id", "fy"))

  # create a PC hold variables
  # some PC holds are indicated in the charge description but labeled as pretrial in the booking type
  # account for this by creating multiple pc hold variables
  df1 <- fnc_pc_hold_variables(df1)

  # custom function to add sex code labels
  df1 <- fnc_sex_labels(df1)

  # custom function to add data label
  df1 <- fnc_add_data_labels(df1)
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

###########
# Reactable table by fy
###########

fnc_reactable_fy <- function(df, metric_label, label_width, reactable_counties, note){

  df1 <- df %>%
    dplyr::rename(new_variable_name = 1)

  # create reactable table of number/freq of booking types by fiscal year and for all 3 years
  fy_table <- reactable(df1,
                        pagination = FALSE,
                        theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
                        defaultColDef = reactable::colDef(
                          format = colFormat(separators = TRUE), align = "center",
                          footer = function(values, name) {
                            if (name %in% c("count_19", "count_20", "count_21", "total")) {
                              htmltools::div(paste0("", formatC(
                                x = sum(values),
                                digits = 0,
                                big.mark = ",",
                                format = "f"
                              )))
                            }
                          },
                          footerStyle = list(fontWeight = "bold")
                        ),
                        compact = TRUE,
                        fullWidth = FALSE,
                        columnGroups = list(
                          colGroup(name = "2019", columns = c("count_19", "pct_19")),
                          colGroup(name = "2020", columns = c("count_20", "pct_20")),
                          colGroup(name = "2021", columns = c("count_21", "pct_21")),
                          colGroup(name = "3 Years", columns = c("total", "freq"))
                        ),
                        columns = list(
                          new_variable_name = colDef(footer = "Total",
                                                     name = metric_label,
                                                     align = "left",
                                                     minWidth = label_width),
                          count_19     = colDef(minWidth = 80,
                                                name = "Count"),
                          pct_19       = colDef(minWidth = 80,
                                                name = "%",
                                                format = colFormat(percent = TRUE, digits = 1)),
                          count_20     = colDef(minWidth = 80,
                                                name = "Count"),
                          pct_20       = colDef(minWidth = 80,
                                                name = "%",
                                                format = colFormat(percent = TRUE, digits = 1)),
                          count_21     = colDef(minWidth = 80,
                                                name = "Count"),
                          pct_21       = colDef(minWidth = 80,
                                                name = "%",
                                                style = list(position = "sticky", borderRight = "1px solid #d3d3d3"),
                                                format = colFormat(percent = TRUE, digits = 1)),
                          total        = colDef(minWidth = 100,
                                                name = "Count"),
                          freq         = colDef(minWidth = 90,
                                                name = "%",
                                                format = colFormat(percent = TRUE, digits = 1)))) %>%
    add_source(paste("Counties included: ", reactable_counties, ". ", note), font_style = "italic", font_size = 14)

  return(fy_table)
}

# custom function to get counties in data
fnc_counties_in_data <- function(df){
  counties <- df %>%
    mutate(county = as.character(county))
  counties <- unique(counties$county); counties <- paste(counties,collapse=", ")
}

