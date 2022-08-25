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

# replace NAs with blanks or no data
fnc_replace_nas <- function(df){
  df <- df %>%
    mutate_if(grepl('<NA>',.), ~replace(., grepl('<NA>', .), "NA")) %>%
    mutate_if(grepl('NA%',.), ~replace(., grepl('NA%', .), "-"))
}


# create high utilizer variable
fnc_create_hu_variable <- function(df){
  df1 <- df %>%
    select(inmate_id, booking_date, fy) %>%
    distinct() %>%
    group_by(inmate_id, fy) %>%
    dplyr::summarise(num_bookings = n()) %>%
    mutate(high_utilizer = ifelse(
      num_bookings >= 3, "High Utilizer", "Not High Utilizer"))
}

fnc_hu_setup <- function(df){
  # sep by fiscal year
  df_high_utilizers_19 <- df %>% filter(fy == 2019) %>%
    group_by(inmate_id, fy) %>%
    dplyr::summarise(num_bookings = n()) %>% filter(num_bookings > 3)
  df_high_utilizers_20 <- df %>% filter(fy == 2020) %>%
    group_by(inmate_id, fy) %>%
    dplyr::summarise(num_bookings = n()) %>% filter(num_bookings > 3)
  df_high_utilizers_21 <- df %>% filter(fy == 2021) %>%
    group_by(inmate_id, fy) %>%
    dplyr::summarise(num_bookings = n()) %>% filter(num_bookings > 3)

  # join data
  df_high_utilizers <- rbind(df_high_utilizers_19, df_high_utilizers_20)
  df_high_utilizers <- rbind(df_high_utilizers, df_high_utilizers_21)

  # merge with sentence data to get details
  df_high_utilizers <- left_join(df_high_utilizers, df, by = c("inmate_id", "fy"))
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

# get prop of pc holds by FY
fnc_pc_hold_by_year <- function(df, type){
  df1 <- data.frame(summarytools::freq(df$pc_hold, order = "freq", cum.percent = FALSE))
  df1 <- df1 %>% tibble::rownames_to_column("pc_hold") %>%
    dplyr::select(pc_hold,
                  count = Freq,
                  pct   = X..Valid)
}

# get prop of high utilizer by FY
fnc_hu_by_year <- function(df, type){
  df1 <- data.frame(summarytools::freq(df$high_utilizer, order = "freq", cum.percent = FALSE))
  df1 <- df1 %>% tibble::rownames_to_column("high_utilizer") %>%
    dplyr::select(high_utilizer,
                  count = Freq,
                  pct   = X..Valid)
}

# get prop of race/ethnicity by FY
fnc_race_by_year <- function(df){
  df1 <- data.frame(summarytools::freq(df$race, order = "freq", cum.percent = FALSE))
  df1 <- df1 %>% tibble::rownames_to_column("race") %>%
    dplyr::select(race,
                  count = Freq,
                  pct   = X..Valid)
}

# get prop of sex by FY
fnc_sex_by_year <- function(df){
  df1 <- data.frame(summarytools::freq(df$sex, order = "freq", cum.percent = FALSE))
  df1 <- df1 %>% tibble::rownames_to_column("sex") %>%
    dplyr::select(sex,
                  count = Freq,
                  pct   = X..Valid)
}

# get prop of sentence types by FY
fnc_sentence_by_year <- function(df){
  df1 <- data.frame(summarytools::freq(df$sentence_status, order = "freq", cum.percent = FALSE))
  df1 <- df1 %>% tibble::rownames_to_column("sentence_status") %>%
    dplyr::select(sentence_status,
                  count = Freq,
                  pct   = X..Valid)
}

# get prop of booking types by FY
fnc_booking_by_year <- function(df){
  df1 <- data.frame(summarytools::freq(df$booking_type, order = "freq", cum.percent = FALSE))
  df1 <- df1 %>% tibble::rownames_to_column("booking_type") %>%
    dplyr::select(booking_type,
                  count = Freq,
                  pct   = X..Valid)
}

###########
# Data descending
###########

# arrange data descending
fnc_pc_hold_data_desc <- function(df){
  df <- df %>% arrange(-count_19)
  df <- df[1:length(df)]
  df$row_num <- seq.int(nrow(df))
  total_num <- as.character(as.numeric(max(df$row_num, na.rm = TRUE)) + 2)
  na_num <- as.character(as.numeric(max(df$row_num, na.rm = TRUE)) + 1)
  df$row_num <- as.character(df$row_num)
  df <- df %>%
    mutate(row_num = case_when(pc_hold == "Total" ~ total_num,
                               pc_hold == "NA" ~ na_num,
                               TRUE ~ row_num))
  df <- df %>% arrange(row_num) %>% dplyr::select(-row_num)
}

# arrange data descending
fnc_sentence_data_desc <- function(df){
  df <- df %>% arrange(-count_19)
  df$row_num <- seq.int(nrow(df))
  total <- as.character(as.numeric(max(df$row_num, na.rm = TRUE)) + 2)
  na <- as.character(as.numeric(max(df$row_num, na.rm = TRUE)) + 1)
  df$row_num <- as.character(df$row_num)
  df <- df %>%
    mutate(row_num = case_when(sentence_status == "Total" ~ total,
                               sentence_status == "<Na>" | sentence_status == "<NA>"~ na,
                               TRUE ~ row_num))
  df$row_num <- as.numeric(df$row_num)
  df <- df %>% arrange(row_num) %>% dplyr::select(-row_num)
}

# arrange data descending
fnc_race_data_desc <- function(df){
  df <- df %>% arrange(-count_19)
  df$row_num <- seq.int(nrow(df))
  total <- as.character(as.numeric(max(df$row_num, na.rm = TRUE)) + 2)
  na <- as.character(as.numeric(max(df$row_num, na.rm = TRUE)) + 1)
  df$row_num <- as.character(df$row_num)
  df <- df %>%
    mutate(row_num = case_when(race == "Total" ~ total,
                               race == "<Na>" | race == "<NA>" ~ na,
                               TRUE ~ row_num))
  df$row_num <- as.numeric(df$row_num)
  df <- df %>% arrange(row_num) %>% dplyr::select(-row_num)
}

# arrange data descending
fnc_sex_data_desc <- function(df){
  df <- df %>% arrange(-count_19)
  df$row_num <- seq.int(nrow(df))
  total <- as.character(as.numeric(max(df$row_num, na.rm = TRUE)) + 2)
  na <- as.character(as.numeric(max(df$row_num, na.rm = TRUE)) + 1)
  df$row_num <- as.character(df$row_num)
  df <- df %>%
    mutate(row_num = case_when(sex == "Total" ~ total,
                               sex == "<Na>" | sex == "<NA>" ~ na,
                               TRUE ~ row_num))
  df$row_num <- as.numeric(df$row_num)
  df <- df %>% arrange(row_num) %>% dplyr::select(-row_num)
}

# arrange data descending
fnc_booking_data_desc <- function(df){
  df <- df %>% arrange(-count_19)
  df$row_num <- seq.int(nrow(df))
  total <- as.character(as.numeric(max(df$row_num, na.rm = TRUE)) + 2)
  na <- as.character(as.numeric(max(df$row_num, na.rm = TRUE)) + 1)
  df$row_num <- as.character(df$row_num)
  df <- df %>%
    mutate(row_num = case_when(booking_type == "Total" ~ total,
                               booking_type == "<Na>" | booking_type == "<NA>" ~ na,
                               TRUE ~ row_num))
  df$row_num <- as.numeric(df$row_num)
  df <- df %>% arrange(row_num) %>% dplyr::select(-row_num)
}

# arrange data descending
fnc_hu_data_desc <- function(df){
  df <- df %>% arrange(-count_19)
  df$row_num <- seq.int(nrow(df))
  total <- as.character(as.numeric(max(df$row_num, na.rm = TRUE)) + 2)
  na <- as.character(as.numeric(max(df$row_num, na.rm = TRUE)) + 1)
  df$row_num <- as.character(df$row_num)
  df <- df %>%
    mutate(row_num = case_when(high_utilizer == "Total" ~ total,
                               high_utilizer == "<Na>" | high_utilizer == "<NA>" ~ na,
                               TRUE ~ row_num))
  df$row_num <- as.numeric(df$row_num)
  df <- df %>% arrange(row_num) %>% dplyr::select(-row_num)
}

###########
# Combine fy data into one table
###########

fnc_pc_hold_table <- function(df_19, df_20, df_21){
  # get count and prop of pc_hold by FY
  pc_hold_19 <- fnc_pc_hold_by_year(df_19)
  pc_hold_20 <- fnc_pc_hold_by_year(df_20)
  pc_hold_21 <- fnc_pc_hold_by_year(df_21)

  # rename variables for merging, indicate which year
  pc_hold_19 <- pc_hold_19 %>% dplyr::rename(count_19 = count,
                                             pct_19   = pct)
  pc_hold_20 <- pc_hold_20 %>% dplyr::rename(count_20 = count,
                                             pct_20   = pct)
  pc_hold_21 <- pc_hold_21 %>% dplyr::rename(count_21 = count,
                                             pct_21   = pct)

  # join data
  df <- merge(pc_hold_19, pc_hold_20, by = "pc_hold", all.x = TRUE, all.y = TRUE)
  df <- merge(df, pc_hold_21, by = "pc_hold", all.x = TRUE, all.y = TRUE)

  # create row totals and frequencies
  df[df == "NA%"] = NA
  df <- df %>%
    filter(pc_hold != "Total")
  df <- fnc_replace_nas(df)

  # get totals and frequencies without including NAs
  df <- fnc_row_totals(df)

  # divide pcts by 100
  df <- df %>% mutate(pct_19 = pct_19/100,
                      pct_20 = pct_20/100,
                      pct_21 = pct_21/100)

  # arrange table data
  df <- fnc_pc_hold_data_desc(df)

  return(df)
}

fnc_race_table <- function(df_19, df_20, df_21){
  # get count and prop of race ethnicity by FY
  race_19 <- fnc_race_by_year(df_19)
  race_20 <- fnc_race_by_year(df_20)
  race_21 <- fnc_race_by_year(df_21)

  # rename variables for merging, indicate which year
  race_19 <- race_19 %>% dplyr::rename(count_19 = count,
                                       pct_19   = pct)
  race_20 <- race_20 %>% dplyr::rename(count_20 = count,
                                       pct_20   = pct)
  race_21 <- race_21 %>% dplyr::rename(count_21 = count,
                                       pct_21   = pct)

  # join data
  df <- merge(race_19, race_20, by = "race", all.x = TRUE, all.y = TRUE)
  df <- merge(df, race_21, by = "race", all.x = TRUE, all.y = TRUE)

  # arrange table data
  df <- fnc_race_data_desc(df)

  # create row totals and frequencies
  df[df == "NA%"] = NA
  df <- df %>%
    filter(race != "Total")
  df <- fnc_row_totals(df)
}

fnc_sex_table <- function(df_19, df_20, df_21){
  # get count and prop of sex by FY
  sex_19 <- fnc_sex_by_year(df_19)
  sex_20 <- fnc_sex_by_year(df_20)
  sex_21 <- fnc_sex_by_year(df_21)

  # rename variables for merging, indicate which year
  sex_19 <- sex_19 %>% dplyr::rename(count_19 = count,
                                     pct_19   = pct)
  sex_20 <- sex_20 %>% dplyr::rename(count_20 = count,
                                     pct_20   = pct)
  sex_21 <- sex_21 %>% dplyr::rename(count_21 = count,
                                     pct_21   = pct)

  # join data
  df <- merge(sex_19, sex_20, by = "sex", all.x = TRUE, all.y = TRUE)
  df <- merge(df, sex_21, by = "sex", all.x = TRUE, all.y = TRUE)

  # arrange table data
  df <- fnc_sex_data_desc(df)

  # create row totals and frequencies
  df[df == "NA%"] = NA
  df <- df %>%
    filter(sex != "Total")
  df <- fnc_row_totals(df)
}

fnc_booking_table <- function(df_19, df_20, df_21){
  # get count and prop of booking by FY
  booking_19 <- fnc_booking_by_year(df_19)
  booking_20 <- fnc_booking_by_year(df_20)
  booking_21 <- fnc_booking_by_year(df_21)

  # rename variables for merging, indicate which year
  booking_19 <- booking_19 %>% dplyr::rename(count_19 = count,
                                             pct_19   = pct)
  booking_20 <- booking_20 %>% dplyr::rename(count_20 = count,
                                             pct_20   = pct)
  booking_21 <- booking_21 %>% dplyr::rename(count_21 = count,
                                             pct_21   = pct)

  # join data
  df <- merge(booking_19, booking_20, by = "booking_type", all.x = TRUE, all.y = TRUE)
  df <- merge(df, booking_21, by = "booking_type", all.x = TRUE, all.y = TRUE)

  # arrange table data
  df <- fnc_booking_data_desc(df)

  # create row totals and frequencies
  df[df == "NA%"] = NA
  df <- df %>%
    filter(booking_type != "Total")
  df <- fnc_row_totals(df)
}

fnc_sentence_table <- function(df_19, df_20, df_21){
  # get count and prop of sentence by FY
  sentence_19 <- fnc_sentence_by_year(df_19)
  sentence_20 <- fnc_sentence_by_year(df_20)
  sentence_21 <- fnc_sentence_by_year(df_21)

  # rename variables for merging, indicate which year
  sentence_19 <- sentence_19 %>% dplyr::rename(count_19 = count,
                                               pct_19   = pct)
  sentence_20 <- sentence_20 %>% dplyr::rename(count_20 = count,
                                               pct_20   = pct)
  sentence_21 <- sentence_21 %>% dplyr::rename(count_21 = count,
                                               pct_21   = pct)

  # join data
  df <- merge(sentence_19, sentence_20, by = "sentence_status", all.x = TRUE, all.y = TRUE)
  df <- merge(df, sentence_21, by = "sentence_status", all.x = TRUE, all.y = TRUE)

  # arrange table data
  df <- fnc_sentence_data_desc(df)

  # create row totals and frequencies
  df[df == "NA%"] = NA
  df <- df %>%
    filter(sentence_status != "Total")
  df <- fnc_row_totals(df)
}

fnc_hu_table <- function(df_19, df_20, df_21){
  # get count and prop of high utilizer by FY
  hu_19 <- fnc_hu_by_year(df_19)
  hu_20 <- fnc_hu_by_year(df_20)
  hu_21 <- fnc_hu_by_year(df_21)

  # rename variables for merging, indicate which year
  hu_19 <- hu_19 %>% dplyr::rename(count_19 = count,
                                   pct_19   = pct)
  hu_20 <- hu_20 %>% dplyr::rename(count_20 = count,
                                   pct_20   = pct)
  hu_21 <- hu_21 %>% dplyr::rename(count_21 = count,
                                   pct_21   = pct)

  # join data
  df <- merge(hu_19, hu_20, by = "high_utilizer", all.x = TRUE, all.y = TRUE)
  df <- merge(df, hu_21, by = "high_utilizer", all.x = TRUE, all.y = TRUE)

  # arrange table data
  df <- fnc_hu_data_desc(df)

  # create row totals and frequencies
  df[df == "NA%"] = NA
  df <- df %>%
    filter(high_utilizer != "Total")
  df <- fnc_row_totals(df)
}
