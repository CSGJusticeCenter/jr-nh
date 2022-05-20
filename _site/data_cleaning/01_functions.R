############################################
# Custom functions for efficient coding
# Last updated: May 13, 2022
# Author: Mari Roberts
############################################

source("data_cleaning/00_library.R")

###########
# Data cleaning
###########

# get prop of race/ethnicity by FY
fnc_race_by_year <- function(df){
  df1 <- data.frame(summarytools::freq(df$race, order = "freq", cum.percent = FALSE))
  df1 <- df1 %>% tibble::rownames_to_column("race_ethnicity") %>%
    dplyr::select(race_ethnicity,
                  booking_count = Freq,
                  booking_pct   = X..Valid) %>%
    mutate(booking_pct = round(booking_pct, 1)) %>%
    mutate(booking_pct = paste0(as.character(booking_pct), "%"))
}

# get prop of sex by FY
fnc_sex_by_year <- function(df){
  df1 <- data.frame(summarytools::freq(df$sex, order = "freq", cum.percent = FALSE))
  df1 <- df1 %>% tibble::rownames_to_column("sex") %>%
    dplyr::select(sex,
                  booking_count = Freq,
                  booking_pct   = X..Valid) %>%
    mutate(booking_pct = round(booking_pct, 1)) %>%
    mutate(booking_pct = paste0(as.character(booking_pct), "%"))
}

# get prop of sentence types by FY
fnc_sentence_by_year <- function(df){
  df1 <- data.frame(summarytools::freq(df$sentence_status, order = "freq", cum.percent = FALSE))
  df1 <- df1 %>% tibble::rownames_to_column("sentence_status") %>%
    dplyr::select(sentence_status,
                  sentence_count = Freq,
                  sentence_pct   = X..Valid) %>%
    mutate(sentence_pct = round(sentence_pct, 1)) %>%
    mutate(sentence_pct = paste0(as.character(sentence_pct), "%"),
           sentence_status = str_to_title(sentence_status))
}

# get prop of booking types by FY
fnc_booking_by_year <- function(df){
  df1 <- data.frame(summarytools::freq(df$booking_type, order = "freq", cum.percent = FALSE))
  df1 <- df1 %>% tibble::rownames_to_column("booking_type") %>%
    dplyr::select(booking_type,
                  booking_count = Freq,
                  booking_pct   = X..Valid) %>%
    mutate(booking_pct = round(booking_pct, 1)) %>%
    mutate(booking_pct = paste0(as.character(booking_pct), "%"),
           booking_type = str_to_title(booking_type))
}

###########
# Plots
###########

# booking heat map
fnc_booking_heatmap <- function(df){
ggplot(df, aes(year, month)) +
  geom_tile(aes(fill = N), colour = "white") +
  #scale_fill_gradient(low = "#d4e9f8", high = "#00475d") +
  scale_fill_gradient(low = "#eeed90", high = "#315c15") +
  guides(fill=guide_legend(title="Total Bookings")) +
  labs(title = "Number of Bookings by Month and FY",
       x = "Year", y = "Month") +
  theme_bw() + theme_minimal()
}

###########
# Tables
###########

# kable freq tables
fnc_freq_table <- function(df, title){
  last_row <- nrow(df)
  kable(df, format.args = list(big.mark = ","), align=rep('c'),
      col.names=c(title,"# Bookings","% Bookings", "# Bookings","% Bookings", "# Bookings","% Bookings")) %>%
  kable_styling(bootstrap_options = c("condensed", "responsive"),
                row_label_position = "l") %>%
  add_header_above(c(" " = 1, "FY 2019" = 2, "FY 2020" = 2, "FY 2021" = 2)) %>%
    row_spec(last_row, bold = TRUE)
}

# arrange data descending
fnc_sentence_data_desc <- function(df){
  df <- df %>% arrange(-sentence_count_19)
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
  df <- df %>% arrange(-booking_count_19)
  df$row_num <- seq.int(nrow(df))
  total <- as.character(as.numeric(max(df$row_num, na.rm = TRUE)) + 2)
  na <- as.character(as.numeric(max(df$row_num, na.rm = TRUE)) + 1)
  df$row_num <- as.character(df$row_num)
  df <- df %>%
    mutate(row_num = case_when(race_ethnicity == "Total" ~ total,
                               race_ethnicity == "<Na>" | race_ethnicity == "<NA>" ~ na,
                               TRUE ~ row_num))
  df$row_num <- as.numeric(df$row_num)
  df <- df %>% arrange(row_num) %>% dplyr::select(-row_num)
}

# arrange data descending
fnc_sex_data_desc <- function(df){
  df <- df %>% arrange(-booking_count_19)
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
  df <- df %>% arrange(-booking_count_19)
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

###########
# Clean data by county
###########

fnc_race_table <- function(df_19, df_20, df_21){
  # get count and prop of race ethnicity by FY
  race_19 <- fnc_race_by_year(df_19)
  race_20 <- fnc_race_by_year(df_20)
  race_21 <- fnc_race_by_year(df_21)

  # rename variables for merging, indicate which year
  race_19 <- race_19 %>% dplyr::rename(booking_count_19 = booking_count,
                                       booking_pct_19   = booking_pct)
  race_20 <- race_20 %>% dplyr::rename(booking_count_20 = booking_count,
                                       booking_pct_20   = booking_pct)
  race_21 <- race_21 %>% dplyr::rename(booking_count_21 = booking_count,
                                       booking_pct_21   = booking_pct)

  # join data
  df_race <- merge(race_19, race_20, by = "race_ethnicity", all.x = TRUE, all.y = TRUE)
  df_race <- merge(df_race, race_21, by = "race_ethnicity", all.x = TRUE, all.y = TRUE)

  # arrange table data
  df_race <- fnc_race_data_desc(df_race)
}

fnc_sex_table <- function(df_19, df_20, df_21){
  # get count and prop of sex by FY
  sex_19 <- fnc_sex_by_year(df_19)
  sex_20 <- fnc_sex_by_year(df_20)
  sex_21 <- fnc_sex_by_year(df_21)

  # rename variables for merging, indicate which year
  sex_19 <- sex_19 %>% dplyr::rename(booking_count_19 = booking_count,
                                     booking_pct_19   = booking_pct)
  sex_20 <- sex_20 %>% dplyr::rename(booking_count_20 = booking_count,
                                     booking_pct_20   = booking_pct)
  sex_21 <- sex_21 %>% dplyr::rename(booking_count_21 = booking_count,
                                     booking_pct_21   = booking_pct)

  # join data
  df_sex <- merge(sex_19, sex_20, by = "sex", all.x = TRUE, all.y = TRUE)
  df_sex <- merge(df_sex, sex_21, by = "sex", all.x = TRUE, all.y = TRUE)

  # arrange table data
  df_sex <- fnc_sex_data_desc(df_sex)
}

# not working properly
# fnc_booking_heatmap_data <- function(df){
#   df <- cheshire_adm
#   df$ymd <- lubridate::ymd_hms(as.character(df$booking_date))
#   df$month <- lubridate::month(df$ymd, label = TRUE)
#   df$year <- lubridate::year(df$ymd)
#   df$wday <- lubridate::wday(df$ymd, label = TRUE)
#   df$hour <- lubridate::hour(df$ymd)
#
#   df1 <- ddply(df, c("year", "month"), summarise, N = length(ymd))
#
#   #reverse order of months for easier graphing
#   df1$month <- factor(df1$month, levels=rev(levels(df1$month)))
# }

fnc_booking_table <- function(df_19, df_20, df_21){
  # get count and prop of booking by FY
  booking_19 <- fnc_booking_by_year(df_19)
  booking_20 <- fnc_booking_by_year(df_20)
  booking_21 <- fnc_booking_by_year(df_21)

  # rename variables for merging, indicate which year
  booking_19 <- booking_19 %>% dplyr::rename(booking_count_19 = booking_count,
                                             booking_pct_19   = booking_pct)
  booking_20 <- booking_20 %>% dplyr::rename(booking_count_20 = booking_count,
                                             booking_pct_20   = booking_pct)
  booking_21 <- booking_21 %>% dplyr::rename(booking_count_21 = booking_count,
                                             booking_pct_21   = booking_pct)

  # join data
  df_booking <- merge(booking_19, booking_20, by = "booking_type", all.x = TRUE, all.y = TRUE)
  df_booking <- merge(df_booking, booking_21, by = "booking_type", all.x = TRUE, all.y = TRUE)

  # arrange table data
  df_booking <- fnc_booking_data_desc(df_booking)
}

fnc_sentence_table <- function(df_19, df_20, df_21){
  # get count and prop of sentence by FY
  sentence_19 <- fnc_sentence_by_year(df_19)
  sentence_20 <- fnc_sentence_by_year(df_20)
  sentence_21 <- fnc_sentence_by_year(df_21)

  # rename variables for merging, indicate which year
  sentence_19 <- sentence_19 %>% dplyr::rename(sentence_count_19 = sentence_count,
                                               sentence_pct_19   = sentence_pct)
  sentence_20 <- sentence_20 %>% dplyr::rename(sentence_count_20 = sentence_count,
                                               sentence_pct_20   = sentence_pct)
  sentence_21 <- sentence_21 %>% dplyr::rename(sentence_count_21 = sentence_count,
                                               sentence_pct_21   = sentence_pct)

  # join data
  df_sentence <- merge(sentence_19, sentence_20, by = "sentence_status", all.x = TRUE, all.y = TRUE)
  df_sentence <- merge(df_sentence, sentence_21, by = "sentence_status", all.x = TRUE, all.y = TRUE)

  # arrange table data
  df_sentence <- fnc_sentence_data_desc(df_sentence)
}

fnc_hu_setup <- function(df){
  # sep by fiscal year
  df_high_utilizers_19 <- df %>% filter(fy == 2019) %>%
    group_by(inmate_id, fy) %>%
    dplyr::summarise(num_bookings = n()) %>% filter(num_bookings > 2)
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

###########
# Maps
###########




###########
# Highcharter
###########


