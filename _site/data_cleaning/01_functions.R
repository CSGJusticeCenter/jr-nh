############################################
# Project: JRI New Hampshire
# File: functions.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Load custom functions for efficient coding
############################################

# load packages
source("data_cleaning/00_library.R")

####################################

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
  df <- df %>%
  mutate(total = count_19 + count_20 + count_21) %>%
    mutate(freq = (total/sum(total, na.rm = TRUE))*100) %>%
    adorn_totals("row") %>%
    mutate(pct_19 = round(pct_19, 1),
           pct_20 = round(pct_20, 1),
           pct_21 = round(pct_21, 1),
           freq = round(freq, 1)) %>%
    mutate(pct_19 = paste0(as.character(pct_19), "%"),
           pct_20 = paste0(as.character(pct_20), "%"),
           pct_21 = paste0(as.character(pct_21), "%"),
           freq = paste0(as.character(freq), "%"))
}

###########
# Prop by fiscal year
###########

# get prop of high utilizer by FY
fnc_hu_by_year <- function(df){
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

###########
# Kable tables
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

# Highchart for pc holds over time
# subset data to PC holds
# calculate number of PC holds by month and year
# create tool tip for chart
fnc_pch_time_highchart <- function(df){
  df_pch <- df %>% filter(pc_hold == 1)
  df_pch <- df_pch %>%
    dplyr::group_by(month_year, month_year_text) %>%
    dplyr::summarise(total = n())
  df_pch <- df_pch %>%
    mutate(tooltip = paste0("<b>", month_year_text, "</b><br>","Total: ", total, "<br>"))

  df_pch_highchart <- df_pch %>%
    hchart('line', hcaes(x = month_year, y = total), color = "steelblue") %>%
    hc_setup() %>%
    hc_xAxis(title = list(text = "Month and Year", style = list(color =  "#000000", fontWeight = "bold"))) %>%
    hc_yAxis(title = list(text = "Number of PC Holds", style = list(color =  "#000000", fontWeight = "bold"))) %>%
    hc_title(text = "Number of PC Holds from 2019-2021")
}

###########
# Highcharter
###########

# custom highcharts theme
hc_theme_jc <- hc_theme_merge(
  hc_theme_smpl(),
  hc_theme(
    colors = c(
      "#1795BF",
      "#68C6A8",
      "#F0EA44",
      "#E1B32D",
      "#001F35"),
    chart = list(marginTop = 75, style = list(fontFamily = default_fonts)),
    title = list(style = list(fontFamily = default_fonts, fontSize = "20px")),
    subtitle = list(style = list(fontFamily = default_fonts, fontSize = "16px")),
    # legend = list(align = "right", verticalAlign = "bottom", layout = "vertical"), # labels = list(format = "{percentage:.0f}")
    caption = list(align = "right", y = 15),
    # xAxis = list(labels = list(style = list(fontSize = "15px")),gridLineColor = "transparent"),
    plotOptions = list(
      series = list(states = list(inactive = list(opacity = 1))),
      line = list(marker = list(enabled = TRUE)),
      spline = list(marker = list(enabled = TRUE)),
      area = list(marker = list(enabled = TRUE)),
      areaspline = list(marker = list(enabled = TRUE))))
)

# set up highcharts download buttons
hc_setup <- function(x) {
  hc_add_dependency(x, name = "modules/exporting.js") %>%
    hc_add_dependency(name = "modules/offline-exporting.js") %>%
    hc_exporting(
      enabled = FALSE, # change to TRUE to add drop down download options
      buttons = list(contextButton = list(menuItems = list("printChart", "downloadPNG", "downloadSVG", "downloadPDF")))) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_plotOptions(series = list(animation = FALSE))
}

