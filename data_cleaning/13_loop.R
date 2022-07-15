############################################
# Project: JRI New Hampshire
# File: loop.R
# Last updated: July 15, 2022
# Author: Mari Roberts

# combine county files
# loop through counties to generate tables
############################################

# combine county data for large NH dataframe with all charge descriptions
nh_adm_all <- do.call("rbind", list(belknap_adm_all,
                                    carroll_adm_all,
                                    cheshire_adm_all,
                                    coos_adm_all,
                                    merrimack_adm_all,
                                    rockingham_adm_all))


# remove charge codes and duplicates
# keep sentence status
nh_sentence <- nh_adm_all %>%
  dplyr::select(inmate_id,
                race,
                yob,
                age,
                sex,
                housing,
                sentence_status,
                booking_date,
                los,
                fy,
                num_bookings,
                high_utilizer,
                pc_hold,
                county) %>%
  distinct()

# remove charge codes and duplicates to get picture of cohort
# remove sentence status
nh_booking <- nh_adm_all %>%
  dplyr::select(inmate_id,
                race,
                yob,
                age,
                sex,
                housing,
                booking_date,
                booking_type,
                release_date,
                los,
                fy,
                num_bookings,
                high_utilizer,
                pc_hold,
                county) %>%
  distinct()

# sep by fiscal year
nh_sentence_19 <- nh_sentence %>% filter(fy == 2019)
nh_sentence_20 <- nh_sentence %>% filter(fy == 2020)
nh_sentence_21 <- nh_sentence %>% filter(fy == 2021)

# sep by fy year
nh_booking_19 <- nh_booking %>% filter(fy == 2019)
nh_booking_20 <- nh_booking %>% filter(fy == 2020)
nh_booking_21 <- nh_booking %>% filter(fy == 2021)

counties <- nh_sentence$county %>%
  unique() %>%
  sort()

######
# High Utilizer proportion
######

nh_hu_prop <- map(.x = counties,  .f = function(x) {
  county_adm_19 <- nh_sentence_19 %>% filter(county == x)
  county_adm_20 <- nh_sentence_20 %>% filter(county == x)
  county_adm_21 <- nh_sentence_21 %>% filter(county == x)
  df <- fnc_hu_table(county_adm_19, county_adm_20, county_adm_21)
  df <- df %>% mutate(county = x)
})

nh_hu_prop <- bind_rows(nh_hu_prop)

######
# Race
######

nh_race <- map(.x = counties,  .f = function(x) {
  county_adm_19 <- nh_sentence_19 %>% filter(county == x)
  county_adm_20 <- nh_sentence_20 %>% filter(county == x)
  county_adm_21 <- nh_sentence_21 %>% filter(county == x)
  df <- fnc_race_table(county_adm_19, county_adm_20, county_adm_21)
  df <- df %>% mutate(county = x)
})

nh_race <- bind_rows(nh_race)

######
# Sex
######

nh_sex <- map(.x = counties,  .f = function(x) {
  county_adm_19 <- nh_sentence_19 %>% filter(county == x)
  county_adm_20 <- nh_sentence_20 %>% filter(county == x)
  county_adm_21 <- nh_sentence_21 %>% filter(county == x)
  df <- fnc_sex_table(county_adm_19, county_adm_20, county_adm_21)
  df <- df %>% mutate(county = x)
})

nh_sex <- bind_rows(nh_sex)

######
# Booking Types
######

nh_booking_type <- map(.x = counties,  .f = function(x) {
  county_adm_19 <- nh_booking_19 %>% filter(county == x)
  county_adm_20 <- nh_booking_20 %>% filter(county == x)
  county_adm_21 <- nh_booking_21 %>% filter(county == x)
  df <- fnc_booking_table(county_adm_19, county_adm_20, county_adm_21)
  df <- df %>% mutate(county = x)
})

nh_booking_type <- bind_rows(nh_booking_type)

######
# Sentence Statuses
######

nh_sentence_status <- map(.x = counties,  .f = function(x) {
  county_adm_19 <- nh_sentence_19 %>% filter(county == x)
  county_adm_20 <- nh_sentence_20 %>% filter(county == x)
  county_adm_21 <- nh_sentence_21 %>% filter(county == x)
  df <- fnc_sentence_table(county_adm_19, county_adm_20, county_adm_21)
  df <- df %>% mutate(county = x)
})

nh_sentence_status <- bind_rows(nh_sentence_status)

##########################################################################
# High Utilizers - more than 3 bookings in a year
##############################################################################

# custom function to create high utilizers dataframe
nh_high_utilizers_sentence <- fnc_hu_setup(nh_sentence)
nh_high_utilizers_booking  <- fnc_hu_setup(nh_booking)

# sep by fiscal year
nh_high_utilizers_sentence_19 <- nh_high_utilizers_sentence %>% filter(fy == 2019)
nh_high_utilizers_sentence_20 <- nh_high_utilizers_sentence %>% filter(fy == 2020)
nh_high_utilizers_sentence_21 <- nh_high_utilizers_sentence %>% filter(fy == 2021)

# sep by fiscal year
nh_high_utilizers_booking_19 <- nh_high_utilizers_booking %>% filter(fy == 2019)
nh_high_utilizers_booking_20 <- nh_high_utilizers_booking %>% filter(fy == 2020)
nh_high_utilizers_booking_21 <- nh_high_utilizers_booking %>% filter(fy == 2021)

######
# Demographics of High Utilizers
######

nh_hu_race <- map(.x = counties,  .f = function(x) {
  county_adm_19 <- nh_high_utilizers_booking_19 %>% filter(county == x)
  county_adm_20 <- nh_high_utilizers_booking_20 %>% filter(county == x)
  county_adm_21 <- nh_high_utilizers_booking_21 %>% filter(county == x)
  df_hu <- fnc_race_table(county_adm_19, county_adm_20, county_adm_21)
  df_hu <- df_hu %>% mutate(county = x)
})

nh_hu_race <- bind_rows(nh_hu_race)

nh_hu_sex <- map(.x = counties,  .f = function(x) {
  county_adm_19 <- nh_high_utilizers_booking_19 %>% filter(county == x)
  county_adm_20 <- nh_high_utilizers_booking_20 %>% filter(county == x)
  county_adm_21 <- nh_high_utilizers_booking_21 %>% filter(county == x)
  df_hu <- fnc_sex_table(county_adm_19, county_adm_20, county_adm_21)
  df_hu <- df_hu %>% mutate(county = x)
})

nh_hu_sex <- bind_rows(nh_hu_sex)

######
# Booking Types for High Utilizers
######

nh_hu_booking <- map(.x = counties,  .f = function(x) {
  county_adm_19 <- nh_high_utilizers_booking_19 %>% filter(county == x)
  county_adm_20 <- nh_high_utilizers_booking_20 %>% filter(county == x)
  county_adm_21 <- nh_high_utilizers_booking_21 %>% filter(county == x)
  df <- fnc_booking_table(county_adm_19, county_adm_20, county_adm_21)
  df <- df %>% mutate(county = x)
})

nh_hu_booking <- bind_rows(nh_hu_booking)

######
# Sentence Statuses for High Utilizers
######

nh_hu_sentence_status <- map(.x = counties,  .f = function(x) {
  county_adm_19 <- nh_sentence_19 %>% filter(county == x)
  county_adm_20 <- nh_sentence_20 %>% filter(county == x)
  county_adm_21 <- nh_sentence_21 %>% filter(county == x)
  df <- fnc_sentence_table(county_adm_19, county_adm_20, county_adm_21)
  df <- df %>% mutate(county = x)
})

nh_hu_sentence_status <- bind_rows(nh_hu_sentence_status)





##############################################################################
# Highcharts
##############################################################################

######
# PC holds over time
######

# create month year variables
nh_booking$month_year_text <- format(as.Date(nh_booking$booking_date, "%d/%m/%Y"), "%b %Y")
nh_booking$month_year      <- as.Date(as.yearmon(nh_booking$month_year_text))

nh_pch_time_highchart <- map(.x = counties,  .f = function(x) {
  df <- fnc_pch_time_highchart(nh_booking)
})


















############# TEST

county_adm_19 <- nh_sentence_19 %>% filter(county == "Carroll")
county_adm_20 <- nh_sentence_20 %>% filter(county == "Carroll")
county_adm_21 <- nh_sentence_21 %>% filter(county == "Carroll")

# get count and prop of sentence by FY
sentence_19 <- fnc_sentence_by_year(county_adm_19)
sentence_20 <- fnc_sentence_by_year(county_adm_20)
sentence_21 <- fnc_sentence_by_year(county_adm_21)

# rename variables for merging, indicate which year
sentence_19 <- sentence_19 %>% dplyr::rename(sentence_count_19 = sentence_count,
                                             sentence_pct_19   = sentence_pct)
sentence_20 <- sentence_20 %>% dplyr::rename(sentence_count_20 = sentence_count,
                                             sentence_pct_20   = sentence_pct)
sentence_21 <- sentence_21 %>% dplyr::rename(sentence_count_21 = sentence_count,
                                             sentence_pct_21   = sentence_pct)

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

df1 <- df %>%
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
