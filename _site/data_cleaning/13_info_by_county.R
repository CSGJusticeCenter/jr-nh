############################################
# Project: JRI New Hampshire
# File: loop.R
# Last updated: August 22, 2022
# Author: Mari Roberts

# combine county files
# loop through counties to generate tables
############################################

# combine county data or large NH dataframe with all charge descriptions
# missing strafford, hillsborough for now
nh_adm_all <- do.call("rbind", list(belknap_adm_all,
                                    carroll_adm_all,
                                    cheshire_adm_all,
                                    coos_adm_all,
                                    merrimack_adm_all,
                                    rockingham_adm_all,
                                    sullivan_adm_all))

# remove charge codes and duplicates
# keep sentence status
nh_sentence <- nh_adm_all %>%
  dplyr::select(inmate_id,
                race,
                yob,
                age,
                sex,
                sentence_status,
                booking_date,
                fy,
                num_bookings,
                high_utilizer,
                pc_hold,
                county) %>%
  distinct()
dim(nh_sentence)

# remove charge codes and duplicates to get picture of cohort
# remove sentence status
# create month year variables
nh_booking <- nh_adm_all %>%
  dplyr::select(inmate_id,
                race,
                yob,
                age,
                sex,
                booking_date,
                booking_type,
                fy,
                num_bookings,
                high_utilizer,
                pc_hold,
                county) %>%
  mutate(month_year_text = format(as.Date(booking_date, "%d/%m/%Y"), "%b %Y"),
         month_year      = as.Date(as.yearmon(month_year_text))) %>%
  distinct()
dim(nh_booking)

# sep by fiscal year
nh_sentence_19 <- nh_sentence %>% filter(fy == 2019)
nh_sentence_20 <- nh_sentence %>% filter(fy == 2020)
nh_sentence_21 <- nh_sentence %>% filter(fy == 2021)

# sep by fy year
nh_booking_19 <- nh_booking %>% filter(fy == 2019)
nh_booking_20 <- nh_booking %>% filter(fy == 2020)
nh_booking_21 <- nh_booking %>% filter(fy == 2021)

# get list of counties
counties <- nh_adm_all$county %>%
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
