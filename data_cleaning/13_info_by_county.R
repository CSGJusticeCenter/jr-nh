############################################
# Project: JRI New Hampshire
# File: loop.R
# Last updated: August 22, 2022
# Author: Mari Roberts

# combine county files
# loop through counties to generate tables
############################################

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
