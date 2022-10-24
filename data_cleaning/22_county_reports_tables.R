############################################
# Project: JRI New Hampshire
# File: county_reports_tables.R
# Last updated: August 22, 2022
# Author: Mari Roberts

# combine county files
# loop through counties to generate tables
############################################

######
# High Utilizer proportion
######

# 1%
county_hu_1_pct_prop <- map(.x = counties,  .f = function(x) {
  df_19 <- bookings_entrances_19 %>% filter(county == x)
  df_20 <- bookings_entrances_20 %>% filter(county == x)
  df_21 <- bookings_entrances_21 %>% filter(county == x)
  df <- fnc_variable_table(df_19, df_20, df_21, "high_utilizer_1_pct")
  df <- df %>% mutate(county = x) %>% select(high_utilizer_1_pct = variable_name, everything()) %>%
    filter(high_utilizer_1_pct != "Total")
})

county_hu_1_pct_prop <- bind_rows(county_hu_1_pct_prop)

# 5%
county_hu_5_pct_prop <- map(.x = counties,  .f = function(x) {
  df_19 <- bookings_entrances_19 %>% filter(county == x)
  df_20 <- bookings_entrances_20 %>% filter(county == x)
  df_21 <- bookings_entrances_21 %>% filter(county == x)
  df <- fnc_variable_table(df_19, df_20, df_21, "high_utilizer_5_pct")
  df <- df %>% mutate(county = x) %>% select(high_utilizer_5_pct = variable_name, everything()) %>%
    filter(high_utilizer_5_pct != "Total")
})

county_hu_5_pct_prop <- bind_rows(county_hu_5_pct_prop)

# 10%
county_hu_10_pct_prop <- map(.x = counties,  .f = function(x) {
  df_19 <- bookings_entrances_19 %>% filter(county == x)
  df_20 <- bookings_entrances_20 %>% filter(county == x)
  df_21 <- bookings_entrances_21 %>% filter(county == x)
  df <- fnc_variable_table(df_19, df_20, df_21, "high_utilizer_10_pct")
  df <- df %>% mutate(county = x) %>% select(high_utilizer_10_pct = variable_name, everything()) %>%
    filter(high_utilizer_10_pct != "Total")
})

county_hu_10_pct_prop <- bind_rows(county_hu_10_pct_prop)

######
# Race
######

county_race <- map(.x = counties,  .f = function(x) {
  df_19 <- bookings_entrances_19 %>% filter(county == x)
  df_20 <- bookings_entrances_20 %>% filter(county == x)
  df_21 <- bookings_entrances_21 %>% filter(county == x)
  df <- fnc_variable_table(df_19, df_20, df_21, "race")
  df <- df %>% mutate(county = x) %>% select(race = variable_name, everything()) %>%
    filter(race != "Total")
})

county_race <- bind_rows(county_race)

######
# Age
######

county_age_category <- map(.x = counties,  .f = function(x) {
  df_19 <- bookings_entrances_19 %>% filter(county == x)
  df_20 <- bookings_entrances_20 %>% filter(county == x)
  df_21 <- bookings_entrances_21 %>% filter(county == x)
  df <- fnc_variable_table(df_19, df_20, df_21, "age_category")
  df <- df %>% mutate(county = x) %>% select(age_category = variable_name, everything()) %>%
    arrange(age_category) %>%
    filter(age_category != "Total")
})

county_age_category <- bind_rows(county_age_category)

######
# Gender
######

county_gender <- map(.x = counties,  .f = function(x) {
  df_19 <- bookings_entrances_19 %>% filter(county == x)
  df_20 <- bookings_entrances_20 %>% filter(county == x)
  df_21 <- bookings_entrances_21 %>% filter(county == x)
  df <- fnc_variable_table(df_19, df_20, df_21, "gender")
  df <- df %>% mutate(county = x) %>% select(gender = variable_name, everything())%>%
    filter(gender != "Total")
})

county_gender <- bind_rows(county_gender)

######
# Booking Types
######

# county_bookings_entrances_type <- map(.x = counties,  .f = function(x) {
#   df_19 <- bookings_entrances_19 %>% filter(county == x)
#   df_20 <- bookings_entrances_20 %>% filter(county == x)
#   df_21 <- bookings_entrances_21 %>% filter(county == x)
#   df <- fnc_variable_table(df_19, df_20, df_21, "booking_type")
#   df <- df %>% mutate(county = x) %>% select(booking_type = variable_name, everything()) %>%
#     filter(booking_type != "Total")
# })
#
# county_booking_type <- bind_rows(county_booking_type)

######
# Sentence statuses
######

# county_sentence_status <- map(.x = counties,  .f = function(x) {
#   df_19 <- sentence_status_19 %>% filter(county == x)
#   df_20 <- sentence_status_20 %>% filter(county == x)
#   df_21 <- sentence_status_21 %>% filter(county == x)
#   df <- fnc_variable_table(df_19, df_20, df_21, "sentence_status")
#   df <- df %>% mutate(county = x) %>% select(sentence_status = variable_name, everything())%>%
#     filter(sentence_status != "Total")
# })
#
# county_sentence_status <- bind_rows(county_sentence_status)

######
# Release types
######

# county_release_type <- map(.x = counties,  .f = function(x) {
#   df_19 <- release_type_19 %>% filter(county == x)
#   df_20 <- release_type_20 %>% filter(county == x)
#   df_21 <- release_type_21 %>% filter(county == x)
#   df <- fnc_variable_table(df_19, df_20, df_21, "release_type")
#   df <- df %>% mutate(county = x) %>% select(release_type = variable_name, everything()) %>%
#     filter(release_type != "Total")
# })
#
# county_release_type <- bind_rows(county_release_type)

##################
# Save data
##################

save(county_hu_1_pct_prop,   file=paste0(sp_data_path, "/Data/r_data/county_hu_1_pct_prop.Rda",   sep = ""))
save(county_hu_5_pct_prop,   file=paste0(sp_data_path, "/Data/r_data/county_hu_5_pct_prop.Rda",   sep = ""))
save(county_hu_10_pct_prop,  file=paste0(sp_data_path, "/Data/r_data/county_hu_10_pct_prop.Rda",  sep = ""))

save(county_race,            file=paste0(sp_data_path, "/Data/r_data/county_race.Rda",            sep = ""))
save(county_age_category,    file=paste0(sp_data_path, "/Data/r_data/county_age_category.Rda",    sep = ""))
save(county_gender,          file=paste0(sp_data_path, "/Data/r_data/county_gender.Rda",          sep = ""))

# save(county_booking_type,    file=paste0(sp_data_path, "/Data/r_data/county_booking_type.Rda",    sep = ""))
# save(county_sentence_status, file=paste0(sp_data_path, "/Data/r_data/county_sentence_status.Rda", sep = ""))
# save(county_release_type,    file=paste0(sp_data_path, "/Data/r_data/county_release_type.Rda",    sep = ""))
