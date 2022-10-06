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
nh_hu_1_pct_prop <- map(.x = counties,  .f = function(x) {
  df_19 <- nh_booking_19 %>% filter(county == x)
  df_20 <- nh_booking_20 %>% filter(county == x)
  df_21 <- nh_booking_21 %>% filter(county == x)
  df <- fnc_variable_table(df_19, df_20, df_21, "high_utilizer_1_pct")
  df <- df %>% mutate(county = x) %>% select(high_utilizer_1_pct = variable_name, everything())
})

nh_hu_1_pct_prop <- bind_rows(nh_hu_1_pct_prop)

# 3%
nh_hu_3_pct_prop <- map(.x = counties,  .f = function(x) {
  df_19 <- nh_booking_19 %>% filter(county == x)
  df_20 <- nh_booking_20 %>% filter(county == x)
  df_21 <- nh_booking_21 %>% filter(county == x)
  df <- fnc_variable_table(df_19, df_20, df_21, "high_utilizer_3_pct")
  df <- df %>% mutate(county = x) %>% select(high_utilizer_3_pct = variable_name, everything())
})

nh_hu_3_pct_prop <- bind_rows(nh_hu_3_pct_prop)

# 5%
nh_hu_5_pct_prop <- map(.x = counties,  .f = function(x) {
  df_19 <- nh_booking_19 %>% filter(county == x)
  df_20 <- nh_booking_20 %>% filter(county == x)
  df_21 <- nh_booking_21 %>% filter(county == x)
  df <- fnc_variable_table(df_19, df_20, df_21, "high_utilizer_5_pct")
  df <- df %>% mutate(county = x) %>% select(high_utilizer_5_pct = variable_name, everything())
})

nh_hu_5_pct_prop <- bind_rows(nh_hu_5_pct_prop)

######
# Race
######

nh_race <- map(.x = counties,  .f = function(x) {
  df_19 <- nh_booking_19 %>% filter(county == x)
  df_20 <- nh_booking_20 %>% filter(county == x)
  df_21 <- nh_booking_21 %>% filter(county == x)
  df <- fnc_variable_table(df_19, df_20, df_21, "race")
  df <- df %>% mutate(county = x) %>% select(race = variable_name, everything())
})

nh_race <- bind_rows(nh_race)

######
# Gender
######

nh_gender <- map(.x = counties,  .f = function(x) {
  df_19 <- nh_booking_19 %>% filter(county == x)
  df_20 <- nh_booking_20 %>% filter(county == x)
  df_21 <- nh_booking_21 %>% filter(county == x)
  df <- fnc_variable_table(df_19, df_20, df_21, "gender")
  df <- df %>% mutate(county = x) %>% select(gender = variable_name, everything())
})

nh_gender <- bind_rows(nh_gender)

######
# Booking Types
######

nh_booking_type <- map(.x = counties,  .f = function(x) {
  df_19 <- nh_booking_19 %>% filter(county == x)
  df_20 <- nh_booking_20 %>% filter(county == x)
  df_21 <- nh_booking_21 %>% filter(county == x)
  df <- fnc_variable_table(df_19, df_20, df_21, "booking_type")
  df <- df %>% mutate(county = x) %>% select(booking_type = variable_name, everything())
})

nh_booking_type <- bind_rows(nh_booking_type)

######
# Sentence statuses
######

nh_sentence_status <- map(.x = counties,  .f = function(x) {
  df_19 <- nh_sentence_status_19 %>% filter(county == x)
  df_20 <- nh_sentence_status_20 %>% filter(county == x)
  df_21 <- nh_sentence_status_21 %>% filter(county == x)
  df <- fnc_variable_table(df_19, df_20, df_21, "sentence_status")
  df <- df %>% mutate(county = x) %>% select(sentence_status = variable_name, everything())
})

nh_sentence_status <- bind_rows(nh_sentence_status)

######
# Release types
######

nh_release_type <- map(.x = counties,  .f = function(x) {
  df_19 <- nh_release_type_19 %>% filter(county == x)
  df_20 <- nh_release_type_20 %>% filter(county == x)
  df_21 <- nh_release_type_21 %>% filter(county == x)
  df <- fnc_variable_table(df_19, df_20, df_21, "release_type")
  df <- df %>% mutate(county = x) %>% select(release_type = variable_name, everything())
})

nh_release_type <- bind_rows(nh_release_type)


##################
# Save data
##################

save(nh_hu_1_pct_prop,   file=paste0(sp_data_path, "/Data/r_data/nh_hu_1_pct_prop.Rda",   sep = ""))
save(nh_hu_3_pct_prop,   file=paste0(sp_data_path, "/Data/r_data/nh_hu_3_pct_prop.Rda",   sep = ""))
save(nh_hu_5_pct_prop,   file=paste0(sp_data_path, "/Data/r_data/nh_hu_5_pct_prop.Rda",   sep = ""))

save(nh_race,            file=paste0(sp_data_path, "/Data/r_data/nh_race.Rda",            sep = ""))
save(nh_gender,          file=paste0(sp_data_path, "/Data/r_data/nh_gender.Rda",          sep = ""))
save(nh_booking_type,    file=paste0(sp_data_path, "/Data/r_data/nh_booking_type.Rda",    sep = ""))
save(nh_sentence_status, file=paste0(sp_data_path, "/Data/r_data/nh_sentence_status.Rda", sep = ""))
save(nh_release_type,    file=paste0(sp_data_path, "/Data/r_data/nh_release_type.Rda",    sep = ""))
