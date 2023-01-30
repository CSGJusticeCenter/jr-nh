
################################################################################

# check dimensions of each file for carroll_adm vs carroll_medicaid

################################################################################

temp <- clean_names(carroll_bookings.xlsx)
temp$booking_dt_tm <- .POSIXct(temp$booking_dt_tm, tz="UTC")
temp$booking_dt_tm <-   format(temp$booking_dt_tm, "%m/%d/%Y")
temp$booking_dt_tm <-  as.Date(temp$booking_dt_tm, format = "%m/%d/%Y")
temp$release_dt_tm <- .POSIXct(temp$release_dt_tm, tz="UTC")
temp$release_dt_tm <-   format(temp$release_dt_tm, "%m/%d/%Y")
temp$release_dt_tm <-  as.Date(temp$release_dt_tm, format = "%m/%d/%Y")

temp <- temp %>% dplyr::select(unique_person_id,
              inmate_id,
              yob,
              race_code = race,
              sex,
              housing,
              charge_code,
              charge_desc = charge,
              booking_date = booking_dt_tm,
              booking_type,
              release_date = release_dt_tm,
              sentence_status = sentencing_status)

length(unique(temp$unique_person_id)) #  1868
temp <- temp %>%
  mutate(unique_person_id = ifelse(is.na(unique_person_id), inmate_id, unique_person_id),
         booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y"),
         county = "Carroll")
temp$booking_id <- temp %>% group_indices(unique_person_id, booking_date)
temp <- temp %>%
  mutate(unique_person_id = paste(county, unique_person_id, sep = "_"),
         booking_id = paste(county, "booking", booking_id, sep = "_")) %>%
  select(unique_person_id, inmate_id, booking_id, everything())

length(unique(temp$unique_person_id))
length(unique(temp$booking_id))



length(unique(carroll_adm1$id))
length(unique(carroll_adm1$booking_id))

length(unique(carroll_medicaid $unique_person_id))
length(unique(carroll_medicaid $booking_id))

3746 - 2646 #  1100

temp_medicaid <- carroll_medicaid %>% select(dhhs_id = unique_person_id,
                                             booking_date,
                                             jail_dob_year,
                                             jail_sex,
                                             jail_race) %>%
  mutate(jail_race = case_when(
    jail_race == "A" ~ "AAPI",
    jail_race == "B" ~ "Black",
    jail_race == "W" ~ "White",
    jail_race == "I" ~ "American Indian Alaska Native",
    jail_race == "U" ~ "Unknown"
  )) %>%
  mutate(jail_race = ifelse(jail_race == "Unknown", NA, jail_race))

temp_adm <- carroll_adm1 %>% select(id,
                                    booking_date,
                                    jail_dob_year = yob,
                                    jail_sex = gender,
                                    jail_race = race) %>%
  mutate(jail_sex = ifelse(jail_sex == 1, "F", "M"))

str(temp_medicaid)
str(temp_adm)

temp_joined <- merge(temp_adm, temp_medicaid, by = c("booking_date", "jail_dob_year", "jail_sex", "jail_race"))

################################################################################

# check dimensions of each file for adm_all (Carroll) vs carroll_medicaid

################################################################################

temp_medicaid <- carroll_medicaid %>% select(dhhs_id = unique_person_id,
                                             booking_date,
                                             jail_dob_year,
                                             jail_sex,
                                             jail_race) %>%
  mutate(jail_race = case_when(
    jail_race == "A" ~ "AAPI",
    jail_race == "B" ~ "Black",
    jail_race == "W" ~ "White",
    jail_race == "I" ~ "American Indian Alaska Native",
    jail_race == "U" ~ "Unknown"
  )) %>%
  mutate(jail_race = ifelse(jail_race == "Unknown", NA, jail_race))

temp_adm <- adm_all %>% filter(county == "Carroll") %>%
  select(id,
         booking_date,
         jail_dob_year = yob,
         jail_sex = gender,
         jail_race = race) %>%
  mutate(jail_sex = ifelse(jail_sex == 1, "F", "M"))

str(temp_medicaid)
str(temp_adm)

temp_joined <- merge(temp_adm, temp_medicaid, by = c("booking_date", "jail_dob_year", "jail_sex", "jail_race"), all.x = TRUE)
temp_joined_nas <- temp_joined %>% filter(is.na(dhhs_id)) %>% select(id) %>% distinct()
dim(temp_joined_nas)

################################################################################

# check dimensions of each file for cheshire_adm vs cheshire_medicaid

################################################################################

length(unique(cheshire_adm1$id))         # 2809
length(unique(cheshire_adm1$booking_id)) # 4069

length(unique(cheshire_medicaid$unique_person_id)) # 2475
length(unique(cheshire_medicaid$booking_id))       # 3799

temp_medicaid <- cheshire_medicaid %>% select(dhhs_id = unique_person_id,
                                              booking_date,
                                              jail_dob_year,
                                              jail_sex,
                                              jail_race) %>%
  mutate(jail_race = case_when(
    jail_race == "A" ~ "AAPI",
    jail_race == "B" ~ "Black",
    jail_race == "W" ~ "White",
    jail_race == "I" ~ "American Indian Alaska Native",
    jail_race == "U" ~ "Unknown",
    jail_race == "H" ~ "Hispanic",
    jail_race == "L" ~ "Hispanic",
    jail_race == "P" ~ "AAPI"
  )) %>%
  mutate(jail_race = ifelse(jail_race == "Unknown", NA, jail_race))

temp_adm <- cheshire_adm1 %>% select(id,
                                     booking_date,
                                     jail_dob_year = yob,
                                     jail_sex = gender,
                                     jail_race = race) %>%
  mutate(jail_sex = ifelse(jail_sex == 1, "F", "M"))

str(temp_medicaid)
str(temp_adm)

temp_joined <- merge(temp_adm, temp_medicaid, by = c("booking_date", "jail_dob_year", "jail_sex", "jail_race"))

################################################################################

# check dimensions of each file for adm_all (Cheshire) vs cheshire_medicaid

################################################################################

length(unique(cheshire_medicaid$unique_person_id)) # 2475
length(unique(cheshire_medicaid$booking_id))       # 3799

cheshire_data <- adm_all %>% filter(county == "Cheshire")
length(unique(cheshire_data$id))         # 2809
length(unique(cheshire_data$booking_id)) # 4069

temp_medicaid <- cheshire_medicaid %>% select(dhhs_id = unique_person_id,
                                              booking_date,
                                              jail_dob_year,
                                              jail_sex,
                                              jail_race) %>%
  mutate(jail_race = case_when(
    jail_race == "A" ~ "AAPI",
    jail_race == "B" ~ "Black",
    jail_race == "W" ~ "White",
    jail_race == "I" ~ "American Indian Alaska Native",
    jail_race == "U" ~ "Unknown",
    jail_race == "H" ~ "Hispanic",
    jail_race == "L" ~ "Hispanic",
    jail_race == "P" ~ "AAPI"
  )) %>%
  mutate(jail_race = ifelse(jail_race == "Unknown", NA, jail_race))

temp_adm <- adm_all %>% filter(county == "Cheshire") %>%
  select(id,
         booking_date,
         jail_dob_year = yob,
         jail_sex = gender,
         jail_race = race) %>%
  mutate(jail_sex = ifelse(jail_sex == 1, "F", "M"))

str(temp_medicaid)
str(temp_adm)

temp_joined <- merge(temp_adm, temp_medicaid, by = c("booking_date", "jail_dob_year", "jail_sex", "jail_race"), all.x = TRUE)
temp_joined_nas <- temp_joined %>% filter(is.na(dhhs_id)) %>% select(id) %>% distinct()
dim(temp_joined_nas) # 738 individuals in the CSG data that could not be matched
