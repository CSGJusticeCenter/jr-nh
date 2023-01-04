
################################################################################

# check dimensions of each file for carroll_adm vs carroll_medicaid

################################################################################

length(unique(carroll_adm1$inmate_id))  # 1849
length(unique(carroll_adm1$booking_id)) # 3746


length(unique(carroll_medicaid $unique_person_id)) # 1825
length(unique(carroll_medicaid $booking_id))       # 2646


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

temp_joined <- merge(temp_adm, temp_medicaid, by = c("booking_date", "jail_dob_year", "jail_sex", "jail_race"))

################################################################################

# check dimensions of each file for cheshire_adm vs cheshire_medicaid

################################################################################

length(unique(cheshire_adm1$inmate_id))  # 2464
length(unique(cheshire_adm1$booking_id)) # 4069

length(unique(cheshire_medicaid$unique_person_id)) # 2693
length(unique(cheshire_medicaid$booking_id))       # 2727


4069 - 2727 #  1342


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

length(unique(cheshire_medicaid$unique_person_id)) # 2693
length(unique(cheshire_medicaid$booking_id))       # 2727

cheshire_data <- adm_all %>% filter(county == "Cheshire")
length(unique(cheshire_data$id))         # 2809
length(unique(cheshire_data$booking_id)) # 4069

4069-2727 # 1342

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

temp_joined <- merge(temp_adm, temp_medicaid, by = c("booking_date", "jail_dob_year", "jail_sex", "jail_race"))
