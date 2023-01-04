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
