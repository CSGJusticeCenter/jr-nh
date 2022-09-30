# create dataframe that includes charge descriptions
withlos <- nh_adm_all %>%
  dplyr::select(county,
                id,
                race,
                yob,
                age,
                gender,
                booking_id,
                booking_date,
                charge_code,
                charge_desc,
                booking_type,
                release_type,
                sentence_status,
                los,
                fy,
                num_bookings,
                high_utilizer_1_pct,
                high_utilizer_3_pct,
                high_utilizer_5_pct,
                pc_hold_booking,
                pc_hold_charge,
                pc_hold_sentence,
                pc_hold) %>%
  distinct()
dim(withlos) # 73206

# create dataframe that includes charge descriptions
withoutlos <- nh_adm_all %>%
  dplyr::select(county,
                id,
                race,
                yob,
                age,
                gender,
                booking_id,
                booking_date,
                charge_code,
                charge_desc,
                booking_type,
                release_type,
                sentence_status,
                #los,
                fy,
                num_bookings,
                high_utilizer_1_pct,
                high_utilizer_3_pct,
                high_utilizer_5_pct,
                pc_hold_booking,
                pc_hold_charge,
                pc_hold_sentence,
                pc_hold) %>%
  distinct()
dim(withoutlos) # 73185


withlos <- withlos %>% select(booking_id, los) %>% distinct()
length(unique(withlos$booking_id))
dim(withlos)
dups_los <- withlos[duplicated(withlos$booking_id)|duplicated(withlos$booking_id, fromLast=TRUE),]

df1 <- belknap_adm_all %>%
  mutate(fy = case_when(booking_date >= "2018-07-01" & booking_date <= "2019-06-30" ~ 2019,
                        booking_date >= "2019-07-01" & booking_date <= "2020-06-30" ~ 2020,
                        booking_date >= "2020-07-01" & booking_date <= "2021-06-30" ~ 2021),
         age = fy - yob,
         los = difftime(as.POSIXct(release_date), as.POSIXct(booking_date, tz="UTC"), units="days"),
         race_label = case_when(race_code == "A"  ~ "AAPI",
                                race_code == "C"  ~ "AAPI",
                                race_code == "P"  ~ "AAPI",
                                race_code == "K"  ~ "Black Hispanic",
                                race_code == "Asian/Pacific Islander" ~ "AAPI",


                                race_code == "B"  ~ "Black",
                                race_code == "Black" ~ "Black",

                                race_code == "H"  ~ "Hispanic",
                                race_code == "L"  ~ "Hispanic",

                                race_code == "I"  ~ "American Indian Alaska Native",
                                race_code == "American Indian/Alaskan Native" ~ "American Indian Alaska Native",

                                race_code == "U"  ~ "Unknown",
                                race_code == "NH" ~ "Unknown",
                                race_code == "N"  ~ "Unknown",
                                race_code == "X"  ~ "Unknown",
                                race_code == "Not Specified" ~ "Unknown",
                                race_code == "Unknown" ~ "Unknown",

                                race_code == "O"  ~ "Other",
                                race_code == "P"  ~ "Other",

                                race_code == "W"  ~ "White",
                                race_code == "White" ~ "White")) %>%
  filter(fy == 2019 | fy == 2020 | fy == 2021) %>%
  dplyr::select(id,
                inmate_id,
                yob,
                race_code,
                race = race_label,
                gender = sex,
                age,
                charge_code,
                charge_desc,
                booking_date,
                booking_type,
                release_date,
                release_type,
                sentence_status,
                los,
                county,
                fy)

df1 <- df1 %>%
  mutate(id = ifelse(is.na(id), inmate_id, id))
df1$booking_id <- df1 %>% group_indices(id, booking_date)
df1 <- df1 %>%
  mutate(id = paste(county, id, sep = "_"),
         booking_id = paste(county, "booking", booking_id, sep = "_")) %>%
  select(id, inmate_id, booking_id, everything())

temp <- df1 %>%
  group_by(booking_id) %>%
  summarise(los_new = max(los, na.rm=TRUE))

temp2 <- merge(temp, df1, by = "booking_id")











