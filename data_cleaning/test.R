# min, median, mean, and max of bookings/entrances
# select variables
# all counties included
hu_booking_entrances <- booking_entrances %>%
  ungroup() %>%
  filter(high_utilizer_4_times == "Yes") %>%
  select(fy,
         county,
         id,
         booking_id,
         num_bookings,
         los,
         los_category,
         pc_hold_in_booking,
         high_utilizer_4_times,
         high_utilizer_1_pct,
         high_utilizer_5_pct,
         high_utilizer_10_pct,
         month_year_text,
         month_year,
         race,
         age,
         age_category,
         gender) %>%
  distinct() %>%
  mutate(county = case_when(county == "Coos" ~ "Coos (bookings only)", TRUE ~ county))

dim(hu_booking_entrances); length(unique(hu_booking_entrances$booking_id)); length(unique(hu_booking_entrances$id))
# Total 51545, 51545, 32177
# HU's (4 or more bookings or entrances a year) 7009, 7009, 1055

# summary table showing min, median, mean, and max of bookings/entrances (including Coos)
county_num_bookings_entrances_summary <- fnc_summary_county(hu_booking_entrances, "num_bookings")
num_bookings_entrances_summary        <- fnc_summary(hu_booking_entrances, "num_bookings")
num_bookings_entrances_summary        <- num_bookings_entrances_summary %>% mutate(county = "State")
county_num_bookings_entrances_summary <- rbind(county_num_bookings_entrances_summary, num_bookings_entrances_summary)

# reactable table of summary table showing min, median, mean, and max of bookings/entrances (including Coos)
table_county_num_bookings_entrances_summary <- fnc_reactable_summary(county_num_bookings_entrances_summary, "County")
