
temp <- df %>%
  ungroup() %>%
  filter(hu_variable_name == yesno) %>%
  select(county,
         id,
         num_bookings) %>%
  distinct()

temp1 <- temp %>% group_by(county) %>%
  summarise(mean = mean(num_bookings))

county_exclusion_text <- "Coos (bookings only)"

########################################################################

# min, median, mean, and max of bookings/entrances
fnc_hus_descriptive_summary <- function(df, hu_variable_name, yesno, county_exclusion_text){

  #df$hu_variable_name <- get(hu_variable_name, df)

  ##########
  # HU People
  ##########

  df_id <- bookings_entrances %>%
    ungroup() %>%
    filter(high_utilizer_10_pct == "Yes") %>%
    select(county,
           id,
           num_bookings,
           high_utilizer_4_times,
           high_utilizer_1_pct,
           high_utilizer_5_pct,
           high_utilizer_10_pct) %>%
    distinct() %>%
    group_by(county) %>%
    summarise(total_hu_people  = n()) %>%
    mutate(county = case_when(county == "Coos" ~ county_exclusion_text, TRUE ~ county))

  df_id_total <- bookings_entrances %>%
    ungroup() %>%
    filter(high_utilizer_10_pct == "Yes") %>%
    select(county,
           id,
           num_bookings,
           high_utilizer_4_times,
           high_utilizer_1_pct,
           high_utilizer_5_pct,
           high_utilizer_10_pct) %>%
    distinct() %>%
    group_by() %>%
    summarise(total_hu_people  = n()) %>%
    mutate(county = "State")

  df_id <- rbind(df_id, df_id_total)

  ##########
  # HU ENTRANCES
  ##########

  df_booking_id <- bookings_entrances %>%
    ungroup() %>%
    filter(high_utilizer_10_pct == "Yes") %>%
    select(county,
           booking_id,
           num_bookings,
           high_utilizer_4_times,
           high_utilizer_1_pct,
           high_utilizer_5_pct,
           high_utilizer_10_pct) %>%
    distinct() %>%
    group_by(county) %>%
    summarise(total_hu_entrances = n()) %>%
    mutate(county = case_when(county == "Coos" ~ county_exclusion_text, TRUE ~ county))

  df_booking_id_total <- bookings_entrances %>%
    ungroup() %>%
    filter(high_utilizer_10_pct == "Yes") %>%
    select(county,
           booking_id,
           num_bookings,
           high_utilizer_4_times,
           high_utilizer_1_pct,
           high_utilizer_5_pct,
           high_utilizer_10_pct) %>%
    distinct() %>%
    group_by() %>%
    summarise(total_hu_entrances = n()) %>%
    mutate(county = "State")

  df_booking_id <- rbind(df_booking_id, df_booking_id_total)

  ##########
  # HU min median mean max range
  ##########

  df_summary <- bookings_entrances %>%
    ungroup() %>%
    filter(high_utilizer_10_pct == "Yes") %>%
    select(county, id, num_bookings) %>%
    distinct() %>%
    group_by(county) %>%
    summarise(min    = min(num_bookings, na.rm = T),
              median = median(num_bookings, na.rm = T),
              mean   = mean(num_bookings, na.rm = T),
              max    = max(num_bookings, na.rm = T)) %>%
    select(county, everything()) %>%
    mutate(county = case_when(county == "Coos" ~ county_exclusion_text, TRUE ~ county))

  df_summary_total <- bookings_entrances %>%
    ungroup() %>%
    filter(high_utilizer_10_pct == "Yes") %>%
    select(id, num_bookings) %>%
    distinct() %>%
    group_by() %>%
    summarise(min    = min(num_bookings, na.rm = T),
              median = median(num_bookings, na.rm = T),
              mean   = mean(num_bookings, na.rm = T),
              max    = max(num_bookings, na.rm = T)) %>%
    mutate(county = "State") %>%
    select(county, everything())

  df_summary <- rbind(df_summary, df_summary_total)

  table_final <- df_booking_id %>%
    left_join(df_id, by = "county") %>%
    left_join(df_summary, by = "county") %>%
    arrange(county %in% "State")

}
