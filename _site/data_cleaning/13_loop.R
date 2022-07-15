############################################
# Project: JRI New Hampshire
# File: loop.R
# Last updated: July 15, 2022
# Author: Mari Roberts

# combine county files
# loop through counties to generate tables
############################################

# combine county data for large NH dataframe with all charge descriptions
nh_adm_all <- do.call("rbind", list(belknap_adm_all,
                                    carroll_adm_all,
                                    cheshire_adm_all,
                                    coos_adm_all,
                                    merrimack_adm_all,
                                    rockingham_adm_all))


# remove charge codes and duplicates
# keep sentence status
nh_adm <- nh_adm_all %>%
  dplyr::select(inmate_id,
                race,
                yob,
                age,
                sex,
                housing,
                sentence_status,
                booking_date,
                los,
                fy,
                num_bookings,
                high_utilizer,
                pc_hold,
                county) %>%
  distinct()

# remove charge codes and duplicates to get picture of cohort
# remove sentence status
nh_booking <- nh_adm_all %>%
  dplyr::select(inmate_id,
                race,
                yob,
                age,
                sex,
                housing,
                booking_date,
                booking_type,
                release_date,
                los,
                fy,
                num_bookings,
                high_utilizer,
                pc_hold,
                county) %>%
  distinct()

# sep by fiscal year
nh_adm_19 <- nh_adm %>% filter(fy == 2019)
nh_adm_20 <- nh_adm %>% filter(fy == 2020)
nh_adm_21 <- nh_adm %>% filter(fy == 2021)

# sep by fy year
nh_booking_19 <- nh_booking %>% filter(fy == 2019)
nh_booking_20 <- nh_booking %>% filter(fy == 2020)
nh_booking_21 <- nh_booking %>% filter(fy == 2021)

######
# High Utilizer proportion
######

counties <- nh_adm$county %>%
  unique() %>%
  sort()

nh_hu_prop_list <- map(.x = counties,  .f = function(x) {
  county_adm_19 <- nh_adm_19 %>% filter(county == x)
  county_adm_20 <- nh_adm_20 %>% filter(county == x)
  county_adm_21 <- nh_adm_21 %>% filter(county == x)
  df_hu <- fnc_hu_table(county_adm_19, county_adm_20, county_adm_21)
})
