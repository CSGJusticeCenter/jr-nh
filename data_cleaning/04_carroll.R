############################################
# Project: JRI New Hampshire
# File: carroll.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

###################
# Carroll County
###################

# Clean variable names
carroll_adm_all <- clean_names(carroll_bookings.xlsx)

# Not using releases for now because of merge issues
# carroll_releases <- clean_names(carroll_releases.xlsx)
# carroll_bookings <- clean_names(carroll_bookings.xlsx)
# Merge two adm files together
# carroll_adm_all <- merge(carroll_releases, carroll_bookings, by = c("inmate_id", "release_dt_tm"), all.x = TRUE)

# Change date formats for booking and release dataes
carroll_adm_all$booking_dt_tm <- .POSIXct(carroll_adm_all$booking_dt_tm, tz="UTC")
carroll_adm_all$booking_dt_tm <-   format(carroll_adm_all$booking_dt_tm, "%m/%d/%Y")
carroll_adm_all$booking_dt_tm <-  as.Date(carroll_adm_all$booking_dt_tm, format = "%m/%d/%Y")
carroll_adm_all$release_dt_tm <- .POSIXct(carroll_adm_all$release_dt_tm, tz="UTC")
carroll_adm_all$release_dt_tm <-   format(carroll_adm_all$release_dt_tm, "%m/%d/%Y")
carroll_adm_all$release_dt_tm <-  as.Date(carroll_adm_all$release_dt_tm, format = "%m/%d/%Y")

# Set up data to be consistent with other counties
carroll_adm_all <- carroll_bookings.xlsx %>%
  clean_names() %>%
  mutate(race_label = NA,
         release_type = NA) %>%
  dplyr::select(id = unique_person_id,
                inmate_id,
                yob,
                race_code = race,
                race_label,
                sex,
                housing,
                charge_code,
                charge_desc = charge,
                booking_date = booking_dt_tm,
                booking_type,
                release_date = release_dt_tm,
                release_type,
                sentence_status = sentencing_status) %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y"),
         county = "Carroll") %>%
  distinct()

dim(carroll_adm_all); length(unique(carroll_adm_all$inmate_id)) # 5402, 1849
