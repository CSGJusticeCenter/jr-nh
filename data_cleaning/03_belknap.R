############################################
# Project: JRI New Hampshire
# File: belknap.R
# Last updated: June 1, 2022
# Author: Mari Roberts

# Standardize files across counties
# FY July 1, 2018 – June 30, 2021
############################################

###################
# Belknap County
###################

belknap_adm_all <- belknap_adm.xlsx %>%
  clean_names() %>%
  mutate(charge_code = NA,
         race_label = NA) %>%
  dplyr::select(id = unique_person_id,
                inmate_id,
                yob = year_of_birth,
                race_code = race,
                race_label,
                sex,
                housing = housing_instability_or_homelessness_indicator,
                charge_code,
                charge_desc = charged_offense_code_description_including_technical_violations_of_supervision,
                booking_date,
                booking_type,
                release_date,
                release_type,
                sentence_status = sentencing_status) %>%
  mutate(booking_date = as.Date(booking_date, format = "%m/%d/%Y"),
         release_date = as.Date(release_date, format = "%m/%d/%Y"),
         county = "Belknap") %>% distinct()

########
# Booking types
########

# PC holds are in the booking type and charge description
belknap_booking_type    <- as.data.frame(table(belknap_adm_all$booking_type))     # "PROTECTIVE CUSTODY"
belknap_release_type    <- as.data.frame(table(belknap_adm_all$release_type))     # No data
belknap_sentence_status <- as.data.frame(table(belknap_adm_all$sentence_status))  # No data
belknap_charge_desc     <- as.data.frame(table(belknap_adm_all$charge_desc))      # "PROTECTIVE CUSTODY", "PROTECTIVE CUSTODY/INTOXICATION"

df_belknap_pc_hold_recording_raw <- belknap_adm_all %>%
  filter_all(any_vars(grepl("PROTECTIVE CUSTODY", ., ignore.case = TRUE))) %>%
  group_by(booking_type, charge_desc, release_type, sentence_status) %>%
  dplyr::summarise(total = n()) %>%
  distinct() %>%
  arrange(-total)

belknap_pc_hold_recording_raw <- reactable(df_belknap_pc_hold_recording_raw,
                                           pagination = FALSE,
                                           theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
                                           defaultColDef = reactable::colDef(format = colFormat(separators = TRUE), align = "left"),
                                           compact = TRUE,
                                           fullWidth = FALSE,
                                           columns = list(
                                             booking_type    = colDef(minWidth = 250, name = "Booking Type"),
                                             charge_desc     = colDef(minWidth = 250, name = "Charge Description"),
                                             release_type    = colDef(minWidth = 250, name = "Release Type"),
                                             sentence_status = colDef(minWidth = 250, name = "Sentence Status"),
                                             total           = colDef(minWidth = 75,  name = "Total")))


# save raw PC hold recordings table
save(belknap_pc_hold_recording_raw, file=paste0(sp_data_path, "/Data/r_data/belknap_pc_hold_recording_raw.Rda", sep = ""))
