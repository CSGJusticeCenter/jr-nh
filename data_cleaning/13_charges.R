############################################
# Project: JRI New Hampshire
# File:  charges.R
# Last updated: November 8, 2022
# Author: Andrew Byrum

# Explore charge data for each jail -- attempting to clean and separate charge code from charge description
# Then join cleaned charge data with charge codes file to see how much coverage we have/how much manual coding is required
# Ultimate need is to link charge codes/descriptions to charge classes (misdemeanor/felony/etc) and charge category (violent, drug, public peace, etc.)

### TO DO: 

# - START WITH adm_all.rda INSTEAD OF INDIVIDUAL COUNTY FILES B/C MARI CLEANED UP CHARGE/PC HOLD FLAGS AFTER JOINING COUNTIES TOGETHER

# - DECIDE HOW TO HANDLE DUPLICATE CHARGES IN LOOKUP FILE -- WITH SAME CODES AND DATES, BUT DIFFERENT DEGREES
# Should we choose the most serious or least serious? 

# - Clarify with superintendents/jail partners whether charge data represent most serious charge linked to bookings
# aside from hillsborough where all charges appear to be entered along with a booking

# - Explore other resources to use for records missing charge codes

# - Join all counties together and then fill across lookup table values using charge description 
# (i.e., to fill in charge codes and charge lookup table values using descriptions that did have a matching code linked)

# - Fix rbind issue (columns don't match) for join_1 and join_2 dataframes

# - Missing charge code/charge description data by county. How should we interpret this? 

# - De-dup hillsborough by most serious charge once all charges are cleaned

# - Is there a way to code charge type (public order, property, drugs/alcohol, violent) using existing codes?


################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Use cleaned and standardized county-level dataframes created in `12_dataframes.R`
load(paste0(sp_data_path, "/Data/r_data/belknap_adm.Rda",           sep = ""))
load(paste0(sp_data_path, "/Data/r_data/carroll_adm.Rda",           sep = ""))
load(paste0(sp_data_path, "/Data/r_data/cheshire_adm.Rda",          sep = ""))
load(paste0(sp_data_path, "/Data/r_data/coos_adm.Rda",              sep = ""))
load(paste0(sp_data_path, "/Data/r_data/hillsborough_adm.Rda",      sep = ""))
load(paste0(sp_data_path, "/Data/r_data/merrimack_adm.Rda",         sep = ""))
load(paste0(sp_data_path, "/Data/r_data/rockingham_adm.Rda",        sep = ""))
load(paste0(sp_data_path, "/Data/r_data/strafford_adm.Rda",         sep = ""))
load(paste0(sp_data_path, "/Data/r_data/sullivan_adm.Rda",          sep = ""))

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

##################################
# Charge Lookup Table
##################################
charge_codes_lookup <- charge_codes.xlsx %>%
  clean_names() %>%
  mutate(descriptor = tolower(descriptor),
         statute_title = tolower(statute_title),
         charge_code_lookup = tolower(offense_statute)) %>% 
### first de-dup: where there are pure duplicates, keep record with most recent eff_date
  arrange(charge_code_lookup,desc(eff_date)) %>% 
  distinct(charge_code_lookup,
           degree,
           .keep_all = TRUE) %>% 
  select(charge_code_lookup, descriptor, statute_title, smart_code, ctl_number, vis, degree) %>% 
### clean up degree names for eventual tables or visualization
### second de-up: 
### DECIDE HOW TO HANDLE DUPLICATE CHARGES IN LOOKUP FILE -- WITH SAME CODES AND DATES, BUT DIFFERENT DEGREES
### for now, I'm keeping the most serious charge when there are pure duplicates
  mutate(degree_clean = case_when(
    degree=="FA" ~ "A Felony",
    degree=="FB" ~ "B Felony",
    degree=="FS" ~ "FS Felony",
    degree=="FU" ~ "FU Felony",
    degree=="MA" ~ "A Misdemeanor",
    degree=="MB" ~ "B Misdemeanor",
    degree=="V" ~ "Violation",### cleaning degree classes: https://www.russmanlaw.com/new-hampshire-classification-of-offenses
    TRUE ~ as.character(NA)), ### still need clarification on severity of "FS" and "FU"
  degree_severity_order = case_when(
    degree_clean=="A Felony" ~ 1,
    degree_clean=="B Felony" ~ 2,
    degree_clean=="FS Felony" ~ 3,
    degree_clean=="FU Felony" ~ 4,
    degree_clean=="A Misdemeanor" ~ 5,
    degree_clean=="B Misdemeanor" ~ 6,
    degree_clean=="Violation" ~ 7,### ordering degree classes in order of severity: https://www.russmanlaw.com/new-hampshire-classification-of-offenses
      TRUE ~ as.numeric(NA))) %>%  
      arrange(charge_code_lookup, 
              degree_severity_order) %>% ### here we are de-duping by charge code, keeping the record with the most serious offense conviction linked
      distinct(charge_code_lookup,
               .keep_all=TRUE)
  

##################################
# Belknap
##################################
belknap_adm_charge_clean <- belknap_adm1 %>% 
  separate(charge_desc, 
           into = c("charge_desc_clean","charge_code_clean"), 
           sep = "(?<=[a-zA-Z])\\s*(?=[0-9])",
           remove = FALSE) %>% 
  mutate(charge_desc_clean = tolower(charge_desc_clean),
         charge_code_clean = tolower(charge_code_clean)) %>% 
# fill charge code by charge descriptions (some descriptions are missing a code where others have one)
  group_by(charge_desc_clean) %>% 
  fill(charge_code_clean, 
       .direction = "downup") %>%
  ungroup()

### drop PC holds, join to charge lookup table, and see what % of non-pc hold bookings join to lookup table
belknap_adm_charge_clean_join_one <- belknap_adm_charge_clean %>% 
  filter(pc_hold=="Non-PC Hold") %>% 
  left_join(charge_codes_lookup, 
            by = c("charge_code_clean"="charge_code_lookup")) %>% 
  mutate(missing_charge_data_first_join = ifelse(is.na(smart_code),
                                      1,
                                      0))

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(belknap_adm_charge_clean_join_one$missing_charge_data_first_join) ### only missing clean charge data for 1110 of 41811 records

### here's a list of the jail-entered charge descriptions where no charge code and/or clean charge data are available
table(belknap_adm_charge_clean_join_one$charge_desc_clean[belknap_adm_charge_clean_join_one$missing_charge_data_first_join==1])

### for non-pc hold bookings that did not join to the charge lookup table, either because there isn't a charge code
### in the jail-provided file or because the charge code in the jail-provided file doesn't match any charge code in 
### lookup
belknap_adm_charge_clean_join_two <- belknap_adm_charge_clean_join_one %>% 
  filter(missing_charge_data_first_join == 1) %>% 
  select(-c(descriptor, 
            smart_code, 
            ctl_number, 
            vis, 
            degree, 
            degree_clean, 
            degree_severity_order)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) when joining back  left_join(charge_codes_lookup, 
  left_join(charge_codes_lookup, 
            by = c("charge_desc_clean" = "statute_title")) %>% 
  mutate(missing_charge_data_second_join = ifelse(is.na(smart_code),
                                                 1,
                                                 0))

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(belknap_adm_charge_clean_join_two$missing_charge_data_second_join) ### only recovered 39 of 1110 records

### append belknap_adm_charge_clean_test_join_two back to belknap_adm_charge_clean_test_join_one
belknap_adm_charge_clean_join_one_final <- belknap_adm_charge_clean_join_one %>% 
  filter(missing_charge_data_first_join==0) %>% 
  dplyr::select(-missing_charge_data_first_join)

### clean second file for rbind
belknap_adm_charge_clean_join_two_final <- belknap_adm_charge_clean_join_two %>% 
  dplyr::select(-c(missing_charge_data_first_join,
                   missing_charge_data_second_join))

### combine df's from first and second attempts to clean
belknap_adm_charge_clean_final <- rbind(belknap_adm_charge_clean_join_one_final,belknap_adm_charge_clean_join_two)

### final tally for belknap: missing 1,073 of 41,811 non-pc hold records


##################################
# Carroll
##################################
carroll_adm_charge_clean <- carroll_adm1 %>% 
  mutate(charge_desc_clean = tolower(charge_desc),
         charge_code_clean = tolower(charge_code)) %>% 
# fill charge code by charge descriptions (some descriptions are missing a code where others have one)
  group_by(charge_desc_clean) %>% 
  fill(charge_code_clean, 
       .direction = "downup") %>%
  ungroup()

### drop PC holds, join to charge lookup table, and see what % of non-pc hold bookings join to lookup table
carroll_adm_charge_clean_join_one <- carroll_adm_charge_clean %>% 
  filter(pc_hold=="Non-PC Hold") %>% 
  left_join(charge_codes_lookup, 
            by = c("charge_code_clean"="charge_code_lookup")) %>% 
  mutate(missing_charge_data_first_join = ifelse(is.na(smart_code),
                                                 1,
                                                 0))

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(carroll_adm_charge_clean_join_one$missing_charge_data_first_join) ### missing clean charge data for 2646 records

### here's a list of the jail-entered charge descriptions where no charge code and/or clean charge data are available
table(carroll_adm_charge_clean_join_one$charge_desc_clean[carroll_adm_charge_clean_join_one$missing_charge_data_first_join==1])

### for non-pc hold bookings that did not join to the charge lookup table, either because there isn't a charge code
### in the jail-provided file or because the charge code in the jail-provided file doesn't match any charge code in 
### lookup
carroll_adm_charge_clean_join_two <- carroll_adm_charge_clean_join_one %>% 
  filter(missing_charge_data_first_join == 1) %>% 
  select(-c(descriptor, 
            smart_code, 
            ctl_number, 
            vis, 
            degree, 
            degree_clean, 
            degree_severity_order)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) when joining back
  left_join(charge_codes_lookup, 
            by = c("charge_desc_clean" = "statute_title")) %>% 
  mutate(missing_charge_data_second_join = ifelse(is.na(smart_code),
                                                  1,
                                                  0))

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(carroll_adm_charge_clean_join_two$missing_charge_data_second_join) ### recovered ~2370 of 2646 records (there are dupes with the joins)

### append second file back to first file

### clean first file for rbind
carroll_adm_charge_clean_join_one_final <- carroll_adm_charge_clean_join_one %>% 
  filter(missing_charge_data_first_join==0) %>% 
  dplyr::select(-missing_charge_data_first_join)

### clean second file for rbind
carroll_adm_charge_clean_join_two_final <- carroll_adm_charge_clean_join_two %>% 
  dplyr::select(-c(missing_charge_data_first_join,
                   missing_charge_data_second_join))

### combine df's from first and second attempts to clean
carroll_adm_charge_clean_final <- rbind(carroll_adm_charge_clean_join_one_final,carroll_adm_charge_clean_join_two_final)

### final tally for carroll: missing 1,634 non-pc hold records

##################################
# Cheshire
##################################
cheshire_adm_charge_clean <- cheshire_adm1 %>% 
  mutate(charge_desc_clean = tolower(charge_desc),
         charge_code_clean = tolower(charge_code)) %>% 
# fill charge code by charge descriptions (some descriptions are missing a code where others have one)
  group_by(charge_desc_clean) %>% 
  fill(charge_code_clean, 
       .direction = "downup") %>%
  ungroup()

### drop PC holds, join to charge lookup table, and see what % of non-pc hold bookings join to lookup table
cheshire_adm_charge_clean_join_one <- cheshire_adm_charge_clean %>% 
  filter(pc_hold=="Non-PC Hold") %>% 
  left_join(charge_codes_lookup, 
            by = c("charge_code_clean"="charge_code_lookup")) %>% 
  mutate(missing_charge_data_first_join = ifelse(is.na(smart_code),
                                                 1,
                                                 0))

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(cheshire_adm_charge_clean_join_one$missing_charge_data_first_join) ### missing clean charge data for 2646 records

### here's a list of the jail-entered charge descriptions where no charge code and/or clean charge data are available
table(cheshire_adm_charge_clean_join_one$charge_desc_clean[cheshire_adm_charge_clean_join_one$missing_charge_data_first_join==1])

### for non-pc hold bookings that did not join to the charge lookup table, either because there isn't a charge code
### in the jail-provided file or because the charge code in the jail-provided file doesn't match any charge code in 
### lookup
cheshire_adm_charge_clean_join_two <- cheshire_adm_charge_clean_join_one %>% 
  filter(missing_charge_data_first_join == 1) %>% 
  select(-c(descriptor, 
            smart_code, 
            ctl_number, 
            vis, 
            degree, 
            degree_clean, 
            degree_severity_order)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) when joining back  left_join(charge_codes_lookup, 
  left_join(charge_codes_lookup, 
            by = c("charge_desc_clean" = "statute_title")) %>% 
  mutate(missing_charge_data_second_join = ifelse(is.na(smart_code),
                                                  1,
                                                  0))

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(cheshire_adm_charge_clean_join_two$missing_charge_data_second_join) ### recovered ~2370 of 2646 records (there are dupes with the joins)

### append second file back to first file

### clean first file for rbind
cheshire_adm_charge_clean_join_one_final <- cheshire_adm_charge_clean_join_one %>% 
  filter(missing_charge_data_first_join==0) %>% 
  dplyr::select(-missing_charge_data_first_join)

### clean second file for rbind
cheshire_adm_charge_clean_join_two_final <- cheshire_adm_charge_clean_join_two %>% 
  dplyr::select(-c(missing_charge_data_first_join,
                   missing_charge_data_second_join))

### combine df's from first and second attempts to clean
cheshire_adm_charge_clean_final <- rbind(cheshire_adm_charge_clean_join_one_final,cheshire_adm_charge_clean_join_two_final)

### final tally for cheshire: missing 1,711 non-pc hold records

##################################
# Coos
##################################
coos_adm_charge_clean <- coos_adm1 %>% 
  mutate(charge_desc_clean = tolower(charge_desc),
         charge_code_clean = tolower(charge_code)) %>% 
# fill charge code by charge descriptions (some descriptions are missing a code where others have one)
  group_by(charge_desc_clean) %>% 
  fill(charge_code_clean, 
       .direction = "downup") %>%
  ungroup()

### drop PC holds, join to charge lookup table, and see what % of non-pc hold bookings join to lookup table
coos_adm_charge_clean_join_one <- coos_adm_charge_clean %>% 
  filter(pc_hold=="Non-PC Hold") %>% 
  left_join(charge_codes_lookup, 
            by = c("charge_code_clean"="charge_code_lookup")) %>% 
  mutate(missing_charge_data_first_join = ifelse(is.na(smart_code),
                                                 1,
                                                 0))

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(coos_adm_charge_clean_join_one$missing_charge_data_first_join) ### missing clean charge data for 2646 records

### here's a list of the jail-entered charge descriptions where no charge code and/or clean charge data are available
table(coos_adm_charge_clean_join_one$charge_desc_clean[coos_adm_charge_clean_join_one$missing_charge_data_first_join==1])

### for non-pc hold bookings that did not join to the charge lookup table, either because there isn't a charge code
### in the jail-provided file or because the charge code in the jail-provided file doesn't match any charge code in 
### lookup
coos_adm_charge_clean_join_two <- coos_adm_charge_clean_join_one %>% 
  filter(missing_charge_data_first_join == 1) %>% 
  select(-c(descriptor, 
            smart_code, 
            ctl_number, 
            vis, 
            degree, 
            degree_clean, 
            degree_severity_order)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) when joining back  left_join(charge_codes_lookup, 
  left_join(charge_codes_lookup, 
            by = c("charge_desc_clean" = "statute_title")) %>% 
  mutate(missing_charge_data_second_join = ifelse(is.na(smart_code),
                                                  1,
                                                  0))

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(coos_adm_charge_clean_join_two$missing_charge_data_second_join) ### recovered ~2370 of 2646 records (there are dupes with the joins)

### append second file back to first file

### clean first file for rbind
coos_adm_charge_clean_join_one_final <- coos_adm_charge_clean_join_one %>% 
  filter(missing_charge_data_first_join==0) %>% 
  dplyr::select(-missing_charge_data_first_join)

### clean second file for rbind
coos_adm_charge_clean_join_two_final <- coos_adm_charge_clean_join_two %>% 
  dplyr::select(-c(missing_charge_data_first_join,
                   missing_charge_data_second_join))

### combine df's from first and second attempts to clean
coos_adm_charge_clean_final <- rbind(coos_adm_charge_clean_join_one_final,coos_adm_charge_clean_join_two_final)

### final tally for coos: missing 391 non-pc hold records

##################################
# Hillsborough
##################################
hillsborough_adm_charge_clean <- hillsborough_adm1 %>% 
  separate(charge_desc, 
           paste("charge_desc", 1:11, sep="_"), 
           sep=";", 
           extra="drop",
           remove=FALSE) %>% ### 11 charges is the most included in charge_desc
  mutate(across(charge_desc_1:charge_desc_11, str_trim)) %>% 
  pivot_longer(charge_desc_1:charge_desc_11, 
               names_to = "charge_desc_first_clean_count", 
               values_to = "charge_desc_first_clean_value") %>% ### pivot wide to long for easier cleaning
### need to keep first code, drop second code, and tolower across all new charge_code and charge_desc cols
  separate(charge_desc_first_clean_value,
           paste("charge_desc_second_clean_value", 1:2, sep="_"), 
           sep=" - ", ### adding space to either side as some charge codes include dashes without spaces
           extra="drop",
           remove=FALSE) %>% 
### remove duplicate charge codes from charge_desc_second_clean_value_2
## remove everything after first occurrence of consecutive numbers 
## (this helps avoid removing charge descriptions with numbers like "2nd degree...") 
  mutate(charge_desc_second_clean_value_2 = str_remove(charge_desc_second_clean_value_2, "[0-9][0-9].+")) %>% 
### manually clean values that regex didn't properly clean
  mutate(charge_desc_second_clean_value_2 = case_when(
    charge_desc_second_clean_value_1=="LITTER CONTROL" ~ "LITTER CONTROL",
    charge_desc_second_clean_value_1=="263:1 - 263:1  - LICENSE REQUIRED" ~ "LICENSE REQUIRED",
    charge_desc_second_clean_value_1=="MV DRIVERS SCHOOL 263:44" ~ "MV DRIVERS SCHOOL",
    charge_desc_second_clean_value_1=="Eletronic Bench Warrant" ~ "Electronic Bench Warrant",
    charge_desc_second_clean_value_1=="DUTY TO INFORM 651" ~ "DUTY TO INFORM",
    charge_desc_second_clean_value_1=="DUTY OF GOVERNOR 612:2" ~ "DUTY OF GOVERNOR",
    TRUE ~ as.character(charge_desc_second_clean_value_2))) %>% 
  mutate(charge_desc_second_clean_value_1_num_only = case_when(
    charge_desc_second_clean_value_1=="LITTER CONTROL" ~ as.character(NA),
    charge_desc_second_clean_value_1=="PENALTY 262:40" ~ "262:40-C",
    charge_desc_second_clean_value_1=="MV DRIVERS SCHOOL 263:44" ~ "263:44",
    charge_desc_second_clean_value_1=="Eletronic Bench Warrant" ~ as.character(NA),
    charge_desc_second_clean_value_1=="DUTY TO INFORM 651" ~ "651-B:5",
    charge_desc_second_clean_value_1=="DUTY OF GOVERNOR 612:2" ~ "612:2",
    TRUE ~ as.character(charge_desc_second_clean_value_1))) %>% 
### again remove leading and trailing blanks
  mutate(across(charge_desc_second_clean_value_1:charge_desc_second_clean_value_2, str_trim)) %>% 
  mutate(charge_desc_clean = tolower(charge_desc_second_clean_value_2),
         charge_code_clean = tolower(charge_desc_second_clean_value_1_num_only)) %>% 
  ### de-dup by individual, booking, and charge since we have pivoted wide to long and created duplicates
  distinct(id,
           inmate_id,
           booking_id,
           booking_date,
           charge_code_clean,
           charge_desc_clean,
           .keep_all = TRUE) 

### drop PC holds, join to charge lookup table, and see what % of non-pc hold bookings join to lookup table
  
### will need to group by booking and only keep most serious charge
### however, we can't de-dup by most serious charge until we've code all missing charges
hillsborough_adm_charge_clean_join_one <- hillsborough_adm_charge_clean %>% 
  filter(pc_hold=="Non-PC Hold") %>% 
  left_join(charge_codes_lookup, 
            by = c("charge_code_clean"="charge_code_lookup")) %>% 
  mutate(missing_charge_data_first_join = ifelse(is.na(smart_code),
                                                 1,
                                                 0))
### TO DO
### code to eventually use to de-dup by most serious charge 
  # arrange(id,
  #         booking_id,
  #         degree_severity_order) %>% ### here we are de-duping by supervision case, keeping the record with the most serious offense conviction linked
  # distinct(id,
  #          booking_id,
  #          .keep_all=TRUE)

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(hillsborough_adm_charge_clean_join_one$missing_charge_data_first_join) ### missing clean charge data for 2646 records

### here's a list of the jail-entered charge descriptions where no charge code and/or clean charge data are available
table(hillsborough_adm_charge_clean_join_one$charge_desc_clean[hillsborough_adm_charge_clean_join_one$missing_charge_data_first_join==1])

### for non-pc hold bookings that did not join to the charge lookup table, either because there isn't a charge code
### in the jail-provided file or because the charge code in the jail-provided file doesn't match any charge code in 
### lookup
hillsborough_adm_charge_clean_join_two <- hillsborough_adm_charge_clean_join_one %>% 
  filter(missing_charge_data_first_join == 1) %>% 
  select(-c(descriptor, 
            smart_code, 
            ctl_number, 
            vis, 
            degree, 
            degree_clean, 
            degree_severity_order,
            charge_desc_first_clean_count:charge_desc_second_clean_value_1_num_only)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) when joining back  left_join(charge_codes_lookup, 
  left_join(charge_codes_lookup, 
            by = c("charge_desc_clean" = "statute_title")) %>% 
  mutate(missing_charge_data_second_join = ifelse(is.na(smart_code),
                                                  1,
                                                  0))

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(hillsborough_adm_charge_clean_join_two$missing_charge_data_second_join) ### recovered ~2370 of 2646 records (there are dupes with the joins)

### append second file back to first file

### clean first file for rbind
hillsborough_adm_charge_clean_join_one_final <- hillsborough_adm_charge_clean_join_one %>% 
  filter(missing_charge_data_first_join==0) %>% 
  dplyr::select(-c(missing_charge_data_first_join,charge_desc_first_clean_count:charge_desc_second_clean_value_1_num_only))

### clean second file for rbind
hillsborough_adm_charge_clean_join_two_final <- hillsborough_adm_charge_clean_join_two %>% 
  dplyr::select(-c(missing_charge_data_first_join,
                   missing_charge_data_second_join))

### combine df's from first and second attempts to clean
hillsborough_adm_charge_clean_final <- rbind(hillsborough_adm_charge_clean_join_one_final,
                                             hillsborough_adm_charge_clean_join_two_final)

### final tally for hillsborough: missing 391 non-pc hold records

##################################
# Merrimack
##################################

### NOTE: MERRIMACK DOESN'T HAVE ANY CHARGE CODES -- SO WE HAVE TO RELY ON CHARGE DESCRIPTIONS

merrimack_adm_charge_clean <- merrimack_adm1 %>% 
  mutate(charge_desc_clean = tolower(charge_desc),
         charge_code_clean = tolower(charge_code)) 

### drop PC holds, join to charge lookup table, and see what % of non-pc hold bookings join to lookup table
merrimack_adm_charge_clean_join_one <- merrimack_adm_charge_clean %>% 
  filter(pc_hold=="Non-PC Hold") %>% 
  left_join(charge_codes_lookup, 
            by = c("charge_desc_clean" = "statute_title")) %>% 
  mutate(missing_charge_data_first_join = ifelse(is.na(smart_code),
                                                 1,
                                                 0),
         statute_title = charge_desc_clean) ### create statute_title column for eventual rbind with all other county dataframes) 

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(merrimack_adm_charge_clean_join_one$missing_charge_data_first_join) ### missing clean charge data for 6,000+ records

### here's a list of the jail-entered charge descriptions where no charge code and/or clean charge data are available
table(merrimack_adm_charge_clean_join_one$charge_desc_clean[merrimack_adm_charge_clean_join_one$missing_charge_data_first_join==1])

### for non-pc hold bookings that did not join to the charge lookup table, either because there isn't a charge code
### in the jail-provided file or because the charge code in the jail-provided file doesn't match any charge code in 
### lookup
merrimack_adm_charge_clean_join_two <- merrimack_adm_charge_clean_join_one %>% 
  filter(missing_charge_data_first_join == 1) %>% 
  select(-c(descriptor, 
            smart_code, 
            ctl_number, 
            vis, 
            degree, 
            degree_clean, 
            charge_code_lookup,
            degree_severity_order)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) when joining back  left_join(charge_codes_lookup, 
  left_join(charge_codes_lookup, 
            by = c("charge_desc_clean" = "statute_title")) %>% 
  mutate(missing_charge_data_second_join = ifelse(is.na(smart_code),
                                                  1,
                                                  0),
         statute_title = charge_desc_clean) ### create statute_title column for eventual rbind with all other county dataframes

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(merrimack_adm_charge_clean_join_two$missing_charge_data_second_join) ### recovered ~200 of 6,000+ records (there are dupes with the joins)

### append second file back to first file

### clean first file for rbind
merrimack_adm_charge_clean_join_one_final <- merrimack_adm_charge_clean_join_one %>% 
  filter(missing_charge_data_first_join==0) %>% 
  dplyr::select(-missing_charge_data_first_join)

### clean second file for rbind
merrimack_adm_charge_clean_join_two_final <- merrimack_adm_charge_clean_join_two %>% 
  dplyr::select(-c(missing_charge_data_first_join,
                   missing_charge_data_second_join))

### combine df's from first and second attempts to clean
merrimack_adm_charge_clean_final <- rbind(merrimack_adm_charge_clean_join_one_final,
                                          merrimack_adm_charge_clean_join_two_final)

### final tally for merrimack: missing 6,461 non-pc hold records

##################################
# Rockingham
##################################
rockingham_adm_charge_clean <- rockingham_adm1 %>% 
  mutate(charge_desc_clean = tolower(charge_desc),
         charge_code_clean = tolower(charge_code)) %>% 
# fill charge code by charge descriptions (some descriptions are missing a code where others have one)
  group_by(charge_desc_clean) %>% 
  fill(charge_code_clean, 
       .direction = "downup") %>%
  ungroup()

### drop PC holds, join to charge lookup table, and see what % of non-pc hold bookings join to lookup table
rockingham_adm_charge_clean_join_one <- rockingham_adm_charge_clean %>% 
  filter(pc_hold=="Non-PC Hold") %>% 
  left_join(charge_codes_lookup, 
            by = c("charge_code_clean"="charge_code_lookup")) %>% 
  mutate(missing_charge_data_first_join = ifelse(is.na(smart_code),
                                                 1,
                                                 0))

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(rockingham_adm_charge_clean_join_one$missing_charge_data_first_join) ### missing clean charge data for ~5500 records

### here's a list of the jail-entered charge descriptions where no charge code and/or clean charge data are available
table(rockingham_adm_charge_clean_join_one$charge_desc_clean[rockingham_adm_charge_clean_join_one$missing_charge_data_first_join==1])

### for non-pc hold bookings that did not join to the charge lookup table, either because there isn't a charge code
### in the jail-provided file or because the charge code in the jail-provided file doesn't match any charge code in 
### lookup
rockingham_adm_charge_clean_join_two <- rockingham_adm_charge_clean_join_one %>% 
  filter(missing_charge_data_first_join == 1) %>% 
  select(-c(descriptor, 
            smart_code, 
            ctl_number, 
            vis, 
            degree, 
            degree_clean, 
            degree_severity_order)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) when joining back  left_join(charge_codes_lookup, 
  left_join(charge_codes_lookup, 
            by = c("charge_desc_clean" = "statute_title")) %>% 
  mutate(missing_charge_data_second_join = ifelse(is.na(smart_code),
                                                  1,
                                                  0))

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(rockingham_adm_charge_clean_join_two$missing_charge_data_second_join) ### recovered ~1700 of 5500 records (there are dupes with the joins)

### append second file back to first file

### clean first file for rbind
rockingham_adm_charge_clean_join_one_final <- rockingham_adm_charge_clean_join_one %>% 
  filter(missing_charge_data_first_join==0) %>% 
  dplyr::select(-missing_charge_data_first_join)

### clean second file for rbind
rockingham_adm_charge_clean_join_two_final <- rockingham_adm_charge_clean_join_two %>% 
  dplyr::select(-c(missing_charge_data_first_join,
                   missing_charge_data_second_join))

### combine df's from first and second attempts to clean
rockingham_adm_charge_clean_final <- rbind(rockingham_adm_charge_clean_join_one_final,
                                           rockingham_adm_charge_clean_join_two_final)

### final tally for rockingham: missing 2,826 non-pc hold records

##################################
# Strafford
##################################

### NOTE: Strafford is missing both charge codes and charge descriptions. Unless we receive updated data,
### we'll have to exclude Strafford from all charge-specific analysis

##################################
# Sullivan
##################################
sullivan_adm_charge_clean <- sullivan_adm1 %>% 
  mutate(charge_desc_clean = tolower(charge_desc),
         charge_code_clean = tolower(charge_code)) %>% 
# fill charge code by charge descriptions (some descriptions are missing a code where others have one)
  group_by(charge_desc_clean) %>% 
  fill(charge_code_clean, 
       .direction = "downup") %>%
  ungroup()

### drop PC holds, join to charge lookup table, and see what % of non-pc hold bookings join to lookup table
sullivan_adm_charge_clean_join_one <- sullivan_adm_charge_clean %>% 
  filter(pc_hold=="Non-PC Hold") %>% 
  left_join(charge_codes_lookup, 
            by = c("charge_code_clean"="charge_code_lookup")) %>% 
  mutate(missing_charge_data_first_join = ifelse(is.na(smart_code),
                                                 1,
                                                 0))

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(sullivan_adm_charge_clean_join_one$missing_charge_data_first_join) ### missing clean charge data for ~1200 records

### here's a list of the jail-entered charge descriptions where no charge code and/or clean charge data are available
table(sullivan_adm_charge_clean_join_one$charge_desc_clean[sullivan_adm_charge_clean_join_one$missing_charge_data_first_join==1])

### for non-pc hold bookings that did not join to the charge lookup table, either because there isn't a charge code
### in the jail-provided file or because the charge code in the jail-provided file doesn't match any charge code in 
### lookup
sullivan_adm_charge_clean_join_two <- sullivan_adm_charge_clean_join_one %>% 
  filter(missing_charge_data_first_join == 1) %>% 
  select(-c(descriptor, 
            smart_code, 
            ctl_number, 
            vis, 
            degree, 
            degree_clean, 
            degree_severity_order)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) when joining back  left_join(charge_codes_lookup, 
  left_join(charge_codes_lookup, 
            by = c("charge_desc_clean" = "statute_title")) %>% 
  mutate(missing_charge_data_second_join = ifelse(is.na(smart_code),
                                                  1,
                                                  0))

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(sullivan_adm_charge_clean_join_two$missing_charge_data_second_join) ### recovered ~44 of 1200 records (there are dupes with the joins)

### append second file back to first file

### clean first file for rbind
sullivan_adm_charge_clean_join_one_final <- sullivan_adm_charge_clean_join_one %>% 
  filter(missing_charge_data_first_join==0) %>% 
  dplyr::select(-missing_charge_data_first_join)

### clean second file for rbind
sullivan_adm_charge_clean_join_two_final <- sullivan_adm_charge_clean_join_two %>% 
  dplyr::select(-c(missing_charge_data_first_join,
                   missing_charge_data_second_join))

### combine df's from first and second attempts to clean
sullivan_adm_charge_clean_final <- rbind(sullivan_adm_charge_clean_join_one_final,
                                         sullivan_adm_charge_clean_join_two_final)

### final tally for sullivan: missing 2,826 non-pc hold records

################################################################################
# Extract all charge codes and charge descriptions from records that didn't join
### Compile list of unique codes and descriptions for (possible) manual coding
################################################################################
nh_eight_county_charge_join_missing <- rbind(belknap_adm_charge_clean_join_two,
                                             carroll_adm_charge_clean_join_two, 
                                             cheshire_adm_charge_clean_join_two,
                                             coos_adm_charge_clean_join_two,
                                             hillsborough_adm_charge_clean_join_two,
                                             merrimack_adm_charge_clean_join_two,
                                             rockingham_adm_charge_clean_join_two,
                                             sullivan_adm_charge_clean_join_two) %>% 
  filter(missing_charge_data_second_join==1) %>% 
  ### fill charge code by charge descriptions (some descriptions are missing a code where others have one)
  group_by(charge_desc_clean) %>%
  fill(charge_code_clean,
         .direction = "downup") %>%
  ungroup() %>% 
  ### recode NAs as "Missing" with charge codes and descriptions for frequency table
  mutate(charge_code_clean = ifelse(is.na(charge_code_clean),"Missing",charge_code_clean),
         charge_desc_clean = ifelse(is.na(charge_desc_clean),"Missing",charge_desc_clean)) 



### make frequency table with charge codes/descriptions that did not link to lookup table

### create denominator for entire table
unique_charge_code_desc_sample_denom <- n_distinct(nh_eight_county_charge_join_missing$id,
                                                   nh_eight_county_charge_join_missing$inmate_id,
                                                   nh_eight_county_charge_join_missing$booking_id,
                                                   nh_eight_county_charge_join_missing$booking_date,
                                                   nh_eight_county_charge_join_missing$charge_code_clean,
                                                   nh_eight_county_charge_join_missing$charge_desc_clean)
### build table
table_nh_eight_county_missing_charge_counts <- nh_eight_county_charge_join_missing %>% 
  group_by(charge_code_clean,
           charge_desc_clean) %>% 
  summarise(`Unique Charges (N)` = n_distinct(id,
                                              inmate_id,
                                              booking_id,
                                              booking_date,
                                              charge_code_clean,
                                              charge_desc_clean),
            `Unique Charges (%)` = scales::percent(`Unique Charges (N)`/unique_charge_code_desc_sample_denom,
                                                   accuracy = .1)) %>% 
  ungroup()

### write out table to excel
write.xlsx(table_nh_eight_county_missing_charge_counts,
           file.path(sp_data_path,"Data/Offense Information/offenses_missing_from_lookup_file.xlsx"),
           asTable = FALSE, 
           overwrite = TRUE)
