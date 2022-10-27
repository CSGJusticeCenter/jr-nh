############################################
# Project: JRI New Hampshire
# File:  charges.R
# Last updated: October 26, 2022
# Author: Andrew Byrum

# Explore charge data for each jail -- attempting to clean and separate charge code from charge description
# Then join cleaned charge data with charge codes file to see how much coverage we have/how much manual coding is required
# Ultimate need is to link charge codes/descriptions to charge classes (misdemeanor/felony/etc) and charge category (violent, drug, public peace, etc.)

### TO DO: 

# - START WITH adm_all.rda INSTEAD OF INDIVIDUAL COUNTY FILES B/C MARI CLEANED UP CHARGE/PC HOLD FLAGS AFTER JOINING COUNTIES TOGETHER

# - DECIDE HOW TO HANDLE DUPLICATE CHARGES IN LOOKUP FILE -- WITH SAME CODES AND DATES, BUT DIFFERENT DEGREES
# Should we choose the most serious or least serious? 
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

#####################
# Charge Lookup Table
#####################
charge_codes_lookup <- charge_codes.xlsx %>%
  clean_names() %>%
  mutate(descriptor = tolower(descriptor),
         statute_title = tolower(statute_title),
         charge_code_clean = tolower(offense_statute)) %>% 
  arrange(charge_code_clean,desc(eff_date)) %>% 
  distinct(charge_code_clean,
           degree,
           .keep_all = TRUE) %>% 
  select(charge_code_clean, descriptor, statute_title, smart_code, ctl_number, vis, degree)
  

##########
# Belknap
##########
belknap_adm_charge_clean <- belknap_adm1 %>% 
  separate(charge_desc, 
           into = c("charge_desc_clean","charge_code_clean"), 
           sep = "(?<=[a-zA-Z])\\s*(?=[0-9])",
           remove = FALSE) %>% 
  mutate(charge_desc_clean = tolower(charge_desc_clean),
         charge_code_clean = tolower(charge_code_clean)) %>% 
  group_by(charge_desc_clean) %>% 
  fill(charge_code_clean, 
       .direction = "downup") %>%
  ungroup()
  # fill charge code by charge descriptions (some descriptions are missing a code where others have one)

### drop PC holds, join to charge lookup table, and see what % of non-pc hold bookings join to lookup table
belknap_adm_charge_clean_join_one <- belknap_adm_charge_clean %>% 
  filter(pc_hold=="Non-PC Hold") %>% 
  left_join(charge_codes_lookup, 
            by = "charge_code_clean") %>% 
  mutate(missing_charge_data_first_join = ifelse(is.na(smart_code),
                                      1,
                                      0))

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(belknap_adm_charge_clean_join_one$missing_charge_data_first_join) ### only missing clean charge data for 1110 of 41811 records

### here's a list of the jail-entered charge descriptions where no charge code and/or clean charge data are available
table(belknap_adm_charge_clean_join_one$charge_desc_clean[belknap_adm_charge_clean_test$missing_charge_data_first_join==1])

### for non-pc hold bookings that did not join to the charge lookup table, either because there isn't a charge code
### in the jail-provided file or because the charge code in the jail-provided file doesn't match any charge code in 
### lookup
belknap_adm_charge_clean_join_two <- belknap_adm_charge_clean_join_one %>% 
  filter(missing_charge_data_first_join == 1) %>% 
  select(-c(descriptor, statute_title, smart_code, ctl_number, vis, degree)) %>% 
  left_join(charge_codes_lookup, 
            by = c("charge_desc_clean" = "descriptor")) %>% 
  mutate(missing_charge_data_second_join = ifelse(is.na(smart_code),
                                                 1,
                                                 0))

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(belknap_adm_charge_clean_join_two$missing_charge_data_second_join) ### only recovered 39 of 1110 records

### append belknap_adm_charge_clean_test_join_two back to belknap_adm_charge_clean_test_join_one
belknap_adm_charge_clean_join_one_final <- belknap_adm_charge_clean_join_one %>% 
  filter(missing_charge_data_first_join==0)

belknap_adm_charge_clean_final <- rbind(belknap_adm_charge_clean_join_one_final,belknap_adm_charge_clean_join_two)

### final tally for belknap: missing 1,073 of 41,811 non-pc hold records


##########
# Carroll
##########
carroll_adm_charge_clean <- carroll_adm1 %>% 
  mutate(charge_desc_clean = tolower(charge_desc),
         charge_code_clean = tolower(charge_code)) %>% 
  group_by(charge_desc_clean) %>% 
  fill(charge_code_clean, 
       .direction = "downup") %>%
  ungroup()
# fill charge code by charge descriptions (some descriptions are missing a code where others have one)

### drop PC holds, join to charge lookup table, and see what % of non-pc hold bookings join to lookup table
carroll_adm_charge_clean_join_one <- carroll_adm_charge_clean %>% 
  filter(pc_hold=="Non-PC Hold") %>% 
  left_join(charge_codes_lookup, 
            by = "charge_code_clean") %>% 
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
  select(-c(descriptor, statute_title, smart_code, ctl_number, vis, degree)) %>% 
  left_join(charge_codes_lookup, 
            by = c("charge_desc_clean" = "statute_title")) %>% 
  mutate(missing_charge_data_second_join = ifelse(is.na(smart_code),
                                                  1,
                                                  0))

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(carroll_adm_charge_clean_join_two$missing_charge_data_second_join) ### recovered ~2370 of 2646 records (there are dupes with the joins)

### append belknap_adm_charge_clean_test_join_two back to belknap_adm_charge_clean_test_join_one
carroll_adm_charge_clean_join_one_final <- carroll_adm_charge_clean_join_one$ %>% 
  filter(missing_charge_data_first_join==0) %>% 
  dplyr::select(-missing_charge_data_first_join)

carroll_adm_charge_clean_join_two_final <- carroll_adm_charge_clean_join_two %>% 
  dplyr::select(-c(missing_charge_data_first_join,missing_charge_data_second_join))

### columns not matching because of charge_desc_clean/statute_title?
carroll_adm_charge_clean_final <- rbind(carroll_adm_charge_clean_join_one_final,carroll_adm_charge_clean_join_two)

### final tally for carroll: missing 1,634 non-pc hold records

##########
# Cheshire
##########
cheshire_adm_charge_clean <- cheshire_adm1 %>% 
  mutate(charge_desc_clean = tolower(charge_desc),
         charge_code_clean = tolower(charge_code)) %>% 
  group_by(charge_desc_clean) %>% 
  fill(charge_code_clean, 
       .direction = "downup") %>%
  ungroup()
# fill charge code by charge descriptions (some descriptions are missing a code where others have one)

### drop PC holds, join to charge lookup table, and see what % of non-pc hold bookings join to lookup table
cheshire_adm_charge_clean_join_one <- cheshire_adm_charge_clean %>% 
  filter(pc_hold=="Non-PC Hold") %>% 
  left_join(charge_codes_lookup, 
            by = "charge_code_clean") %>% 
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
  select(-c(descriptor, statute_title, smart_code, ctl_number, vis, degree)) %>% 
  left_join(charge_codes_lookup, 
            by = c("charge_desc_clean" = "statute_title")) %>% 
  mutate(missing_charge_data_second_join = ifelse(is.na(smart_code),
                                                  1,
                                                  0))

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(cheshire_adm_charge_clean_join_two$missing_charge_data_second_join) ### recovered ~2370 of 2646 records (there are dupes with the joins)

### append belknap_adm_charge_clean_test_join_two back to belknap_adm_charge_clean_test_join_one
cheshire_adm_charge_clean_join_one_final <- cheshire_adm_charge_clean_join_one %>% 
  filter(missing_charge_data_first_join==0)

cheshire_adm_charge_clean_final <- rbind(cheshire_adm_charge_clean_join_one_final,cheshire_adm_charge_clean_join_two)

### final tally for cheshire: missing 1,711 non-pc hold records

##########
# Coos
##########
coos_adm_charge_clean <- coos_adm1 %>% 
  mutate(charge_desc_clean = tolower(charge_desc),
         charge_code_clean = tolower(charge_code)) %>% 
  group_by(charge_desc_clean) %>% 
  fill(charge_code_clean, 
       .direction = "downup") %>%
  ungroup()
# fill charge code by charge descriptions (some descriptions are missing a code where others have one)

### drop PC holds, join to charge lookup table, and see what % of non-pc hold bookings join to lookup table
coos_adm_charge_clean_join_one <- coos_adm_charge_clean %>% 
  filter(pc_hold=="Non-PC Hold") %>% 
  left_join(charge_codes_lookup, 
            by = "charge_code_clean") %>% 
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
  select(-c(descriptor, statute_title, smart_code, ctl_number, vis, degree)) %>% 
  left_join(charge_codes_lookup, 
            by = c("charge_desc_clean" = "statute_title")) %>% 
  mutate(missing_charge_data_second_join = ifelse(is.na(smart_code),
                                                  1,
                                                  0))

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(coos_adm_charge_clean_join_two$missing_charge_data_second_join) ### recovered ~2370 of 2646 records (there are dupes with the joins)

### append belknap_adm_charge_clean_test_join_two back to belknap_adm_charge_clean_test_join_one
coos_adm_charge_clean_join_one_final <- coos_adm_charge_clean_join_one %>% 
  filter(missing_charge_data_first_join==0)

coos_adm_charge_clean_final <- rbind(coos_adm_charge_clean_join_one_final,coos_adm_charge_clean_join_two)

### final tally for coos: missing 391 non-pc hold records

##########
# Hillsborough
##########
hillsborough_adm_charge_clean <- hillsborough_adm1 %>% 
  separate(charge_desc, 
           paste("charge_desc", 1:11, sep="_"), 
           sep=";", 
           extra="drop",
           remove=FALSE) %>% ### 11 charges is the most included in charge_desc
  mutate(across(charge_desc_1:charge_desc_11, str_trim)) ### remove leading and trailing blanks

### here -- 10/27

  separate(charge_desc, 
           into = c("charge_desc_clean","charge_code_clean"), 
           sep = "(?<=[a-zA-Z])\\s*(?=[0-9])",
           remove = FALSE) %>% 
  mutate(charge_desc_clean = tolower(charge_desc),
         charge_code_clean = tolower(charge_code)) %>% 
  group_by(charge_desc_clean) %>% 
  fill(charge_code_clean, 
       .direction = "downup") %>%
  ungroup()
# fill charge code by charge descriptions (some descriptions are missing a code where others have one)

### drop PC holds, join to charge lookup table, and see what % of non-pc hold bookings join to lookup table
hillsborough_adm_charge_clean_join_one <- hillsborough_adm_charge_clean %>% 
  filter(pc_hold=="Non-PC Hold") %>% 
  left_join(charge_codes_lookup, 
            by = "charge_code_clean") %>% 
  mutate(missing_charge_data_first_join = ifelse(is.na(smart_code),
                                                 1,
                                                 0))

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(hillsborough_adm_charge_clean_join_one$missing_charge_data_first_join) ### missing clean charge data for 2646 records

### here's a list of the jail-entered charge descriptions where no charge code and/or clean charge data are available
table(hillsborough_adm_charge_clean_join_one$charge_desc_clean[hillsborough_adm_charge_clean_join_one$missing_charge_data_first_join==1])

### for non-pc hold bookings that did not join to the charge lookup table, either because there isn't a charge code
### in the jail-provided file or because the charge code in the jail-provided file doesn't match any charge code in 
### lookup
hillsborough_adm_charge_clean_join_two <- hillsborough_adm_charge_clean_join_one %>% 
  filter(missing_charge_data_first_join == 1) %>% 
  select(-c(descriptor, statute_title, smart_code, ctl_number, vis, degree)) %>% 
  left_join(charge_codes_lookup, 
            by = c("charge_desc_clean" = "statute_title")) %>% 
  mutate(missing_charge_data_second_join = ifelse(is.na(smart_code),
                                                  1,
                                                  0))

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(hillsborough_adm_charge_clean_join_two$missing_charge_data_second_join) ### recovered ~2370 of 2646 records (there are dupes with the joins)

### append belknap_adm_charge_clean_test_join_two back to belknap_adm_charge_clean_test_join_one
hillsborough_adm_charge_clean_join_one_final <- hillsborough_adm_charge_clean_join_one %>% 
  filter(missing_charge_data_first_join==0)

hillsborough_adm_charge_clean_final <- rbind(hillsborough_adm_charge_clean_join_one_final,hillsborough_adm_charge_clean_join_two)

### final tally for hillsborough: missing 391 non-pc hold records

##########
# Merrimack
##########

##########
# Rockingham
##########

##########
# Strafford
##########

##########
# Sullivan
##########


