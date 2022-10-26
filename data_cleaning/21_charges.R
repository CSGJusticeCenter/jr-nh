############################################
# Project: JRI New Hampshire
# File:  charges.R
# Last updated: October 26, 2022
# Author: Andrew Byrum

# Explore charge data for each jail -- attempting to clean and separate charge code from charge description
# Then join cleaned charge data with charge codes file to see how much coverage we have/how much manual coding is required
# Ultimate need is to link charge codes/descriptions to charge classes (misdemeanor/felony/etc) and charge category (violent, drug, public peace, etc.)

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
  select(charge_code_clean, descriptor, statute_title, smart_code, ctl_number, vis, degree) %>%
  distinct()

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
belknap_adm_charge_clean_test <- belknap_adm_charge_clean %>% 
  filter(pc_hold=="Non-PC Hold") %>% 
  left_join(charge_codes_lookup, 
            by = "charge_code_clean") %>% 
  mutate(missing_charge_data = ifelse(is.na(descriptor),
                                      1,
                                      0))

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
table(belknap_adm_charge_clean_test$missing_charge_data) ### only missing clean charge data for 1110 of 41811 records

### here's a list of the jail-entered charge descriptions where no charge code and/or clean charge data are available
table(belknap_adm_charge_clean_test$charge_desc_clean[belknap_adm_charge_clean_test$missing_charge_data==1])

##########
# Carroll
##########

##########
# Cheshire
##########

##########
# Coos
##########

##########
# Hillsborough
##########

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


