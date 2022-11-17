############################################
# Project: JRI New Hampshire
# File: 21_charges.R
# Last updated: November 17, 2022
# Author: Andrew Byrum

# Explore charge data for each jail -- attempting to clean and separate charge code from charge description
# Then join cleaned charge data with charge codes file to see how much coverage we have/how much manual coding is required
# Create lookup table with all charge codes, descriptions, and pertinent charge degrees and charge types
# Ultimate hope is to link charge codes/descriptions to charge classes (misdemeanor/felony/etc) and charge category (violent, drug, public order, etc.)

### TO DO/TALK THROUGH: 

# - TO DOUBLE CHECK: START WITH adm_all.rda INSTEAD OF INDIVIDUAL COUNTY FILES B/C MARI CLEANED UP CHARGE/PC HOLD FLAGS AFTER JOINING COUNTIES TOGETHER?

# - De-dup by individual/booking -- keeping most serious crime type? (hierarchy: violent, property, drug/alcohol, public order, vop?)
# all jails appear to have submitted all charges associated with a given individual/booking (i.e., instances with multiple charges for one booking id)

# - Missing charge code/charge description data by county. How should we interpret this? Only Belknap seems to have somewhat notable missingness: 
# about 11% of non-pc hold booking have no charge code or charge description

# - Business rule to confirm: de-duping by individual (id + inmate_id), booking (booking_id), and charge (either charge_code or charge_desc depending on data availability)
### There appear to be some duplicates across these fields, but we have no way of knowing if two identical charges for the same booking
### represents a duplicated entry or multiple counts of the same charge. Unfortunately, this will likely just be a limitation of this analysis


################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
# load packages
# source("data_cleaning/00_library_ab_path.R")

# Use cleaned and standardized county-level dataframes created in `12_dataframes.R`
load(paste0(sp_data_path, "/Data/r_data/belknap_adm.Rda", sep = ""))
load(paste0(sp_data_path, "/Data/r_data/carroll_adm.Rda", sep = ""))
load(paste0(sp_data_path, "/Data/r_data/cheshire_adm.Rda", sep = ""))
load(paste0(sp_data_path, "/Data/r_data/coos_adm.Rda", sep = ""))
load(paste0(sp_data_path, "/Data/r_data/hillsborough_adm.Rda", sep = ""))
load(paste0(sp_data_path, "/Data/r_data/merrimack_adm.Rda", sep = ""))
load(paste0(sp_data_path, "/Data/r_data/rockingham_adm.Rda", sep = ""))
load(paste0(sp_data_path, "/Data/r_data/strafford_adm.Rda", sep = ""))
load(paste0(sp_data_path, "/Data/r_data/sullivan_adm.Rda", sep = ""))

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

##################################
# Charge Lookup Table
##################################

### I created this lookup table by doing the following:

#1. Start with original charge code/description lookup table ("CPI_DMV_COURT_20180412083730")

### This table included information on charge degree but not crime type (violent, property, drug/alcohol, etc.)

### We were able to join about 70% of cleaned charge data from jails to this original lookup table

#2. Created a second table with the same columns as the original lookup table with all charge codes and descriptions that did not join (the other 30%)

#3. Loosely using the categorization of violent, property, public order offenses on the NH Department of Safety data dashboard 
#(https://crimestats.dos.nh.gov/public/Dim/dimension.aspx), I created the following categories for certain offenses:
# violent, property, drug/alcohol, public order, and probation/parole violation. I added this additional column manually in both
# the original lookup table as well as the second table with offenses that did not join to the original lookup

### For full list of which offenses are grouped into each crime type, see table below

#4. In the 'STATUTE TITLE' column in the second lookup table, I added the cleaned up charge descriptions found in the original lookup table
# that corresponded to the raw values from the jail data that did not join (e.g., "shoplifting" becomes "Willful Concealment and Shoplifiting")

### this allows us to look at top charges within and across categories 
### we'll still need to clean up/standardize some charge/statute descriptions 
# (e.g. "willful concealment" and "willful concealment and shoplifting" should be the same)

#5. Append all records from second lookup table (with charges that did not join to original lookup) to original lookup table

##### Note: When attempting to join values from the lookup table to jail-level files, we'll need to attempt three joins on:
# statute code, descriptor, and statute title. One of these three should join to all charges we've categorized 

### import raw charge codes lookup table
charge_codes.xlsx <- read_excel(paste0(sp_data_path, "/Data/Offense Information/CPI_DMV_COURT_crime_type_clean.xls"), 
                                sheet = "charge_lookup_crime_type_full")

### clean lookup table
charge_codes_crime_type_lookup <- charge_codes.xlsx %>%
  clean_names() %>%
  mutate(descriptor_lookup = tolower(descriptor),
         statute_title_lookup = tolower(statute_title),
         charge_code_lookup = tolower(offense_statute)) %>% 
### Based on the jail charge data we've received, I don't think we can look at severity/degree of charges
### With only offense statute and description, we can't accurately tell if a given charge is a misdemeanor or felony (or even class A misdemeanor versus class B misdemeanor)
### For this level of detail, we'd need either the 'Smart Code' or the 'ODDSY Code' - which may not even be known at the time of booking
### For instance, there are 8 entries for 'criminal trespass'/'statute '635:2' in the lookup file - which degrees ranging from violations to class A and B misdemeanors to class B felonies
### I think the best option, at this point, is to just categorize the charges into violent, public order, drugs/alcohol, and property
  distinct(charge_code_lookup,
           descriptor_lookup,
           statute_title_lookup,
           crime_type,
           .keep_all = TRUE) %>% ### de-dup by offense information and crime type 
  select(charge_code_lookup, descriptor_lookup, statute_title_lookup, crime_type_lookup=crime_type) 


### removing degree and cleaning of degree from syntax for reasons stated above;
### keeping syntax commented out here just in case something changes

# ### clean up degree names for eventual tables or visualization
# ### second de-up: 
# ### DECIDE HOW TO HANDLE DUPLICATE CHARGES IN LOOKUP FILE -- WITH SAME CODES AND DATES, BUT DIFFERENT DEGREES
# ### for now, I'm keeping the most serious charge when there are pure duplicates
#   mutate(degree_clean = case_when(
#     degree=="FA" ~ "A Felony",
#     degree=="FB" ~ "B Felony",
#     degree=="FS" ~ "FS Felony",
#     degree=="FU" ~ "FU Felony",
#     degree=="MA" ~ "A Misdemeanor",
#     degree=="MB" ~ "B Misdemeanor",
#     degree=="V" ~ "Violation",### cleaning degree classes: https://www.russmanlaw.com/new-hampshire-classification-of-offenses
#     TRUE ~ as.character(NA)), ### still need clarification on severity of "FS" and "FU"
#   degree_severity_order = case_when(
#     degree_clean=="A Felony" ~ 1,
#     degree_clean=="B Felony" ~ 2,
#     degree_clean=="FS Felony" ~ 3,
#     degree_clean=="FU Felony" ~ 4,
#     degree_clean=="A Misdemeanor" ~ 5,
#     degree_clean=="B Misdemeanor" ~ 6,
#     degree_clean=="Violation" ~ 7,### ordering degree classes in order of severity: https://www.russmanlaw.com/new-hampshire-classification-of-offenses
#       TRUE ~ as.numeric(NA))) %>%  
#       arrange(charge_code_lookup, 
#               degree_severity_order) %>% ### here we are de-duping by charge code, keeping the record with the most serious offense conviction linked
#       distinct(charge_code_lookup,
#                .keep_all=TRUE)
  

##################################
# Belknap
##################################

### clean up belknap file
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
  ungroup() %>% 
### de-duping by individual (id + inmate_id), booking (booking_id), and charge 
### there appear to be some duplicates across these fields, 
### but we have no way of knowing if two identical charges for the same booking
### represents a duplicated entry or multiple counts of the same charge
### unfortunately, this will likely just be a limitation of this analysis
  distinct(id,
           inmate_id,
           booking_id,
           charge_code_clean,
           .keep_all = TRUE) 

####### use three separate joins as jail charge data will join on either charge code, statute title, or descriptor

### join to charge_codes_lookup via charge_code_lookup
belknap_adm_charge_clean_join_one <- belknap_adm_charge_clean %>% 
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_code_clean"="charge_code_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc,
           .keep_all = TRUE) %>% 
  mutate(charge_code_lookup = ifelse(!is.na(descriptor_lookup) == TRUE,
                                     charge_code_clean,
                                     NA)) ### populate for eventual rbind where all three df's need same columns

### join to charge_codes_lookup via charge_desc_clean and statute_title_lookup
belknap_adm_charge_clean_join_two <- belknap_adm_charge_clean_join_one %>% 
  filter(is.na(descriptor_lookup) == TRUE) %>% 
  select(-c(charge_code_lookup,
            descriptor_lookup,
            statute_title_lookup,
            crime_type_lookup)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) for rbind  
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_desc_clean" = "statute_title_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  mutate(statute_title_lookup = ifelse(!is.na(descriptor_lookup) == TRUE,
                                       charge_desc_clean,
                                       NA)) ### populate for eventual rbind where all three df's need same columns 

### join to charge_codes_lookup via charge_desc_clean and descriptor_lookup
belknap_adm_charge_clean_join_three <- belknap_adm_charge_clean_join_two %>% 
  filter(is.na(descriptor_lookup) == TRUE) %>% 
  select(-c(charge_code_lookup,
            descriptor_lookup,
            statute_title_lookup,
            crime_type_lookup)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) for rbind  
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_desc_clean" = "descriptor_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  mutate(descriptor_lookup = ifelse(!is.na(statute_title_lookup) == TRUE,
                                    charge_desc_clean,
                                    NA)) ### populate for eventual rbind where all three df's need same columns 

### combine df's from first, second, and third attempts to join to lookup values
belknap_adm_charge_clean_final <- rbind(belknap_adm_charge_clean_join_one,
                                        belknap_adm_charge_clean_join_two,
                                        belknap_adm_charge_clean_join_three) %>% 
### de-dup once more for records that ended up duplicating across the joins
### use arrange to keep records with joining lookup values over identical records with missing lookup values
  arrange(id,
          inmate_id,
          booking_id,
          statute_title_lookup) %>%
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
### as a check, at this point our total record should be identical to the cleaned jail file we started with -- it is
### now we want to de-dup by booking (one booking may have multiple charges associated) 
### keep most serious charge by crime type (hierarchy: violent, property, drug/alcohol, public order, vop)
  mutate(crime_type_severity_order = case_when(
        crime_type_lookup=="Violent" ~ 1,
        crime_type_lookup=="Property" ~ 2,
        crime_type_lookup=="Drug/Alcohol" ~ 3,
        crime_type_lookup=="Public Order" ~ 4,
        crime_type_lookup=="Probation/Parole Violation" ~ 5,
        is.na(crime_type_lookup)==TRUE ~ 6,
        TRUE ~ as.numeric(NA))) %>%
  arrange(id,
          inmate_id,
          booking_id,
          crime_type_severity_order) %>% 
### de-dup by booking, keeping the record with the most serious crime type
  distinct(id,
           inmate_id,
           booking_id,
           .keep_all=TRUE) %>% 
### create missing_lookup_charge_data column to double check missingness with non-pc holds
  mutate(missing_lookup_charge_data = ifelse(is.na(statute_title_lookup)==TRUE,
                                      1,
                                      0))

  
### excluding PC holds, see what % of non-pc hold bookings join to lookup table
  
### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
### missing clean charge data for 305 non-PC bookings -- all of which are missing charge codes and descriptions in jail-provided data
table(belknap_adm_charge_clean_final$missing_lookup_charge_data,belknap_adm_charge_clean_final$pc_hold) 


##################################
# Carroll
##################################

### clean up carroll file
carroll_adm_charge_clean <- carroll_adm1 %>% 
  mutate(charge_desc_clean = tolower(charge_desc),
         charge_code_clean = tolower(charge_code)) %>% 
# fill charge code by charge descriptions (some descriptions are missing a code where others have one)
  group_by(charge_desc_clean) %>% 
  fill(charge_code_clean, 
       .direction = "downup") %>%
  ungroup() %>% 
  ### de-duping by individual (id + inmate_id), booking (booking_id), and charge 
  ### there appear to be some duplicates across these fields, 
  ### but we have no way of knowing if two identical charges for the same booking
  ### represents a duplicated entry or multiple counts of the same charge
  ### unfortunately, this will likely just be a limitation of this analysis
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc,
           .keep_all = TRUE) 

####### use three separate joins as jail charge data will join on either charge code, statute title, or descriptor

### join to charge_codes_lookup via charge_code_lookup
carroll_adm_charge_clean_join_one <- carroll_adm_charge_clean %>% 
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_code_clean"="charge_code_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_code_clean,
           .keep_all = TRUE) %>% 
  mutate(charge_code_lookup = ifelse(!is.na(descriptor_lookup) == TRUE,
                                     charge_code_clean,
                                     NA)) ### populate for eventual rbind where all three df's need same columns

### join to charge_codes_lookup via charge_desc_clean and statute_title_lookup
carroll_adm_charge_clean_join_two <- carroll_adm_charge_clean_join_one %>% 
  filter(is.na(descriptor_lookup) == TRUE) %>% 
  select(-c(charge_code_lookup,
            descriptor_lookup,
            statute_title_lookup,
            crime_type_lookup)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) for rbind  
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_desc_clean" = "statute_title_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  mutate(statute_title_lookup = ifelse(!is.na(descriptor_lookup) == TRUE,
                                       charge_desc_clean,
                                       NA)) ### populate for eventual rbind where all three df's need same columns 

### join to charge_codes_lookup via charge_desc_clean and descriptor_lookup
carroll_adm_charge_clean_join_three <- carroll_adm_charge_clean_join_two %>% 
  filter(is.na(descriptor_lookup) == TRUE) %>% 
  select(-c(charge_code_lookup,
            descriptor_lookup,
            statute_title_lookup,
            crime_type_lookup)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) for rbind  
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_desc_clean" = "descriptor_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  mutate(descriptor_lookup = ifelse(!is.na(statute_title_lookup) == TRUE,
                                    charge_desc_clean,
                                    NA)) ### populate for eventual rbind where all three df's need same columns 

### combine df's from first, second, and third attempts to join to lookup values
carroll_adm_charge_clean_final <- rbind(carroll_adm_charge_clean_join_one,
                                        carroll_adm_charge_clean_join_two,
                                        carroll_adm_charge_clean_join_three) %>% 
  ### de-dup once more for records that ended up duplicating across the joins
  ### use arrange to keep records with joining lookup values over identical records with missing lookup values
  arrange(id,
          inmate_id,
          booking_id,
          statute_title_lookup) %>%
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  ### as a check, at this point our total record should be identical to the cleaned jail file we started with -- it is
  ### now we want to de-dup by booking (one booking may have multiple charges associated) 
  ### keep most serious charge by crime type (hierarchy: violent, property, drug/alcohol, public order, vop)
  mutate(crime_type_severity_order = case_when(
    crime_type_lookup=="Violent" ~ 1,
    crime_type_lookup=="Property" ~ 2,
    crime_type_lookup=="Drug/Alcohol" ~ 3,
    crime_type_lookup=="Public Order" ~ 4,
    crime_type_lookup=="Probation/Parole Violation" ~ 5,
    is.na(crime_type_lookup)==TRUE ~ 6,
    TRUE ~ as.numeric(NA))) %>%
  arrange(id,
          inmate_id,
          booking_id,
          crime_type_severity_order) %>% 
  ### de-dup by booking, keeping the record with the most serious crime type
  distinct(id,
           inmate_id,
           booking_id,
           .keep_all=TRUE) %>% 
  ### create missing_lookup_charge_data column to double check missingness with non-pc holds
  mutate(missing_lookup_charge_data = ifelse(is.na(statute_title_lookup)==TRUE,
                                             1,
                                             0))


### excluding PC holds, see what % of non-pc hold bookings join to lookup table

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
### missing clean charge data for 0 non-PC bookings 
table(carroll_adm_charge_clean_final$missing_lookup_charge_data,carroll_adm_charge_clean_final$pc_hold)

##################################
# Cheshire
##################################

### clean up cheshire file
cheshire_adm_charge_clean <- cheshire_adm1 %>% 
  mutate(charge_desc_clean = tolower(charge_desc),
         charge_code_clean = tolower(charge_code)) %>% 
# fill charge code by charge descriptions (some descriptions are missing a code where others have one)
  group_by(charge_desc_clean) %>% 
  fill(charge_code_clean, 
       .direction = "downup") %>%
  ungroup() %>% 
  ### de-duping by individual (id + inmate_id), booking (booking_id), and charge 
  ### there appear to be some duplicates across these fields, 
  ### but we have no way of knowing if two identical charges for the same booking
  ### represents a duplicated entry or multiple counts of the same charge
  ### unfortunately, this will likely just be a limitation of this analysis
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc,
           .keep_all = TRUE) 

####### use three separate joins as jail charge data will join on either charge code, statute title, or descriptor

### join to charge_codes_lookup via charge_code_lookup
cheshire_adm_charge_clean_join_one <- cheshire_adm_charge_clean %>% 
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_code_clean"="charge_code_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_code_clean,
           .keep_all = TRUE) %>% 
  mutate(charge_code_lookup = ifelse(!is.na(descriptor_lookup) == TRUE,
                                     charge_code_clean,
                                     NA)) ### populate for eventual rbind where all three df's need same columns

### join to charge_codes_lookup via charge_desc_clean and statute_title_lookup
cheshire_adm_charge_clean_join_two <- cheshire_adm_charge_clean_join_one %>% 
  filter(is.na(descriptor_lookup) == TRUE) %>% 
  select(-c(charge_code_lookup,
            descriptor_lookup,
            statute_title_lookup,
            crime_type_lookup)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) for rbind  
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_desc_clean" = "statute_title_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  mutate(statute_title_lookup = ifelse(!is.na(descriptor_lookup) == TRUE,
                                       charge_desc_clean,
                                       NA)) ### populate for eventual rbind where all three df's need same columns 

### join to charge_codes_lookup via charge_desc_clean and descriptor_lookup
cheshire_adm_charge_clean_join_three <- cheshire_adm_charge_clean_join_two %>% 
  filter(is.na(descriptor_lookup) == TRUE) %>% 
  select(-c(charge_code_lookup,
            descriptor_lookup,
            statute_title_lookup,
            crime_type_lookup)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) for rbind  
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_desc_clean" = "descriptor_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  mutate(descriptor_lookup = ifelse(!is.na(statute_title_lookup) == TRUE,
                                    charge_desc_clean,
                                    NA)) ### populate for eventual rbind where all three df's need same columns 

### combine df's from first, second, and third attempts to join to lookup values
cheshire_adm_charge_clean_final <- rbind(cheshire_adm_charge_clean_join_one,
                                        cheshire_adm_charge_clean_join_two,
                                        cheshire_adm_charge_clean_join_three) %>% 
  ### de-dup once more for records that ended up duplicating across the joins
  ### use arrange to keep records with joining lookup values over identical records with missing lookup values
  arrange(id,
          inmate_id,
          booking_id,
          statute_title_lookup) %>%
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  ### as a check, at this point our total record should be identical to the cleaned jail file we started with -- it is
  ### now we want to de-dup by booking (one booking may have multiple charges associated) 
  ### keep most serious charge by crime type (hierarchy: violent, property, drug/alcohol, public order, vop)
  mutate(crime_type_severity_order = case_when(
    crime_type_lookup=="Violent" ~ 1,
    crime_type_lookup=="Property" ~ 2,
    crime_type_lookup=="Drug/Alcohol" ~ 3,
    crime_type_lookup=="Public Order" ~ 4,
    crime_type_lookup=="Probation/Parole Violation" ~ 5,
    is.na(crime_type_lookup)==TRUE ~ 6,
    TRUE ~ as.numeric(NA))) %>%
  arrange(id,
          inmate_id,
          booking_id,
          crime_type_severity_order) %>% 
  ### de-dup by booking, keeping the record with the most serious crime type
  distinct(id,
           inmate_id,
           booking_id,
           .keep_all=TRUE) %>% 
  ### create missing_lookup_charge_data column to double check missingness with non-pc holds
  mutate(missing_lookup_charge_data = ifelse(is.na(statute_title_lookup)==TRUE,
                                             1,
                                             0))


### excluding PC holds, see what % of non-pc hold bookings join to lookup table

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
### missing clean charge data for 3 non-PC bookings -- all of which are missing charge codes and descriptions in jail-provided data
table(cheshire_adm_charge_clean_final$missing_lookup_charge_data,cheshire_adm_charge_clean_final$pc_hold)

##################################
# Coos
##################################

### clean up coos file
coos_adm_charge_clean <- coos_adm1 %>% 
  mutate(charge_desc_clean = tolower(charge_desc),
         charge_code_clean = tolower(charge_code)) %>% 
# fill charge code by charge descriptions (some descriptions are missing a code where others have one)
  group_by(charge_desc_clean) %>% 
  fill(charge_code_clean, 
       .direction = "downup") %>%
  ungroup() %>% 
  ### de-duping by individual (id + inmate_id), booking (booking_id), and charge 
  ### there appear to be some duplicates across these fields, 
  ### but we have no way of knowing if two identical charges for the same booking
  ### represents a duplicated entry or multiple counts of the same charge
  ### unfortunately, this will likely just be a limitation of this analysis
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc,
           .keep_all = TRUE) 

####### use three separate joins as jail charge data will join on either charge code, statute title, or descriptor

### join to charge_codes_lookup via charge_code_lookup
coos_adm_charge_clean_join_one <- coos_adm_charge_clean %>% 
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_code_clean"="charge_code_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_code_clean,
           .keep_all = TRUE) %>% 
  mutate(charge_code_lookup = ifelse(!is.na(descriptor_lookup) == TRUE,
                                     charge_code_clean,
                                     NA)) ### populate for eventual rbind where all three df's need same columns

### join to charge_codes_lookup via charge_desc_clean and statute_title_lookup
coos_adm_charge_clean_join_two <- coos_adm_charge_clean_join_one %>% 
  filter(is.na(descriptor_lookup) == TRUE) %>% 
  select(-c(charge_code_lookup,
            descriptor_lookup,
            statute_title_lookup,
            crime_type_lookup)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) for rbind  
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_desc_clean" = "statute_title_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  mutate(statute_title_lookup = ifelse(!is.na(descriptor_lookup) == TRUE,
                                       charge_desc_clean,
                                       NA)) ### populate for eventual rbind where all three df's need same columns 

### join to charge_codes_lookup via charge_desc_clean and descriptor_lookup
coos_adm_charge_clean_join_three <- coos_adm_charge_clean_join_two %>% 
  filter(is.na(descriptor_lookup) == TRUE) %>% 
  select(-c(charge_code_lookup,
            descriptor_lookup,
            statute_title_lookup,
            crime_type_lookup)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) for rbind  
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_desc_clean" = "descriptor_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  mutate(descriptor_lookup = ifelse(!is.na(statute_title_lookup) == TRUE,
                                    charge_desc_clean,
                                    NA)) ### populate for eventual rbind where all three df's need same columns 

### combine df's from first, second, and third attempts to join to lookup values
coos_adm_charge_clean_final <- rbind(coos_adm_charge_clean_join_one,
                                         coos_adm_charge_clean_join_two,
                                         coos_adm_charge_clean_join_three) %>% 
  ### de-dup once more for records that ended up duplicating across the joins
  ### use arrange to keep records with joining lookup values over identical records with missing lookup values
  arrange(id,
          inmate_id,
          booking_id,
          statute_title_lookup) %>%
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  ### as a check, at this point our total record should be identical to the cleaned jail file we started with -- it is
  ### now we want to de-dup by booking (one booking may have multiple charges associated) 
  ### keep most serious charge by crime type (hierarchy: violent, property, drug/alcohol, public order, vop)
  mutate(crime_type_severity_order = case_when(
    crime_type_lookup=="Violent" ~ 1,
    crime_type_lookup=="Property" ~ 2,
    crime_type_lookup=="Drug/Alcohol" ~ 3,
    crime_type_lookup=="Public Order" ~ 4,
    crime_type_lookup=="Probation/Parole Violation" ~ 5,
    is.na(crime_type_lookup)==TRUE ~ 6,
    TRUE ~ as.numeric(NA))) %>%
  arrange(id,
          inmate_id,
          booking_id,
          crime_type_severity_order) %>% 
  ### de-dup by booking, keeping the record with the most serious crime type
  distinct(id,
           inmate_id,
           booking_id,
           .keep_all=TRUE) %>% 
  ### create missing_lookup_charge_data column to double check missingness with non-pc holds
  mutate(missing_lookup_charge_data = ifelse(is.na(statute_title_lookup)==TRUE,
                                             1,
                                             0))

### excluding PC holds, see what % of non-pc hold bookings join to lookup table

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
### missing clean charge data for 0 non-PC bookings
table(coos_adm_charge_clean_final$missing_lookup_charge_data,coos_adm_charge_clean_final$pc_hold)

##################################
# Hillsborough
##################################

### clean up hillsborough file
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
  ### de-duping by individual (id + inmate_id), booking (booking_id), and charge 
  ### there appear to be some duplicates across these fields, 
  ### but we have no way of knowing if two identical charges for the same booking
  ### represents a duplicated entry or multiple counts of the same charge
  ### unfortunately, this will likely just be a limitation of this analysis
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc,
           .keep_all = TRUE) 

####### use three separate joins as jail charge data will join on either charge code, statute title, or descriptor

### join to charge_codes_lookup via charge_code_lookup
hillsborough_adm_charge_clean_join_one <- hillsborough_adm_charge_clean %>% 
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_code_clean"="charge_code_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_code_clean,
           .keep_all = TRUE) %>% 
  mutate(charge_code_lookup = ifelse(!is.na(descriptor_lookup) == TRUE,
                                     charge_code_clean,
                                     NA)) ### populate for eventual rbind where all three df's need same columns

### join to charge_codes_lookup via charge_desc_clean and statute_title_lookup
hillsborough_adm_charge_clean_join_two <- hillsborough_adm_charge_clean_join_one %>% 
  filter(is.na(descriptor_lookup) == TRUE) %>% 
  select(-c(charge_code_lookup,
            descriptor_lookup,
            statute_title_lookup,
            crime_type_lookup)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) for rbind  
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_desc_clean" = "statute_title_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  mutate(statute_title_lookup = ifelse(!is.na(descriptor_lookup) == TRUE,
                                       charge_desc_clean,
                                       NA)) ### populate for eventual rbind where all three df's need same columns 

### join to charge_codes_lookup via charge_desc_clean and descriptor_lookup
hillsborough_adm_charge_clean_join_three <- hillsborough_adm_charge_clean_join_two %>% 
  filter(is.na(descriptor_lookup) == TRUE) %>% 
  select(-c(charge_code_lookup,
            descriptor_lookup,
            statute_title_lookup,
            crime_type_lookup)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) for rbind  
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_desc_clean" = "descriptor_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  mutate(descriptor_lookup = ifelse(!is.na(statute_title_lookup) == TRUE,
                                    charge_desc_clean,
                                    NA)) ### populate for eventual rbind where all three df's need same columns 

### combine df's from first, second, and third attempts to join to lookup values
hillsborough_adm_charge_clean_final <- rbind(hillsborough_adm_charge_clean_join_one,
                                     hillsborough_adm_charge_clean_join_two,
                                     hillsborough_adm_charge_clean_join_three) %>% 
  ### de-dup once more for records that ended up duplicating across the joins
  ### use arrange to keep records with joining lookup values over identical records with missing lookup values
  arrange(id,
          inmate_id,
          booking_id,
          statute_title_lookup) %>%
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  ### as a check, at this point our total record should be identical to the cleaned jail file we started with -- it is
  ### now we want to de-dup by booking (one booking may have multiple charges associated) 
  ### keep most serious charge by crime type (hierarchy: violent, property, drug/alcohol, public order, vop)
  mutate(crime_type_severity_order = case_when(
    crime_type_lookup=="Violent" ~ 1,
    crime_type_lookup=="Property" ~ 2,
    crime_type_lookup=="Drug/Alcohol" ~ 3,
    crime_type_lookup=="Public Order" ~ 4,
    crime_type_lookup=="Probation/Parole Violation" ~ 5,
    is.na(crime_type_lookup)==TRUE ~ 6,
    TRUE ~ as.numeric(NA))) %>%
  arrange(id,
          inmate_id,
          booking_id,
          crime_type_severity_order) %>% 
  ### de-dup by booking, keeping the record with the most serious crime type
  distinct(id,
           inmate_id,
           booking_id,
           .keep_all=TRUE) %>% 
  ### create missing_lookup_charge_data column to double check missingness with non-pc holds
  mutate(missing_lookup_charge_data = ifelse(is.na(statute_title_lookup)==TRUE,
                                             1,
                                             0)) %>% 
  dplyr::select(-c(charge_desc_first_clean_count,
                   charge_desc_first_clean_value,
                   charge_desc_second_clean_value_1,
                   charge_desc_second_clean_value_2,
                   charge_desc_second_clean_value_1_num_only)) ### remove hillsborough-specific columns for eventual rbind with other jails
                
### excluding PC holds, see what % of non-pc hold bookings join to lookup table

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
### missing clean charge data for 42 non-PC bookings -- all of which are missing charge codes and descriptions in jail-provided data
table(hillsborough_adm_charge_clean_final$missing_lookup_charge_data,hillsborough_adm_charge_clean_final$pc_hold)

##################################
# Merrimack
##################################

### NOTE: MERRIMACK DOESN'T HAVE ANY CHARGE CODES -- SO WE HAVE TO RELY ON CHARGE DESCRIPTIONS

### clean up merrimack file
merrimack_adm_charge_clean <- merrimack_adm1 %>% 
  mutate(charge_desc_clean = tolower(charge_desc),
         charge_code_clean = tolower(charge_code)) %>% 
  ### de-duping by individual (id + inmate_id), booking (booking_id), and charge 
  ### there appear to be some duplicates across these fields, 
  ### but we have no way of knowing if two identical charges for the same booking
  ### represents a duplicated entry or multiple counts of the same charge
  ### unfortunately, this will likely just be a limitation of this analysis
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc,
           .keep_all = TRUE) 

####### use two separate joins as jail charge data will join on either statute title or descriptor

### join to charge_codes_lookup via charge_desc_clean and statute_title_lookup
merrimack_adm_charge_clean_join_one <- merrimack_adm_charge_clean %>% 
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_desc_clean" = "statute_title_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  mutate(statute_title_lookup = ifelse(!is.na(descriptor_lookup) == TRUE,
                                       charge_desc_clean,
                                       NA)) ### populate for eventual rbind where all three df's need same columns 

### join to charge_codes_lookup via charge_desc_clean and descriptor_lookup
merrimack_adm_charge_clean_join_two <- merrimack_adm_charge_clean_join_one %>% 
  filter(is.na(descriptor_lookup) == TRUE) %>% 
  select(-c(charge_code_lookup,
            descriptor_lookup,
            statute_title_lookup,
            crime_type_lookup)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) for rbind  
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_desc_clean" = "descriptor_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  mutate(descriptor_lookup = ifelse(!is.na(statute_title_lookup) == TRUE,
                                    charge_desc_clean,
                                    NA)) ### populate for eventual rbind where all three df's need same columns 

### combine df's from first and second attempts to join to lookup values
merrimack_adm_charge_clean_final <- rbind(merrimack_adm_charge_clean_join_one,
                                             merrimack_adm_charge_clean_join_two) %>% 
  ### de-dup once more for records that ended up duplicating across the joins
  ### use arrange to keep records with joining lookup values over identical records with missing lookup values
  arrange(id,
          inmate_id,
          booking_id,
          statute_title_lookup) %>%
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  ### as a check, at this point our total record should be identical to the cleaned jail file we started with -- it is
  ### now we want to de-dup by booking (one booking may have multiple charges associated) 
  ### keep most serious charge by crime type (hierarchy: violent, property, drug/alcohol, public order, vop)
  mutate(crime_type_severity_order = case_when(
    crime_type_lookup=="Violent" ~ 1,
    crime_type_lookup=="Property" ~ 2,
    crime_type_lookup=="Drug/Alcohol" ~ 3,
    crime_type_lookup=="Public Order" ~ 4,
    crime_type_lookup=="Probation/Parole Violation" ~ 5,
    is.na(crime_type_lookup)==TRUE ~ 6,
    TRUE ~ as.numeric(NA))) %>%
  arrange(id,
          inmate_id,
          booking_id,
          crime_type_severity_order) %>% 
  ### de-dup by booking, keeping the record with the most serious crime type
  distinct(id,
           inmate_id,
           booking_id,
           .keep_all=TRUE) %>% 
  ### create missing_lookup_charge_data column to double check missingness with non-pc holds
  mutate(missing_lookup_charge_data = ifelse(is.na(statute_title_lookup)==TRUE,
                                             1,
                                             0))

### excluding PC holds, see what % of non-pc hold bookings join to lookup table

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
### missing clean charge data for 125 non-PC bookings -- all of which are missing charge codes and descriptions in jail-provided data
table(merrimack_adm_charge_clean_final$missing_lookup_charge_data,merrimack_adm_charge_clean_final$pc_hold)

##################################
# Rockingham
##################################

### clean up rockingham file
rockingham_adm_charge_clean <- rockingham_adm1 %>% 
  mutate(charge_desc_clean = tolower(charge_desc),
         charge_code_clean = tolower(charge_code)) %>% 
# fill charge code by charge descriptions (some descriptions are missing a code where others have one)
  group_by(charge_desc_clean) %>% 
  fill(charge_code_clean, 
       .direction = "downup") %>%
  ungroup() %>% 
  ### de-duping by individual (id + inmate_id), booking (booking_id), and charge 
  ### there appear to be some duplicates across these fields, 
  ### but we have no way of knowing if two identical charges for the same booking
  ### represents a duplicated entry or multiple counts of the same charge
  ### unfortunately, this will likely just be a limitation of this analysis
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc,
           .keep_all = TRUE) 

####### use three separate joins as jail charge data will join on either charge code, statute title, or descriptor

### join to charge_codes_lookup via charge_code_lookup
rockingham_adm_charge_clean_join_one <- rockingham_adm_charge_clean %>% 
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_code_clean"="charge_code_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_code_clean,
           .keep_all = TRUE) %>% 
  mutate(charge_code_lookup = ifelse(!is.na(descriptor_lookup) == TRUE,
                                     charge_code_clean,
                                     NA)) ### populate for eventual rbind where all three df's need same columns

### join to charge_codes_lookup via charge_desc_clean and statute_title_lookup
rockingham_adm_charge_clean_join_two <- rockingham_adm_charge_clean_join_one %>% 
  filter(is.na(descriptor_lookup) == TRUE) %>% 
  select(-c(charge_code_lookup,
            descriptor_lookup,
            statute_title_lookup,
            crime_type_lookup)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) for rbind  
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_desc_clean" = "statute_title_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  mutate(statute_title_lookup = ifelse(!is.na(descriptor_lookup) == TRUE,
                                       charge_desc_clean,
                                       NA)) ### populate for eventual rbind where all three df's need same columns 

### join to charge_codes_lookup via charge_desc_clean and descriptor_lookup
rockingham_adm_charge_clean_join_three <- rockingham_adm_charge_clean_join_two %>% 
  filter(is.na(descriptor_lookup) == TRUE) %>% 
  select(-c(charge_code_lookup,
            descriptor_lookup,
            statute_title_lookup,
            crime_type_lookup)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) for rbind  
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_desc_clean" = "descriptor_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  mutate(descriptor_lookup = ifelse(!is.na(statute_title_lookup) == TRUE,
                                    charge_desc_clean,
                                    NA)) ### populate for eventual rbind where all three df's need same columns 

### combine df's from first, second, and third attempts to join to lookup values
rockingham_adm_charge_clean_final <- rbind(rockingham_adm_charge_clean_join_one,
                                             rockingham_adm_charge_clean_join_two,
                                             rockingham_adm_charge_clean_join_three) %>% 
  ### de-dup once more for records that ended up duplicating across the joins
  ### use arrange to keep records with joining lookup values over identical records with missing lookup values
  arrange(id,
          inmate_id,
          booking_id,
          statute_title_lookup) %>%
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  ### as a check, at this point our total record should be identical to the cleaned jail file we started with -- it is
  ### now we want to de-dup by booking (one booking may have multiple charges associated) 
  ### keep most serious charge by crime type (hierarchy: violent, property, drug/alcohol, public order, vop)
  mutate(crime_type_severity_order = case_when(
    crime_type_lookup=="Violent" ~ 1,
    crime_type_lookup=="Property" ~ 2,
    crime_type_lookup=="Drug/Alcohol" ~ 3,
    crime_type_lookup=="Public Order" ~ 4,
    crime_type_lookup=="Probation/Parole Violation" ~ 5,
    is.na(crime_type_lookup)==TRUE ~ 6,
    TRUE ~ as.numeric(NA))) %>%
  arrange(id,
          inmate_id,
          booking_id,
          crime_type_severity_order) %>% 
  ### de-dup by booking, keeping the record with the most serious crime type
  distinct(id,
           inmate_id,
           booking_id,
           .keep_all=TRUE) %>% 
  ### create missing_lookup_charge_data column to double check missingness with non-pc holds
  mutate(missing_lookup_charge_data = ifelse(is.na(statute_title_lookup)==TRUE,
                                             1,
                                             0))

### excluding PC holds, see what % of non-pc hold bookings join to lookup table

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
### missing clean charge data for 16 non-PC bookings -- all of which are missing charge codes and descriptions in jail-provided data
table(rockingham_adm_charge_clean_final$missing_lookup_charge_data,rockingham_adm_charge_clean_final$pc_hold)

##################################
# Strafford
##################################

### NOTE: Strafford is missing both charge codes and charge descriptions. Unless we receive updated data,
### we'll have to exclude Strafford from all charge-specific analysis

##################################
# Sullivan
##################################

### clean up sullivan file
sullivan_adm_charge_clean <- sullivan_adm1 %>% 
  mutate(charge_desc_clean = tolower(charge_desc),
         charge_code_clean = tolower(charge_code)) %>% 
# fill charge code by charge descriptions (some descriptions are missing a code where others have one)
  group_by(charge_desc_clean) %>% 
  fill(charge_code_clean, 
       .direction = "downup") %>%
  ungroup() %>%
  ungroup() %>% 
  ### de-duping by individual (id + inmate_id), booking (booking_id), and charge 
  ### there appear to be some duplicates across these fields, 
  ### but we have no way of knowing if two identical charges for the same booking
  ### represents a duplicated entry or multiple counts of the same charge
  ### unfortunately, this will likely just be a limitation of this analysis
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc,
           .keep_all = TRUE) 

####### use three separate joins as jail charge data will join on either charge code, statute title, or descriptor

### join to charge_codes_lookup via charge_code_lookup
sullivan_adm_charge_clean_join_one <- sullivan_adm_charge_clean %>% 
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_code_clean"="charge_code_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_code_clean,
           .keep_all = TRUE) %>% 
  mutate(charge_code_lookup = ifelse(!is.na(descriptor_lookup) == TRUE,
                                     charge_code_clean,
                                     NA)) ### populate for eventual rbind where all three df's need same columns

### join to charge_codes_lookup via charge_desc_clean and statute_title_lookup
sullivan_adm_charge_clean_join_two <- sullivan_adm_charge_clean_join_one %>% 
  filter(is.na(descriptor_lookup) == TRUE) %>% 
  select(-c(charge_code_lookup,
            descriptor_lookup,
            statute_title_lookup,
            crime_type_lookup)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) for rbind  
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_desc_clean" = "statute_title_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  mutate(statute_title_lookup = ifelse(!is.na(descriptor_lookup) == TRUE,
                                       charge_desc_clean,
                                       NA)) ### populate for eventual rbind where all three df's need same columns 

### join to charge_codes_lookup via charge_desc_clean and descriptor_lookup
sullivan_adm_charge_clean_join_three <- sullivan_adm_charge_clean_join_two %>% 
  filter(is.na(descriptor_lookup) == TRUE) %>% 
  select(-c(charge_code_lookup,
            descriptor_lookup,
            statute_title_lookup,
            crime_type_lookup)) %>% ### remove these columns to avoid duplicates (e.g. .x and .y) for rbind  
  left_join(charge_codes_crime_type_lookup, 
            by = c("charge_desc_clean" = "descriptor_lookup")) %>% 
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  mutate(descriptor_lookup = ifelse(!is.na(statute_title_lookup) == TRUE,
                                    charge_desc_clean,
                                    NA)) ### populate for eventual rbind where all three df's need same columns 

### combine df's from first, second, and third attempts to join to lookup values
sullivan_adm_charge_clean_final <- rbind(sullivan_adm_charge_clean_join_one,
                                           sullivan_adm_charge_clean_join_two,
                                           sullivan_adm_charge_clean_join_three) %>% 
  ### de-dup once more for records that ended up duplicating across the joins
  ### use arrange to keep records with joining lookup values over identical records with missing lookup values
  arrange(id,
          inmate_id,
          booking_id,
          statute_title_lookup) %>%
  distinct(id,
           inmate_id,
           booking_id,
           charge_desc_clean,
           .keep_all = TRUE) %>% 
  ### as a check, at this point our total record should be identical to the cleaned jail file we started with -- it is
  ### now we want to de-dup by booking (one booking may have multiple charges associated) 
  ### keep most serious charge by crime type (hierarchy: violent, property, drug/alcohol, public order, vop)
  mutate(crime_type_severity_order = case_when(
    crime_type_lookup=="Violent" ~ 1,
    crime_type_lookup=="Property" ~ 2,
    crime_type_lookup=="Drug/Alcohol" ~ 3,
    crime_type_lookup=="Public Order" ~ 4,
    crime_type_lookup=="Probation/Parole Violation" ~ 5,
    is.na(crime_type_lookup)==TRUE ~ 6,
    TRUE ~ as.numeric(NA))) %>%
  arrange(id,
          inmate_id,
          booking_id,
          crime_type_severity_order) %>% 
  ### de-dup by booking, keeping the record with the most serious crime type
  distinct(id,
           inmate_id,
           booking_id,
           .keep_all=TRUE) %>% 
  ### create missing_lookup_charge_data column to double check missingness with non-pc holds
  mutate(missing_lookup_charge_data = ifelse(is.na(statute_title_lookup)==TRUE,
                                             1,
                                             0))

### excluding PC holds, see what % of non-pc hold bookings join to lookup table

### let's see how many non-pc holdings are still missing charge data after cleaning and joining to the lookup table
### missing clean charge data for 26 non-PC bookings -- all of which are missing charge codes and descriptions in jail-provided data
table(sullivan_adm_charge_clean_final$missing_lookup_charge_data,sullivan_adm_charge_clean_final$pc_hold)

#######################################################
# Join all eight counties together (excluding strafford)
#######################################################
nh_eight_county_charge_clean_final <- rbind(belknap_adm_charge_clean_final,
                                             carroll_adm_charge_clean_final,
                                             cheshire_adm_charge_clean_final,
                                             coos_adm_charge_clean_final,
                                             hillsborough_adm_charge_clean_final,
                                             merrimack_adm_charge_clean_final,
                                             rockingham_adm_charge_clean_final,
                                             sullivan_adm_charge_clean_final)


#######################################################
# Write out analytic files 
#######################################################

### individual county files
save(belknap_adm_charge_clean_final, file=paste0(sp_data_path, "/Data/r_data/offenses_clean/county/belknap_offenses_clean.Rda", sep = ""))
save(carroll_adm_charge_clean_final, file=paste0(sp_data_path, "/Data/r_data/offenses_clean/county/carroll_offenses_clean.Rda", sep = ""))
save(cheshire_adm_charge_clean_final, file=paste0(sp_data_path, "/Data/r_data/offenses_clean/county/cheshire_offenses_clean.Rda", sep = ""))
save(coos_adm_charge_clean_final, file=paste0(sp_data_path, "/Data/r_data/offenses_clean/county/coos_offenses_clean.Rda", sep = ""))
save(hillsborough_adm_charge_clean_final, file=paste0(sp_data_path, "/Data/r_data/offenses_clean/county/hillsborough_offenses_clean.Rda", sep = ""))
save(merrimack_adm_charge_clean_final, file=paste0(sp_data_path, "/Data/r_data/offenses_clean/county/merrimack_offenses_clean.Rda", sep = ""))
save(rockingham_adm_charge_clean_final, file=paste0(sp_data_path, "/Data/r_data/offenses_clean/county/rockingham_offenses_clean.Rda", sep = ""))
save(sullivan_adm_charge_clean_final, file=paste0(sp_data_path, "/Data/r_data/offenses_clean/county/sullivan_offenses_clean.Rda", sep = ""))

### overall file
save(nh_eight_county_charge_clean_final, file=paste0(sp_data_path, "/Data/r_data/offenses_clean/nh_eight_county_charge_clean_final.Rda", sep = ""))





### old -- from earlier exploration of missingness and creation of lookup table

# ################################################################################
# # Extract all charge codes and charge descriptions from records that didn't join
# ### Compile list of unique codes and descriptions for (possible) manual coding
# ################################################################################
# nh_eight_county_charge_join_missing <- rbind(belknap_adm_charge_clean_join_two,
#                                              carroll_adm_charge_clean_join_two, 
#                                              cheshire_adm_charge_clean_join_two,
#                                              coos_adm_charge_clean_join_two,
#                                              hillsborough_adm_charge_clean_join_two,
#                                              merrimack_adm_charge_clean_join_two,
#                                              rockingham_adm_charge_clean_join_two,
#                                              sullivan_adm_charge_clean_join_two) %>% 
#   filter(missing_charge_data_second_join==1) %>% 
#   ### fill charge code by charge descriptions (some descriptions are missing a code where others have one)
#   group_by(charge_desc_clean) %>%
#   fill(charge_code_clean,
#          .direction = "downup") %>%
#   ungroup() %>% 
#   ### recode NAs as "Missing" with charge codes and descriptions for frequency table
#   mutate(charge_code_clean = ifelse(is.na(charge_code_clean),"Missing",charge_code_clean),
#          charge_desc_clean = ifelse(is.na(charge_desc_clean),"Missing",charge_desc_clean)) 
# 
# 
# 
# ### make frequency table with charge codes/descriptions that did not link to lookup table
# 
# ### create denominator for entire table
# unique_charge_code_desc_sample_denom <- n_distinct(nh_eight_county_charge_join_missing$id,
#                                                    nh_eight_county_charge_join_missing$inmate_id,
#                                                    nh_eight_county_charge_join_missing$booking_id,
#                                                    nh_eight_county_charge_join_missing$booking_date,
#                                                    nh_eight_county_charge_join_missing$charge_code_clean,
#                                                    nh_eight_county_charge_join_missing$charge_desc_clean)
# ### build table
# table_nh_eight_county_missing_charge_counts <- nh_eight_county_charge_join_missing %>% 
#   group_by(charge_code_clean,
#            charge_desc_clean) %>% 
#   summarise(`Unique Charges (N)` = n_distinct(id,
#                                               inmate_id,
#                                               booking_id,
#                                               booking_date,
#                                               charge_code_clean,
#                                               charge_desc_clean),
#             `Unique Charges (%)` = scales::percent(`Unique Charges (N)`/unique_charge_code_desc_sample_denom,
#                                                    accuracy = .1)) %>% 
#   ungroup()
# 
# ### write out table to excel
# write.xlsx(table_nh_eight_county_missing_charge_counts,
#            file.path(sp_data_path,"Data/Offense Information/offenses_missing_from_lookup_file.xlsx"),
#            asTable = FALSE, 
#            overwrite = FALSE)
