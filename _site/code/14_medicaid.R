
############################################
# Project: JRI New Hampshire
# File: medicaid.R
# Last updated: February 28, 2023
# Author: Mari Roberts/Andrew Byrum

# Merge descriptions for diagnoses from data dictionary
############################################

# import medicaid data in `02_import.R`

# clean names of medicaid categories
medicaid_categories <- medicaid_categories.xlsx %>%
  clean_names() %>%
  distinct()

# clean names of medicaid enrollment and merge with categories
medicaid_enrollment <- medicaid_enrollment.xlsx %>%
  clean_names() %>%
  distinct() %>%
  rename(eligibility_code = eligibility_category) %>%
  left_join(medicaid_categories, by = "eligibility_code") %>%
  ### filter out eligibility dates with years of "0001" here
  ### correspondence with Uma -- my question: We are seeing some Medicaid eligibility begin dates with years of “0001”
  ### and Medicaid eligibility end dates with years of “9999” in the Medicaid Enrollment file.
  ### Are these most likely data entry errors? Or is there a consistent reason for these numbers?
  ### (e.g., “0001-01-01” as an eligibility start date or “9999-12-31” as an eligibility end date).
  ### Uma's responses: "For the 0001-01-01 – You should filter those out. I should have done that for you but must have missed that step.
  ### I used that data to identify records to exclude. An eligibility end date of 12/31/999 means an open ended eligibility."
  filter(year(ymd(as_date(eligibility_begin_date)))!=0001) %>%
  ### now recode eligibility end dates with years of "9999" as "2030" as these indicate open end dates
  ### or enrollments that were open at the time of the data pull; by recoding to 2030, we'll be sure
  ### to include them in our inclusion/exclusion business rules
  mutate(eligibility_end_date = ifelse(year(ymd(as_date(eligibility_end_date)))==9999,
                                       ymd(as_date("2030-01-01")),
                                       ymd(as_date(eligibility_end_date))),
         eligibility_end_date = ymd(as_date(eligibility_end_date)))

# clean data dictionary to be able to match secondary diagnoses codes to descriptions
# this is the icd-10 iteration to be used with diagnoses after 2015
medicaid_dictionary_icd_10 <- medicaid_dictionary.xlsx %>%
  clean_names() %>%
  distinct() %>%
  select(icd_code = icd_10_cm_code,
         icd_description = icd_10_cm_code_description,
         ccsr_category_description) %>%
  distinct()

# clean data dictionary to be able to match secondary diagnoses codes to descriptions
# this is the icd-9 iteration to be used with diagnoses before 2015
medicaid_dictionary_icd_9 <- read_excel(paste0(sp_data_path, "/Data/Medicaid Data/medicaid_diagnoses_lookup_icd_9_pre_2015.xlsx"),
                                       sheet = "Valid ICD-9 FY2023 & NF Exclude") %>%
  clean_names() %>%
  distinct() %>%
  dplyr::select(icd_code = code,
         icd_description = long_description_valid_icd_9_fy2023) %>%
  distinct()

# for icd-9 data, there are no stock ccsr category descriptions that come with the data
# but i've downloaded "ccs" category descriptions (ccs category descriptions seem to be the predecessor to ccsr categories)
medicaid_dictionary_icd_9_ccs_desc_crosswalk <- read_csv(paste0(sp_data_path, "/Data/Medicaid Data/ccs_description_icd_9_crosswalk.csv")) %>%
  clean_names() %>%
  mutate_all(~str_replace_all(.,"'","")) %>%
  mutate_all(trimws) %>% ### remove leading and trailing apostrophes and whitespace
  dplyr::select(icd_code = icd_9_cm_code,
                ccsr_category_description = ccs_category_description) %>%
  distinct()

### join medicaid_dictionary_icd_9 and the ccs crosswalk
medicaid_dictionary_icd_9_ccs <- left_join(medicaid_dictionary_icd_9,
                                           medicaid_dictionary_icd_9_ccs_desc_crosswalk,
                                           by = "icd_code")

###############################################################################################
# de-duplicate pure duplicates in medicaid_encounters.xlsx first
### correspondence with Uma on why there are pure duplicates in encounters file:
### "No I did not dedup. Remember I had given an example of an ED visit.
### An outpatient claim is billed for the hospital visit and also a professional
### claim is billed for the doctor who attended to the patient."

### note: for analysis of reimbursement, we will want the duplicated version of the
### medicaid encounters file
medicaid_encounters_xlsx_de_dup <- medicaid_encounters.xlsx %>%
  distinct()

# clean encounters file for post-2015 records using the medicaid_dictionary_icd_10 lookup table
medicaid_encounters_post_2015 <- medicaid_encounters_xlsx_de_dup %>%
  clean_names() %>%
  mutate(year = year(first_dos_dt)) %>%
  ### uma renamed some columns in the second upload; i am renaming them here as they appeared in the original pull
  rename_with(~str_remove(., '_fnl')) %>%
  # subset to data on or after 2015-10-01 which is when they started using the ICD-10 code system
  # see: https://www.cdc.gov/nchs/icd/icd10cm_pcs_background.htm
  filter(ymd(as_date(first_dos_dt)) >= as_date("2015-10-01")) %>%

  # remove periods
  mutate_at(vars(dx_prmry_cd, dx_admtng_cd:dx_scndry_cd10), ~ str_replace(., "\\.", "")) %>%

  # currently, we only have codes for secondary diagnoses
  # merge with descriptions from data dictionary provided
  # rename icd description as "dx_prmry_desc" which is the detailesd description for the dx_prmry_cd
  # repeat this for admission reason diagnosis (dx_admtng_cd) and secondary diagnoses (dx_scndry_cd1 to dx_scndry_cd10)

  left_join(medicaid_dictionary_icd_10, by = c('dx_prmry_cd'='icd_code')) %>%
  rename(dx_prmry_desc     = icd_description,
         dx_prmry_category = ccsr_category_description) %>%

  left_join(medicaid_dictionary_icd_10, by = c('dx_admtng_cd'='icd_code')) %>%
  rename(dx_admtng_desc = icd_description,
         dx_admtng_category = ccsr_category_description) %>%

  left_join(medicaid_dictionary_icd_10, by = c('dx_scndry_cd1'='icd_code')) %>%
  rename(dx_scndry_desc1 = icd_description,
         dx_scndry_category1 = ccsr_category_description) %>%

  left_join(medicaid_dictionary_icd_10, by = c('dx_scndry_cd2'='icd_code')) %>%
  rename(dx_scndry_desc2 = icd_description,
         dx_scndry_category2 = ccsr_category_description) %>%

  left_join(medicaid_dictionary_icd_10, by = c('dx_scndry_cd3'='icd_code')) %>%
  rename(dx_scndry_desc3 = icd_description,
         dx_scndry_category3 = ccsr_category_description) %>%

  left_join(medicaid_dictionary_icd_10, by = c('dx_scndry_cd4'='icd_code')) %>%
  rename(dx_scndry_desc4 = icd_description,
         dx_scndry_category4 = ccsr_category_description) %>%

  left_join(medicaid_dictionary_icd_10, by = c('dx_scndry_cd5'='icd_code')) %>%
  rename(dx_scndry_desc5 = icd_description,
         dx_scndry_category5 = ccsr_category_description) %>%

  left_join(medicaid_dictionary_icd_10, by = c('dx_scndry_cd6'='icd_code')) %>%
  rename(dx_scndry_desc6 = icd_description,
         dx_scndry_category6 = ccsr_category_description) %>%

  left_join(medicaid_dictionary_icd_10, by = c('dx_scndry_cd7'='icd_code')) %>%
  rename(dx_scndry_desc7 = icd_description,
         dx_scndry_category7 = ccsr_category_description) %>%

  left_join(medicaid_dictionary_icd_10, by = c('dx_scndry_cd8'='icd_code')) %>%
  rename(dx_scndry_desc8 = icd_description,
         dx_scndry_category8 = ccsr_category_description) %>%

  left_join(medicaid_dictionary_icd_10, by = c('dx_scndry_cd9'='icd_code')) %>%
  rename(dx_scndry_desc9 = icd_description,
         dx_scndry_category9 = ccsr_category_description) %>%

  left_join(medicaid_dictionary_icd_10, by = c('dx_scndry_cd10'='icd_code')) %>%
  rename(dx_scndry_desc10 = icd_description,
         dx_scndry_category10 = ccsr_category_description) %>%

  # order variables and remove codes since they aren't needed anymore
  select(unique_person_id,
         first_dos_dt,
         last_dos_dt,
         dx_prmry_clinical_classification,
         medicaid_reimb_amt,
         service_provided_by_cmhc_provider,
         ed_visit_or_service,
         mh_service_categorized_using_primary_dx_code,
         sud_service_categorized_using_primary_dx_code,
         mental_health_pharmacy_service,
         sud_pharmacy_service,
         other_service,
         year,

         dx_prmry_category,
         dx_admtng_category,
         dx_scndry_category1,
         dx_scndry_category2,
         dx_scndry_category3,
         dx_scndry_category4,
         dx_scndry_category5,
         dx_scndry_category6,
         dx_scndry_category7,
         dx_scndry_category8,
         dx_scndry_category9,
         dx_scndry_category10,

         dx_prmry_desc,
         dx_admtng_desc,
         dx_scndry_desc1,
         dx_scndry_desc2,
         dx_scndry_desc3,
         dx_scndry_desc4,
         dx_scndry_desc5,
         dx_scndry_desc6,
         dx_scndry_desc7,
         dx_scndry_desc8,
         dx_scndry_desc9,
         dx_scndry_desc10)

###############################################################################################
# clean encounters file for post-2015 records using the medicaid_dictionary_icd_10 lookup table
medicaid_encounters_pre_2015 <- medicaid_encounters_xlsx_de_dup %>%
  clean_names() %>%
  mutate(year = year(first_dos_dt)) %>%
  ### uma renamed some columns in the second upload; i am renaming them here as they appeared in the original pull
  rename_with(~str_remove(., '_fnl')) %>%
  # subset to data before 2015-10-01 which is when they started using the ICD-10 code system
  # see: https://www.cdc.gov/nchs/icd/icd10cm_pcs_background.htm
  filter(ymd(as_date(first_dos_dt)) < as_date("2015-10-01")) %>%

  # remove periods
  mutate_at(vars(dx_prmry_cd, dx_admtng_cd:dx_scndry_cd10), ~ str_replace(., "\\.", "")) %>%

  # currently, we only have codes for secondary diagnoses
  # merge with descriptions from data dictionary provided
  # rename icd description as "dx_prmry_desc" which is the detailed description for the dx_prmry_cd
  # repeat this for admission reason diagnosis (dx_admtng_cd) and secondary diagnoses (dx_scndry_cd1 to dx_scndry_cd10)

  left_join(medicaid_dictionary_icd_9_ccs, by = c('dx_prmry_cd'='icd_code')) %>%
  rename(dx_prmry_desc     = icd_description,
         dx_prmry_category = ccsr_category_description) %>%

  left_join(medicaid_dictionary_icd_9_ccs, by = c('dx_admtng_cd'='icd_code')) %>%
  rename(dx_admtng_desc = icd_description,
         dx_admtng_category = ccsr_category_description) %>%

  left_join(medicaid_dictionary_icd_9_ccs, by = c('dx_scndry_cd1'='icd_code')) %>%
  rename(dx_scndry_desc1 = icd_description,
         dx_scndry_category1 = ccsr_category_description) %>%

  left_join(medicaid_dictionary_icd_9_ccs, by = c('dx_scndry_cd2'='icd_code')) %>%
  rename(dx_scndry_desc2 = icd_description,
         dx_scndry_category2 = ccsr_category_description) %>%

  left_join(medicaid_dictionary_icd_9_ccs, by = c('dx_scndry_cd3'='icd_code')) %>%
  rename(dx_scndry_desc3 = icd_description,
         dx_scndry_category3 = ccsr_category_description) %>%

  left_join(medicaid_dictionary_icd_9_ccs, by = c('dx_scndry_cd4'='icd_code')) %>%
  rename(dx_scndry_desc4 = icd_description,
         dx_scndry_category4 = ccsr_category_description) %>%

  left_join(medicaid_dictionary_icd_9_ccs, by = c('dx_scndry_cd5'='icd_code')) %>%
  rename(dx_scndry_desc5 = icd_description,
         dx_scndry_category5 = ccsr_category_description) %>%

  left_join(medicaid_dictionary_icd_9_ccs, by = c('dx_scndry_cd6'='icd_code')) %>%
  rename(dx_scndry_desc6 = icd_description,
         dx_scndry_category6 = ccsr_category_description) %>%

  left_join(medicaid_dictionary_icd_9_ccs, by = c('dx_scndry_cd7'='icd_code')) %>%
  rename(dx_scndry_desc7 = icd_description,
         dx_scndry_category7 = ccsr_category_description) %>%

  left_join(medicaid_dictionary_icd_9_ccs, by = c('dx_scndry_cd8'='icd_code')) %>%
  rename(dx_scndry_desc8 = icd_description,
         dx_scndry_category8 = ccsr_category_description) %>%

  left_join(medicaid_dictionary_icd_9_ccs, by = c('dx_scndry_cd9'='icd_code')) %>%
  rename(dx_scndry_desc9 = icd_description,
         dx_scndry_category9 = ccsr_category_description) %>%

  left_join(medicaid_dictionary_icd_9_ccs, by = c('dx_scndry_cd10'='icd_code')) %>%
  rename(dx_scndry_desc10 = icd_description,
         dx_scndry_category10 = ccsr_category_description) %>%

  # order variables and remove codes since they aren't needed anymore
  select(unique_person_id,
         first_dos_dt,
         last_dos_dt,
         dx_prmry_clinical_classification,
         medicaid_reimb_amt,
         service_provided_by_cmhc_provider,
         ed_visit_or_service,
         mh_service_categorized_using_primary_dx_code,
         sud_service_categorized_using_primary_dx_code,
         mental_health_pharmacy_service,
         sud_pharmacy_service,
         other_service,
         year,

         dx_prmry_category,
         dx_admtng_category,
         dx_scndry_category1,
         dx_scndry_category2,
         dx_scndry_category3,
         dx_scndry_category4,
         dx_scndry_category5,
         dx_scndry_category6,
         dx_scndry_category7,
         dx_scndry_category8,
         dx_scndry_category9,
         dx_scndry_category10,

         dx_prmry_desc,
         dx_admtng_desc,
         dx_scndry_desc1,
         dx_scndry_desc2,
         dx_scndry_desc3,
         dx_scndry_desc4,
         dx_scndry_desc5,
         dx_scndry_desc6,
         dx_scndry_desc7,
         dx_scndry_desc8,
         dx_scndry_desc9,
         dx_scndry_desc10)

### combine medicaid_encounters_post_2015 and medicaid_encounters_pre_2015
medicaid_encounters <- rbind(medicaid_encounters_pre_2015,
                             medicaid_encounters_post_2015)

# save out to external hard drive
write_rds(medicaid_encounters,
          "D:/Analytic/medicaid_encounters.rds")


# Combine jail medicaid data files
# update: we now have race/gender data for belknap and coos
medicaid_jail_all <- rbind(belknap_medicaid,
                           carroll_medicaid,
                           cheshire_medicaid,
                           coos_medicaid,
                           hillsborough_medicaid,
                           merrimack_medicaid,
                           rockingham_medicaid,
                           strafford_medicaid,
                           sullivan_medicaid)

# create high utilizer variables using custom function
# create temporary id to use function then remove
medicaid_jail_all$id  <- medicaid_jail_all$unique_person_id
medicaid_jail_all_hus <- fnc_create_high_utilizer_variables(medicaid_jail_all)
medicaid_jail_all     <- medicaid_jail_all %>%
  left_join(medicaid_jail_all_hus, by = "id") %>%
  dplyr::select(-id)

# save out to external hard drive
write_rds(medicaid_jail_all,
          "D:/Analytic/medicaid_jail_all.rds")

################################################################################
# join medicaid_enrollment, medicaid_categories, medicaid_encounters, and medicaid_jail_all
################################################################################

### use tidylog to get stats when joining, filtering, and grouping
library(tidylog)

### join medicaid_enrollment_categories and medicaid_categories
### to join medicaid_encounters to medicaid_enrollment,
### the medicaid_encounters$first_dos_dt must be >= medicaid_enrollment$eligibility_begin_date
### and <= medicaid_enrollment$eligibility_end_date
medicaid_enrollment_categories_encounters <- left_join(medicaid_enrollment,
                                                       medicaid_encounters,
                                                       by = "unique_person_id") %>%

  ### we have joined all encounters/diagnoses to all medicaid enrollment records by the individual
  ### however, we only want to link encounters/diagnoses to the specific medicaid enrollment period during
  ### which the encounter/diagnosis took place
  ### in order to keep the ~6,000 enrollment records with no encounter records,
  ### require that either (1) enrollment records have no joined encounter data (using overall_bh_flag)
  ### OR (2) that encounter dates fall within given enrollment dates -- using keep_record_flag
  ### OR (3) for enrollment records that join to encounter records (i.e., have overall_bh_flag==1), but
  ### do not have encounters that fall within the provided enrollment periods (for these records, we
  ### want to remove their data from the encounter file)
  ### UPDATE ON GROUP 3:
  ### there are four individuals and six encounters where no encounters fell between
  ### the individual's eligibility start and end dates
  ### because it's such a small number, we'll treat these cases like we do the individuals
  ### who appear in the enrollment file but not the encounter file
  mutate(first_dos_dt = ymd(as_date(first_dos_dt)),
         eligibility_begin_date = ymd(as_date(eligibility_begin_date)),
         eligibility_end_date = ymd(as_date(eligibility_end_date)),
  ### create overall BH flag (indicating that individual has had at least one behavioral health "medicaid encounter",
  ### whether it was a primary or secondary diagnosis) that occurred during an enrollment period from enrollment file
  ### this is because not everyone from medicaid enrollment file joins to encounter file
         overall_bh_correct_dates_flag = ifelse(first_dos_dt >= eligibility_begin_date &
                                       first_dos_dt <= eligibility_end_date,
                                  1,0),
  ### recode overall_bh_flag to include 0's instead of NA's -- these are the records which appear
  ### in medicaid enrollment, but not in medicaid encounters
  ### also flag enrollment records that did not join to any BH encounter records
  ### ? for Uma: is it accurate to label these individuals as having enrolled in medicaid but not having any record of a
  ### BH-related Medicaid encounter
         overall_bh_no_merge_flag = ifelse(is.na(overall_bh_correct_dates_flag)==TRUE,
                                  1,0),
         overall_bh_flag = ifelse(overall_bh_correct_dates_flag==1,
                                                1,0),
         overall_bh_flag = ifelse(is.na(overall_bh_flag)==TRUE,
                           0,overall_bh_flag)) %>%
  ### now create individual-level flag to indicate if individual had any encounter records
  ### join to enrollment records (and with encounters that occurred between eligibility start and end dates)
  group_by(unique_person_id) %>%
  mutate(overall_bh_flag_max = max(overall_bh_flag,
                                   na.rm = TRUE)) %>%
  ungroup() %>%
  ### we want to keep records where either (1) encounters join to enrollment records
  ### and occur between eligibility start and end dates (using overall_bh_flag==1) OR
  ### (2) individuals from the enrollment file have no BH encounter records in the encounter
  ### file (using overall_bh_flag_max==0)
  mutate(keep_record_flag = ifelse(overall_bh_flag==1 | overall_bh_flag_max==0,
                                   1,0)) %>%
  ### need to convert columns from encounter file to character for next step
  mutate(across(first_dos_dt:dx_scndry_desc10,
                as.character)) %>%
  ### recode all medicaid encounter values to NA if overall_bh_flag==0
  ### this ensures that we do not accidentally use encounter data for individuals
  ### who only had encounters that did not occur during the enrollment periods that we received from DHHS
  mutate(across(first_dos_dt:dx_scndry_desc10,
                ~ if_else(overall_bh_flag==0,
                          "NA", .))) %>%
  ### reconvert flags to numeric in case we take sums/counts later
  mutate(across(c(homeless_on_eligbility_begin_date,
                  service_provided_by_cmhc_provider,
                  ed_visit_or_service,
                  mh_service_categorized_using_primary_dx_code,
                  sud_service_categorized_using_primary_dx_code,
                  mental_health_pharmacy_service,
                  sud_pharmacy_service,
                  other_service),
                as.numeric)) %>%
  dplyr::filter(keep_record_flag==1) %>%

  ### SMI definition now pulling from National Alliance on Mental Illness New Hampshire (NAMI NH)
  ### include:
# Schizophrenia (dx_prmry_clinical_classification=="Schizophrenia and other psychotic disorders", "Schizophrenia spectrum and other psychotic disorders")
# Bipolar Disorder ("Bipolar and related disorders")
# Major Depression ("Depressive disorders")
# Post-Traumatic Stress Disorder ("Trauma- and stressor-related disorders")
# Borderline Personality Disorder ("Personality disorders")
# Obsessive-Compulsive Disorder ("Obsessive-compulsive and related disorders", "Disruptive, impulse-control and conduct disorder")
# Panic Disorder,
# Phobias,
# Eating Disorders
### also included: "Mood disorders","Other specified and unspecified mood disorders"

# not currently coded, but perhaps should be: "Suicidal ideation/attempt/intentional self-harm","Anxiety and fear-related disorders","Anxiety disorders","Attention deficit conduct and disruptive behavior disorders"
# "Developmental disorders", "Hallucinogen-related disorders"

  ### then create flag for opioid-related primary diagnoses
  ### Current opioid-related diagnosis business rule: dx_prmry_clinical_classification=="Opioid-related disorders" (there's also "Substance-related disorders" w/ far fewer encounters)

  ### QUESTION TO ASK UMA: why do we have clinical classifications for primary dx that are not BH (e.g. gout or burns) -- do these have flags for primary diagnosis?
  ### decide on which classification/text to use

  ### TO DO: create excel list of all diagnoses with current grouping decisions to run by Mari, Sofia, and team
  ### also confirm that dx_prmry_clinical_classification is the best diagnosis classification to use

  mutate(smi_flag = ifelse(dx_prmry_clinical_classification%in%c("Schizophrenia and other psychotic disorders",
                                                                "Schizophrenia spectrum and other psychotic disorders",
                                                                "Depressive disorders",
                                                                "Bipolar and related disorders",
                                                                "Trauma- and stressor-related disorders",
                                                                "Personality disorders",
                                                                "Obsessive-compulsive and related disorders",
                                                                "Disruptive, impulse-control and conduct disorder",
                                                                "Mood disorders",
                                                                "Other specified and unspecified mood disorders"),
                          1,0),
         opioid_related_flag = ifelse(dx_prmry_clinical_classification=="Opioid-related disorders",
                                    1,0),
         alcohol_related_flag = ifelse(dx_prmry_clinical_classification=="Alcohol-related disorders",
                                       1,0),
         suicide_related_flag = ifelse(dx_prmry_clinical_classification=="Suicidal ideation/attempt/intentional self-harm",
                                       1,0))

### de-dup dataframe by individual to see how many individuals are in which group (1,2,3)
### looks like all but about ... of all individuals in medicaid enrollment file have
### at least one record in medicaid encounters file
### first create temp df that is unique by individual
### unique count of individuals: 13,276 (this is the same as the number of unique individuals in the enrollment file)
medicaid_enrollment_categories_encounters_dedup <- medicaid_enrollment_categories_encounters %>%
  distinct(unique_person_id,
           .keep_all=TRUE)

### how many individuals don't have any BH encounter records?
### of the 13,276 unique individuals in the file, ~2,500 had no BH encounter records (~19%)
### i wonder how does this finding, that 81% of the Medicaid sample had at least one BH Medicaid encounter
### compare to the general NH Medicaid population?
table(medicaid_enrollment_categories_encounters_dedup$overall_bh_no_merge_flag,
      useNA = 'always')






###############################################################################
### save medicaid_enrollment_categories_encounters to external hard drive - needed for reimbursement analysis
###############################################################################
write_rds(medicaid_enrollment_categories_encounters,
          "D:/Analytic/medicaid_enrollment_categories_encounters.rds")






############################################################################################
### now we will create two analytic files from medicaid_enrollment_categories_encounters
### and one analytic file from medicaid_enrollment

### See full notes and business rules for variable creation in data dictionary for analytic files here:
###  https://csgorg.sharepoint.com/:x:/s/Team-JC-Research/EUJfEABIuNNBvKARa4Kq1nwBvSLwsK4aH7K9oZDOWxbc1w?e=zLytTB

### three medicaid analytic files to create:

## 1. individual level file: this file will have flags based on the encounter-level
# raw file split into three time periods (encounters/enrollments prior to, during, and after the study window)

## 2. encounter/diagnosis level file: this file will allow us to explore the medicaid encounters/diagnosis-level data
# with greater specificity (e.g., counts of SUD encounters as well as proximity of either encounters or medicaid enrollment to booking)

## 3. medicaid enrollment level file: this file will allow us to explore the timing of medicaid enrollment/coverage loss and
# jail booking. did individuals lose their medicaid eligibility during their incarceration or shortly afterwards? if so,
# how long is the average jail LOS that results in medicaid coverage loss/un-enrollment? does this vary by county? by HU grouping?
############################################################################################

###########################################################################################################################
###########################################################################################################################
### 1. first, we will create an analytic file unique by individual (as opposed to unique by eligibility period or diagnosis)
### with summary flags for the 2018-2021 period; these flags will tell us if the individual received, for example
### any mental health or substance use disorder services for a primary diagnosis during the study window

medicaid_enrollment_categories_encounters_2018_2021_individual_level <- medicaid_enrollment_categories_encounters %>%
  ### first, we will group by the individual and create flags for services/diagnoses/statuses prior to 7/1/2018 as
  ### historic flags just in case we want them for analysis
  ### flags to create include:
  ### mh_service_categorized_using_primary_dx_code,
  ### sud_service_categorized_using_primary_dx_code
  ### homeless_on_eligbility_begin_date
  ### service_provided_by_cmhc_provider
  ### ed_visit_or_service
  ### raw count of ed_visit_or_service encounters
  ### mental_health_pharmacy_service
  ### sud_pharmacy_service
  ### other_service

  ### create pre_bh_mh_or_sud_service_primary_flag from these two using pmax to take max across columns:
  ### ..._mh_service_primary_flag and ..._sud_service_primary_flag
  ### and create secondary_dx_mh_sud using bh_mh_or_sud_service_secondary_dx_encounter_flag created below
  ### which is based on whether dx_scndry_desc1 is NA or not
  ### to determine if a given individual has primary/secondary/both primary and secondary diagnoses,
  ### we will use these two flags in conjunction

  ### also -- we are using Medicaid eligibility start and end dates for inclusion in study window
  ### (i.e. Medicaid eligibility end date >= 7/1/2018 & Medicaid eligibility start date <= 6/30/2021)

  ### update: adding 'pre_or_study_window_medicaid_match_flag' because we have decided to filter analysis on
  ### medicaid encounters that occur prior to or during the study period in the jail admin sample

  mutate(study_window_medicaid_match_flag = ifelse(eligibility_end_date >= as_date("2018-07-01") & eligibility_begin_date <= as_date("2021-06-30"),
                                                       1,0),
         pre_study_window_medicaid_match_flag = ifelse(eligibility_end_date < as_date("2018-07-01"),
                                        1,0),
         pre_or_study_window_medicaid_match_flag = ifelse(eligibility_begin_date <= as_date("2021-06-30"),
                                                       1,0),
         post_study_window_medicaid_match_flag = ifelse(eligibility_begin_date > as_date("2021-06-30"),
                                        1,0),
         ### here i'm creating an encounter-level flag for whether a record is flagged as BH-related due to
         ### a secondary diagnosis; if there is text in the first secondary diagnosis column, flagging as '1'
         bh_mh_or_sud_service_secondary_dx_encounter_flag = ifelse(!is.na(dx_scndry_desc1)==TRUE | dx_scndry_desc1 !="NA",
                                                                   1,0)) %>%
  group_by(unique_person_id) %>%

  ### create individual-level timing flags that take max of encounter-level data:
  ### whether they had any pre-study, study, or post-study window medicaid enrollment records
  mutate(study_window_medicaid_match_flag_overall = max(study_window_medicaid_match_flag,
                                                            na.rm=TRUE),
         pre_study_window_medicaid_match_flag_overall = max(pre_study_window_medicaid_match_flag,
                                                            na.rm=TRUE),
         pre_or_study_window_medicaid_match_flag_overall = max(pre_or_study_window_medicaid_match_flag,
                                                            na.rm=TRUE),
         post_study_window_medicaid_match_flag_overall = max(post_study_window_medicaid_match_flag,
                                                            na.rm=TRUE)) %>%

  ### first create pre-study window flags
  mutate(pre_bh_flag = max(overall_bh_flag[pre_study_window_medicaid_match_flag==1],
                           na.rm=TRUE),
         pre_mh_service_primary_dx_flag = max(mh_service_categorized_using_primary_dx_code[pre_study_window_medicaid_match_flag==1],
                                           na.rm=TRUE),
         pre_sud_service_primary_dx_flag = max(sud_service_categorized_using_primary_dx_code[pre_study_window_medicaid_match_flag==1],
                                            na.rm=TRUE),
         pre_bh_mh_or_sud_service_primary_dx_flag = pmax(pre_mh_service_primary_dx_flag,pre_sud_service_primary_dx_flag,
                                            na.rm=TRUE),
         pre_bh_mh_or_sud_service_secondary_dx_flag = max(bh_mh_or_sud_service_secondary_dx_encounter_flag[pre_study_window_medicaid_match_flag==1],
                                                          na.rm=TRUE),
         pre_homeless_on_eligibility_begin_flag = max(homeless_on_eligbility_begin_date[pre_study_window_medicaid_match_flag==1],
                                                      na.rm=TRUE),
         pre_service_provided_by_cmhc_provider_flag = max(service_provided_by_cmhc_provider[pre_study_window_medicaid_match_flag==1],
                                                          na.rm=TRUE),
         pre_ed_visit_or_service_flag = max(ed_visit_or_service[pre_study_window_medicaid_match_flag==1],
                                            na.rm=TRUE),
         pre_ed_visit_or_service_encounter_count = sum(ed_visit_or_service[pre_study_window_medicaid_match_flag==1],
                                                       na.rm=TRUE),
         pre_mental_health_pharmacy_service_flag = max(mental_health_pharmacy_service[pre_study_window_medicaid_match_flag==1],
                                                       na.rm=TRUE),
         pre_sud_pharmacy_service_flag = max(sud_pharmacy_service[pre_study_window_medicaid_match_flag==1],
                                             na.rm=TRUE),
         pre_other_service_flag = max(other_service[pre_study_window_medicaid_match_flag==1],
                                      na.rm=TRUE),
         pre_smi_flag = max(smi_flag[pre_study_window_medicaid_match_flag==1],
                            na.rm=TRUE),
         pre_opioid_related_flag = max(opioid_related_flag[pre_study_window_medicaid_match_flag==1],
                             na.rm=TRUE)) %>%

  ### then post-study window flags
  mutate(post_bh_flag = max(overall_bh_flag[post_study_window_medicaid_match_flag==1],
                           na.rm=TRUE),
         post_mh_service_primary_dx_flag = max(mh_service_categorized_using_primary_dx_code[post_study_window_medicaid_match_flag==1],
                                            na.rm=TRUE),
         post_sud_service_primary_dx_flag = max(sud_service_categorized_using_primary_dx_code[post_study_window_medicaid_match_flag==1],
                                             na.rm=TRUE),
         post_bh_mh_or_sud_service_primary_dx_flag = pmax(post_mh_service_primary_dx_flag,post_sud_service_primary_dx_flag,
                                               na.rm=TRUE),
         post_bh_mh_or_sud_service_secondary_dx_flag = max(bh_mh_or_sud_service_secondary_dx_encounter_flag[post_study_window_medicaid_match_flag==1],
                                                           na.rm=TRUE),
         post_homeless_on_eligibility_begin_flag = max(homeless_on_eligbility_begin_date[post_study_window_medicaid_match_flag==1],
                                                       na.rm=TRUE),
         post_service_provided_by_cmhc_provider_flag = max(service_provided_by_cmhc_provider[post_study_window_medicaid_match_flag==1],
                                                           na.rm=TRUE),
         post_ed_visit_or_service_flag = max(ed_visit_or_service[post_study_window_medicaid_match_flag==1],
                                             na.rm=TRUE),
         post_ed_visit_or_service_encounter_count = sum(ed_visit_or_service[post_study_window_medicaid_match_flag==1],
                                                       na.rm=TRUE),
         post_mental_health_pharmacy_service_flag = max(mental_health_pharmacy_service[post_study_window_medicaid_match_flag==1],
                                                        na.rm=TRUE),
         post_sud_pharmacy_service_flag = max(sud_pharmacy_service[post_study_window_medicaid_match_flag==1],
                                              na.rm=TRUE),
         post_other_service_flag = max(other_service[post_study_window_medicaid_match_flag==1],
                                       na.rm=TRUE),
         post_smi_flag = max(smi_flag[post_study_window_medicaid_match_flag==1],
                             na.rm=TRUE),
         post_opioid_related_flag = max(opioid_related_flag[post_study_window_medicaid_match_flag==1],
                                       na.rm=TRUE)) %>%

  ### then pre or study window flags
  mutate(pre_or_study_bh_flag                                    = max(overall_bh_flag[pre_or_study_window_medicaid_match_flag==1],
                                                                       na.rm=TRUE),
         pre_or_study_mh_service_primary_dx_flag                 = max(mh_service_categorized_using_primary_dx_code[pre_or_study_window_medicaid_match_flag==1],
                                                                       na.rm=TRUE),
         pre_or_study_sud_service_primary_dx_flag                = max(sud_service_categorized_using_primary_dx_code[pre_or_study_window_medicaid_match_flag==1],
                                                                       na.rm=TRUE),
         pre_or_study_bh_mh_or_sud_service_primary_dx_flag       = max(pre_or_study_mh_service_primary_dx_flag,pre_or_study_sud_service_primary_dx_flag,
                                                                       na.rm=TRUE),
         pre_or_study_bh_mh_or_sud_service_secondary_dx_flag     = max(bh_mh_or_sud_service_secondary_dx_encounter_flag[pre_or_study_window_medicaid_match_flag==1],
                                                                       na.rm=TRUE),
         pre_or_study_homeless_on_eligibility_begin_flag         = max(homeless_on_eligbility_begin_date[pre_or_study_window_medicaid_match_flag==1],
                                                                       na.rm=TRUE),
         pre_or_study_service_provided_by_cmhc_provider_flag     = max(service_provided_by_cmhc_provider[pre_or_study_window_medicaid_match_flag==1],
                                                                       na.rm=TRUE),
         pre_or_study_ed_visit_or_service_flag                   = max(ed_visit_or_service[pre_or_study_window_medicaid_match_flag==1],
                                                                       na.rm=TRUE),
         pre_or_study_ed_visit_or_service_related_to_smi_flag    = max(ed_visit_or_service[pre_or_study_window_medicaid_match_flag==1 & smi_flag==1],
                                                                       na.rm=TRUE),
         pre_or_study_ed_visit_or_service_related_to_opioid_flag = max(ed_visit_or_service[pre_or_study_window_medicaid_match_flag==1 & opioid_related_flag==1],
                                                                       na.rm=TRUE),
         pre_or_study_ed_visit_or_service_encounter_count        = sum(ed_visit_or_service[pre_or_study_window_medicaid_match_flag==1],
                                                                       na.rm=TRUE),
         pre_or_study_mental_health_pharmacy_service_flag        = max(mental_health_pharmacy_service[pre_or_study_window_medicaid_match_flag==1],
                                                                       na.rm=TRUE),
         pre_or_study_sud_pharmacy_service_flag                  = max(sud_pharmacy_service[pre_or_study_window_medicaid_match_flag==1],
                                                                       na.rm=TRUE),
         pre_or_study_other_service_flag                         = max(other_service[pre_or_study_window_medicaid_match_flag==1],
                                                                       na.rm=TRUE),
         pre_or_study_smi_flag                                   = max(smi_flag[pre_or_study_window_medicaid_match_flag==1],
                                                                       na.rm=TRUE),
         pre_or_study_opioid_related_flag                        = max(opioid_related_flag[pre_or_study_window_medicaid_match_flag==1],
                                                                       na.rm=TRUE),
         pre_or_study_alcohol_related_flag                       = max(alcohol_related_flag[pre_or_study_window_medicaid_match_flag==1],
                                                                       na.rm=TRUE),
         pre_or_study_suicide_related_flag                       = max(suicide_related_flag[pre_or_study_window_medicaid_match_flag==1],
                                                                       na.rm=TRUE)
         ) %>%

  ### now create flags for study window -- all prefixed with 'study_'
  mutate(study_bh_flag = max(overall_bh_flag[study_window_medicaid_match_flag==1],
                           na.rm=TRUE),
         study_mh_service_primary_dx_flag = max(mh_service_categorized_using_primary_dx_code[study_window_medicaid_match_flag==1],
                                          na.rm=TRUE),
         study_sud_service_primary_dx_flag = max(sud_service_categorized_using_primary_dx_code[study_window_medicaid_match_flag==1],
                                           na.rm=TRUE),
         study_bh_mh_or_sud_service_primary_dx_flag = pmax(study_mh_service_primary_dx_flag,study_sud_service_primary_dx_flag,
                                             na.rm=TRUE),
         study_bh_mh_or_sud_service_secondary_dx_flag = max(bh_mh_or_sud_service_secondary_dx_encounter_flag[study_window_medicaid_match_flag==1],
                                                            na.rm=TRUE),
         study_homeless_on_eligibility_begin_flag = max(homeless_on_eligbility_begin_date[study_window_medicaid_match_flag==1],
                                                     na.rm=TRUE),
         study_service_provided_by_cmhc_provider_flag = max(service_provided_by_cmhc_provider[study_window_medicaid_match_flag==1],
                                                         na.rm=TRUE),
         study_ed_visit_or_service_flag = max(ed_visit_or_service[study_window_medicaid_match_flag==1],
                                           na.rm=TRUE),
         study_ed_visit_or_service_encounter_count = sum(ed_visit_or_service[study_window_medicaid_match_flag==1],
                                                     na.rm=TRUE),
         study_mental_health_pharmacy_service_flag = max(mental_health_pharmacy_service[study_window_medicaid_match_flag==1],
                                                      na.rm=TRUE),
         study_sud_pharmacy_service_flag = max(sud_pharmacy_service[study_window_medicaid_match_flag==1],
                                            na.rm=TRUE),
         study_other_service_flag = max(other_service[study_window_medicaid_match_flag==1],
                              na.rm=TRUE),
         study_smi_flag = max(smi_flag[study_window_medicaid_match_flag==1],
                              na.rm=TRUE),
         study_opioid_related_flag = max(opioid_related_flag[study_window_medicaid_match_flag==1],
                                        na.rm=TRUE)) %>%
  ungroup() %>%
  ### for new flags, recode -inf as 0; this happened when we took the max of columns where the only value was NA
  mutate(across(.cols = pre_or_study_mh_service_primary_dx_flag:pre_or_study_suicide_related_flag,
                ~ ifelse(is.infinite(.x),
                         0, .x))) %>%
  ### now de-dup by individual
      distinct(unique_person_id, .keep_all=TRUE)





###############################################################################
### save medicaid_enrollment_categories_encounters_2018_2021_individual_level to external hard drive
###############################################################################
write_rds(medicaid_enrollment_categories_encounters_2018_2021_individual_level,
          "D:/Analytic/medicaid_enrollment_categories_encounters_2018_2021_individual_level_details.rds")






### removing all columns that were specific to encounter- or enrollment-level data
### since we have created an individual-level file with flags from the encounter and enrollment data,
### i am removing the now irrelevant columns to avoid any confusion
medicaid_enrollment_categories_encounters_2018_2021_individual_level <- medicaid_enrollment_categories_encounters_2018_2021_individual_level %>%
    dplyr::select(unique_person_id,
                  overall_bh_flag = overall_bh_flag_max,
                  study_window_medicaid_match_flag_overall:post_study_window_medicaid_match_flag_overall,
                  pre_or_study_bh_flag:pre_or_study_suicide_related_flag,
                  study_bh_flag:study_opioid_related_flag,
                  pre_bh_flag:pre_opioid_related_flag
                  ) %>%
      ### change unique_person_id to character for join with medicaid jail data
      mutate(unique_person_id = as.character(unique_person_id))

###############################################################################
### save medicaid_enrollment_categories_encounters_2018_2021_individual_level to external hard drive
###############################################################################
write_rds(medicaid_enrollment_categories_encounters_2018_2021_individual_level,
          "D:/Analytic/medicaid_analytic_individual_level.rds")

################################################################################
### join medicaid_enrollment_categories_encounters_2018_2021_individual_level to medicaid_jail_all_counties
### we want to keep all medicaid_jail_all_counties records and only those medicaid records which join
################################################################################
medicaid_enrollment_categories_encounters_individual_jail_all <- left_join(medicaid_jail_all,
                                                                medicaid_enrollment_categories_encounters_2018_2021_individual_level,
                                                                by = "unique_person_id")

###############################################################################
### save medicaid_enrollment_categories_encounters_individual_jail_all to external hard drive
###############################################################################
write_rds(medicaid_enrollment_categories_encounters_individual_jail_all,
          "D:/Analytic/jail_medicaid_analytic_individual_booking_level.rds")

###########################################################################################################################
###########################################################################################################################


###########################################################################################################################
###########################################################################################################################
## 2. next, we'll create the encounter/diagnosis level file: this file will allow us to explore the medicaid encounters/diagnosis-level data
# with greater specificity (e.g., counts of SUD encounters as well as proximity of either encounters or medicaid enrollment to booking)

### 1/27: We may only need this file for reimbursement analysis, if we are only interested in absence/presence of certain diagnoses
### as opposed to the counts of them


###########################################################################################################################
###########################################################################################################################


###########################################################################################################################
###########################################################################################################################
## 3. finally, we will build the medicaid enrollment level file: this file will allow us to explore the timing of medicaid enrollment/coverage loss and
# jail booking. did individuals lose their medicaid eligibility during their incarceration or shortly afterwards? if so,
# how long is the average jail LOS that results in medicaid coverage loss/un-enrollment? does this vary by county? by HU grouping?

#############################
### clean medicaid_enrollment
#############################

medicaid_enrollment_to_join <- medicaid_enrollment %>%
  ### change unique_person_id to character for join with medicaid jail data
  ### create unique medicaid enrollment id for grouping and de-duping in next step
  mutate(unique_person_id = as.character(unique_person_id),
         unique_medicaid_enrollment_id = paste0(unique_person_id,
                                                eligibility_begin_date)) %>%
  ### clean dates for analysis using dates from the enrollment and jail file
  mutate(eligibility_begin_date = ymd(as_date(eligibility_begin_date)),
         eligibility_end_date = ymd(as_date(eligibility_end_date))) %>%
  ### now de-dup by enrollment ID; there were a handful of cases where one individual/eligibility begin date
  ### were linked to different eligibility codes (<50/~60k); these cases are still de-duped by individual and begin date
  ### since the number is so small
  distinct(unique_medicaid_enrollment_id,
           .keep_all = TRUE) %>%
  ### now arrange and group by individual to take the next eligibility begin date
  ### following a record's eligibility end date using dplyr::lead to take the next start date
  ### after a given end date (https://dplyr.tidyverse.org/reference/lead-lag.html)
  ### this will allow us to calculate the average time from enrollment end to the next enrollment
  ### for individuals who lose eligibility during or soon after incarceration
  ### NA for next_eligibility_begin_date indicates no subsequent re-enrollment
  arrange(unique_person_id, eligibility_begin_date) %>%
  group_by(unique_person_id) %>%
  mutate(next_eligibility_begin_date = dplyr::lead(eligibility_begin_date,
                                                   1,
                                                   default=NA)) %>%
  ungroup() %>%
  ### now calculate time between two dates in days and flag enrollment records with no subsequent re-enrollment
  mutate(medicaid_eligibility_end_next_begin_diff_days = as.numeric(difftime(next_eligibility_begin_date,
                                                                    eligibility_end_date,
                                                                    units="days")),
         medicaid_eligibility_end_no_new_begin_flag = ifelse(is.na(medicaid_eligibility_end_next_begin_diff_days)==TRUE,
                                                             1,0))


############################
### clean medicaid_jail_all
###########################

### NOTE: if we want to look at county-level findings, we will need a business rule to create a county column that indicates the county that appears most frequently for each individual
### because we are ultimately de-duping by medicaid enrollment, not booking, there may be more than one
### county associated with a given enrollment period; this isn't a perfect approach, but we could keep
### the county that an individual is booked in most frequently; if an individual has been to
### two or more different jails the same number of times, we will choose one/de-dup randomly

medicaid_jail_all_to_join <- medicaid_jail_all %>%
  ### drop booking records with missing release dates -- these are likely folks still incarcerated at time of data pull
  ### this only affects 170/~50k records
  filter(!is.na(release_date)==TRUE) %>%
  ### clean dates for analysis using dates from the enrollment and jail file
  ### calculate length of stay using booking_date and release_date
  mutate(booking_date = ymd(as_date(booking_date)),
         release_date = ymd(as_date(release_date)),
         jail_los = as.numeric(difftime(release_date,
                                        booking_date,
                                        units="days"))) %>%
  ### only keep individual-level (not booking-level) columns since
  ### we are ultimately de-duping by medicaid enrollment
  ### jail_los is the exception -- but we'll account for that in the
  ### next step
  dplyr::select(unique_person_id,
                booking_id,
                booking_date,
                release_date,
                jail_los,
                jail_sex,
                jail_race,
                jail_dob_year,
                high_utilizer_4_times:high_utilizer_10_pct)

################################################################################
### join medicaid_jail_all_counties to medicaid_enrollment
### we want to keep all medicaid_enrollment records and only those jail records which join via unique_person_id
################################################################################
medicaid_enrollment_jail_timing_all <- left_join(medicaid_enrollment_to_join,
                                                 medicaid_jail_all_to_join,
                                                 by = "unique_person_id") %>%
  ### we ultimately want a file that is unique by medicaid enrollment
  ### since our denominator in analysis will be those who were booked in jail AND were enrolled in medicaid during the
  ### study window (i.e., those eligible to have their medicaid coverage ended due to incarceration),
  ### we will drop individuals who did not match to medicaid by left joining the jail data to the medicaid enrollment file
  ### now drop records which appear in medicaid enrollment but not jail file
  filter(!is.na(booking_id)) %>%
  ### create date difference variable for analysis using dates from the enrollment and jail file
  mutate(medicaid_enroll_ends_release_date_diff_days = as.numeric(difftime(eligibility_end_date,
                                                                release_date,
                                                                units="days"))) %>%
  ### now create several flags:
  ### did individual's medicaid enrollment end during incarceration in jail
  ### did individual's medicaid enrollment end within 0 to 5 days of release from jail
  ### did individual's medicaid enrollment end within 5 to 10 days of release from jail
  ### did individual's medicaid enrollment end within 10 to 15 days of release from jail
  ### did individual's medicaid enrollment end within 15 to 20 days of release from jail

  mutate(medicaid_enroll_ends_during_jail_booking_level = ifelse(eligibility_end_date>=booking_date & eligibility_end_date<=release_date,
                                                   1,0),
         medicaid_enroll_ends_after_jail_within_5_days_booking_level = ifelse(medicaid_enroll_ends_release_date_diff_days>0 & medicaid_enroll_ends_release_date_diff_days<=5,
                                                   1,0),
         medicaid_enroll_ends_after_jail_within_10_days_booking_level = ifelse(medicaid_enroll_ends_release_date_diff_days>5 & medicaid_enroll_ends_release_date_diff_days<=10,
                                                                1,0),
         medicaid_enroll_ends_after_jail_within_15_days_booking_level = ifelse(medicaid_enroll_ends_release_date_diff_days>10 & medicaid_enroll_ends_release_date_diff_days<=15,
                                                                1,0),
         medicaid_enroll_ends_after_jail_within_20_days_booking_level = ifelse(medicaid_enroll_ends_release_date_diff_days>15 & medicaid_enroll_ends_release_date_diff_days<=20,
                                                                1,0)) %>%

  ### create overall flag to indicate if enrollment ended either while individual was in jail OR within 20 days or release
  mutate(medicaid_enroll_ends_during_or_within_20_days_jail_booking_level = ifelse(medicaid_enroll_ends_during_jail_booking_level==1 |
                                                                       medicaid_enroll_ends_after_jail_within_5_days_booking_level==1 |
                                                                       medicaid_enroll_ends_after_jail_within_10_days_booking_level==1 |
                                                                       medicaid_enroll_ends_after_jail_within_15_days_booking_level==1 |
                                                                       medicaid_enroll_ends_after_jail_within_20_days_booking_level==1,
                                                                     1,0)) %>%

  ### At this point, our file is messy -- there are joins between all bookings and all enrollments for individuals who
  ### are in both files; for this to be a medicaid enrollment-level file, we now need to group by enrollment and take the max
  ### of the flags we just create; in other words -- if at any point a booking/period of incarceration overlapped with medicaid enrollment ending, we will capture it
  ### because multiple bookings may have occurred during a single period of medicaid enrollment, this allows us to capture any instance
  ### of medicaid enrollment ending within 20 days after jail release

  group_by(unique_medicaid_enrollment_id) %>%
  mutate(medicaid_enroll_ends_during_jail = max(medicaid_enroll_ends_during_jail_booking_level,
                                                na.rm=TRUE),
         medicaid_enroll_ends_after_jail_within_5_days = max(medicaid_enroll_ends_after_jail_within_5_days_booking_level,
                                                na.rm=TRUE),
         medicaid_enroll_ends_after_jail_within_10_days = max(medicaid_enroll_ends_after_jail_within_10_days_booking_level,
                                                na.rm=TRUE),
         medicaid_enroll_ends_after_jail_within_15_days = max(medicaid_enroll_ends_after_jail_within_15_days_booking_level,
                                                na.rm=TRUE),
         medicaid_enroll_ends_after_jail_within_20_days = max(medicaid_enroll_ends_after_jail_within_20_days_booking_level,
                                                na.rm=TRUE)) %>%

  ### note: it's possible that one medicaid enrollment record could have multiple flags
  ### if multiple bookings are linked to one enrollment record and both occur within 20 days of each other
  ### for this reason, we will keep the earliest occurring flag
  ### (e.g., keep medicaid_enroll_ends_during_jail over medicaid_enroll_ends_after_jail_within_5_days, etc)

  mutate(medicaid_enroll_ends_after_jail_within_20_days = ifelse((medicaid_enroll_ends_after_jail_within_15_days==1 |
                                                                   medicaid_enroll_ends_after_jail_within_10_days==1 |
                                                                   medicaid_enroll_ends_after_jail_within_5_days==1 |
                                                                   medicaid_enroll_ends_during_jail==1),
                                                                 0,medicaid_enroll_ends_after_jail_within_20_days),
         medicaid_enroll_ends_after_jail_within_15_days = ifelse((medicaid_enroll_ends_after_jail_within_10_days==1 |
                                                                   medicaid_enroll_ends_after_jail_within_5_days==1 |
                                                                   medicaid_enroll_ends_during_jail==1),
                                                                 0,medicaid_enroll_ends_after_jail_within_15_days),
         medicaid_enroll_ends_after_jail_within_10_days = ifelse((medicaid_enroll_ends_after_jail_within_5_days==1 |
                                                                   medicaid_enroll_ends_during_jail==1),
                                                                 0,medicaid_enroll_ends_after_jail_within_10_days),
         medicaid_enroll_ends_after_jail_within_5_days = ifelse(medicaid_enroll_ends_during_jail==1,
                                                                 0,medicaid_enroll_ends_after_jail_within_5_days)) %>%

  ungroup() %>%
  ### create overall flag to indicate if enrollment ended either while individual was in jail OR within 20 days or release
  mutate(medicaid_enroll_ends_during_or_within_20_days_jail = ifelse(medicaid_enroll_ends_during_jail==1 |
                                                                       medicaid_enroll_ends_after_jail_within_5_days==1 |
                                                                       medicaid_enroll_ends_after_jail_within_10_days==1 |
                                                                       medicaid_enroll_ends_after_jail_within_15_days==1 |
                                                                       medicaid_enroll_ends_after_jail_within_20_days==1,
                                                                     1,0)) %>%

  ### now we choose the specific jail LOS that is associated with the end of medicaid eligibility
  ### need logic for instances where there are two bookings similar to logic above
  ### that keeps booking closest to end of medicaid; if individual-level flag for end of medicaid eligibility and
  ### booking-level flag both equal 1, indicating that is the booking we are associating with the end of an enrollment, we want that LOS
  mutate(keep_booking_los = ifelse((medicaid_enroll_ends_during_jail_booking_level==1 & medicaid_enroll_ends_during_jail==1) |
                                     (medicaid_enroll_ends_after_jail_within_5_days_booking_level==1 & medicaid_enroll_ends_after_jail_within_5_days==1) |
                                     (medicaid_enroll_ends_after_jail_within_10_days_booking_level==1 & medicaid_enroll_ends_after_jail_within_10_days==1) |
                                     (medicaid_enroll_ends_after_jail_within_15_days_booking_level==1 & medicaid_enroll_ends_after_jail_within_15_days==1) |
                                     (medicaid_enroll_ends_after_jail_within_20_days_booking_level==1 & medicaid_enroll_ends_after_jail_within_20_days==1),
                                   1,0)) %>%
  ### now create enrollment-level variable for the LOS of the booking that led to end of medicaid eligibility
  ### on the off chance that two distinct bookings both have the same time to end of medicaid eligibility
  ### (e.g., two booking, each for <1 day occurred within 3 days of each other and medicaid eligibility ended within 5 days of both),
  ### we then take the mean jail_los to create jail_los_medicaid_enroll_end
  ### this column will only be populated for medicaid enrollment records that ended during incarceration or within 20 days of release
  group_by(unique_medicaid_enrollment_id) %>%
  mutate(jail_los_medicaid_enroll_ends = ifelse(medicaid_enroll_ends_during_or_within_20_days_jail==1,
                                               mean(jail_los[keep_booking_los==1],
                                                    na.rm=TRUE),NA)) %>%
  ungroup() %>%
  ### now de-dup by enrollment
  distinct(unique_medicaid_enrollment_id,
           .keep_all = TRUE) %>%
  ### because the file is de-duped by enrollment and not booking, we should only keep enrollment and individual-level data
  ### we can also keep HU grouping data because that is specific to the individual, not the booking
  dplyr::select(unique_person_id,
                unique_medicaid_enrollment_id,
                eligibility_code:long_desc,
                medicaid_eligibility_end_next_begin_diff_days:medicaid_eligibility_end_no_new_begin_flag,
                medicaid_enroll_ends_during_jail:medicaid_enroll_ends_during_or_within_20_days_jail,
                jail_sex,
                jail_race,
                jail_dob_year,
                jail_los_medicaid_enroll_ends,
                high_utilizer_4_times:high_utilizer_10_pct)

###############################################################################
### save medicaid_enrollment_jail_timing_all to external hard drive
###############################################################################
write_rds(medicaid_enrollment_jail_timing_all,
          "D:/Analytic/medicaid_enrollment_jail_timing_all.rds")

##############################
# for analysis for Uma
##############################

### clean medicaid_encounters_xlsx_de_dup and merge to medicaid_enrollment_jail_timing_all
medicaid_encounters_clean <- medicaid_encounters_xlsx_de_dup %>%
  clean_names() %>%
  mutate(first_dos_dt = ymd(as_date(first_dos_dt)),
         last_dos_dt = ymd(as_date(last_dos_dt)))

write_rds(medicaid_encounters_clean,
          "D:/Analytic/medicaid_encounters_de_dup_clean.rds")

###########################################################################################################################
###########################################################################################################################

