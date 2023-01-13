
############################################
# Project: JRI New Hampshire
# File: medicaid.R
# Last updated: November 29, 2022
# Author: Mari Roberts

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
                                       2030,
                                       eligibility_end_date))

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
  select(icd_code = code,
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
                             medicaid_encounters_post_2015) %>% 
  mutate(overall_bh_flag = 1) ### create overall BH flag (indicating that individual has had at least one behavioral health "medicaid encounter",
### whether it was a primary or secondary diagnosis); this is in case not everyone from medicaid enrollment file joins to encounter


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
  select(-id)

# save out to external hard drive
write_rds(medicaid_jail_all,
          "D:/Analytic/medicaid_jail_all.rds")

################################################################################
# join medicaid_enrollment, medicaid_categories, medicaid_encounters, and medicaid_jail_all
################################################################################

library(tidylog)

### we previously joined these two files to add short_desc and long_desc of eligibility codes to 
### medicaid_enrollment

# ### join medicaid_enrollment and medicaid_categories
# medicaid_enrollment_categories <- left_join(medicaid_enrollment,
#                                             medicaid_categories,
#                                             by = "eligibility_code")

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
  ### in doing this, we also drop the ~6,000 enrollment records with no encounter records; need to double check
  ### that this makes sense (i.e., that the population of individuals who appear in the enrollment file, but
  ### not the encounter isn't a special population of interest)
  mutate(first_dos_dt = ymd(as_date(first_dos_dt)),
         eligibility_begin_date = ymd(as_date(eligibility_begin_date)),
         eligibility_end_date = ymd(as_date(eligibility_end_date))) %>% 
  dplyr::filter(first_dos_dt >= eligibility_begin_date,
                first_dos_dt <= eligibility_end_date) 

### looking at the tidylog and this flag, looks like all but about 0.06% of 
### all individuals in medicaid enrollment file have at least one record in medicaid encounters file
table(medicaid_enrollment_categories_encounters$overall_bh_flag,
      useNA = 'always') 

############################################################################################
### now we will create two analytic files from medicaid_enrollment_categories_encounters

### two medicaid analytic files to create: 

## 1. individual level file: this file will have flags based on the encounter-level 
# raw file split into three time periods (encounters/enrollments prior to, during, and after the study window)

## 2. encounter/diagnosis level file: this file will allow us to explore the medicaid encounters/diagnosis-level data
# with greater specificity (e.g., counts of SUD encounters as well as proximity of either encounters or medicaid enrollment to booking)
############################################################################################


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
  ### and create secondary_dx_mh_sud using...if pre_bh_mh_or_sud_service_primary_flag==0 & other data from encounters file is present, 
  ### BH must be based on secondary analysis

  ### Also -- we are using Medicaid eligibility start and end dates for inclusion in study window 
  ### (i.e. Medicaid eligibility end date >= 7/1/2018 & Medicaid eligibility start date <= 6/30/2021)
  mutate(pre_study_window_medicaid_match_flag = ifelse(eligibility_end_date < as_date("2018-07-01"),
                                        1,0),
         post_study_window_medicaid_match_flag = ifelse(eligibility_begin_date > as_date("2021-06-30"),
                                        1,0),
         ### here i'm creating an encounter-level flag for whether a record is flagged as BH-related due to 
         ### a secondary diagnosis; if there is text in the first secondary diagnosis column, flagging as '1'
         bh_mh_or_sud_service_secondary_dx_encounter_flag = ifelse(!is.na(dx_scndry_desc1)==TRUE,
                                                                   1,0)) %>% 
  group_by(unique_person_id) %>% 
  ### first create pre-study window flags
  mutate(pre_mh_service_primary_dx_flag = max(mh_service_categorized_using_primary_dx_code[pre_study_window_medicaid_match_flag==1],
                                           na.rm=TRUE),
         pre_sud_service_primary_dx_flag = max(sud_service_categorized_using_primary_dx_code[pre_study_window_medicaid_match_flag==1],
                                            na.rm=TRUE),
         pre_bh_mh_or_sud_service_primary_dx_flag = pmax(pre_mh_service_primary_flag,pre_sud_service_primary_flag,
                                            na.rm=TRUE),
         pre_bh_mh_or_sud_service_secondary_dx_flag = max(bh_mh_or_sud_service_secondary_only_dx_encounter_flag[pre_study_window_medicaid_match_flag==1],
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
                                      na.rm=TRUE)) %>% 
  
  ####################################################################################################################
  ### HEREHEREHERE -- need to spot check all particularly pre_bh_mh_or_sud_service_secondary_flag (need to add pharmacy consideration here)
  ### need to create pre_bh_mh_or_sud_service_secondary_flag without grouping first and then take max with grouping
  ### for secondary diagnosis flag, do we also include pre_other_service_flag? does have BH encounter record have 
  ### a 1 for primary, secondary, mh pharmacy, sud pharmacy, or other service flag? what is other service?
  ####### final decision: i don't think we need a flag for secondary_dx only -- we can create that after the fact if there is 
  ### a 0 zero for primary dx and a 1 for secondary dx 
  ####################################################################################################################

  ### then post-study window flags
  # mutate(post_mh_service_primary_dx_flag = max(mh_service_categorized_using_primary_dx_code[post_study_window_medicaid_match_flag==1],
  #                                           na.rm=TRUE),
  #        post_sud_service_primary_dx_flag = max(sud_service_categorized_using_primary_dx_code[post_study_window_medicaid_match_flag==1],
  #                                            na.rm=TRUE),
  #        post_bh_mh_or_sud_service_primary_dx_flag = pmax(post_mh_service_primary_flag,post_sud_service_primary_flag,
  #                                              na.rm=TRUE),
  #        post_bh_mh_or_sud_service_secondary_dx_flag = max(bh_mh_or_sud_service_secondary_only_dx_encounter_flag[pre_study_window_medicaid_match_flag==1],na.rm=TRUE),
  #        post_homeless_on_eligibility_begin_flag = max(homeless_on_eligbility_begin_date[post_study_window_medicaid_match_flag==1],
  #                                                      na.rm=TRUE),
  #        post_service_provided_by_cmhc_provider_flag = max(service_provided_by_cmhc_provider[post_study_window_medicaid_match_flag==1],
  #                                                          na.rm=TRUE),
  #        post_ed_visit_or_service_flag = max(ed_visit_or_service[post_study_window_medicaid_match_flag==1],
  #                                            na.rm=TRUE),
  #        post_mental_health_pharmacy_service_flag = max(mental_health_pharmacy_service[post_study_window_medicaid_match_flag==1],
  #                                                       na.rm=TRUE),
  #        post_sud_pharmacy_service_flag = max(sud_pharmacy_service[post_study_window_medicaid_match_flag==1],
  #                                             na.rm=TRUE),
  #        post_other_service_flag = max(other_service[post_study_window_medicaid_match_flag==1],
  #                                      na.rm=TRUE)) %>%
  ungroup() 
# %>% 
### then we drop all records outside the study window (7/1/2018 - 6/30/2021)

#   filter(eligibility_end_date >= as_date("2018-07-01"),
#          eligibility_begin_date <= as_date("2021-06-30")) %>% 
#   group_by(unique_individual_id) %>% 
# ## now finally create flags for study window -- all prefixed with 'study_'
#   mutate(study_mh_service_primary_dx_flag = max(mh_service_categorized_using_primary_dx_code,
#                                           na.rm=TRUE),
#        study_sud_service_primary_dx_flag = max(sud_service_categorized_using_primary_dx_code,
#                                            na.rm=TRUE),
#        study_bh_mh_or_sud_service_primary_dx_flag = pmax(study_mh_service_primary_flag,study_sud_service_primary_flag,
#                                              na.rm=TRUE),
#        study_bh_mh_or_sud_service_secondary_dx_flag = max(bh_mh_or_sud_service_secondary_only_dx_encounter_flag[pre_study_window_medicaid_match_flag==1],na.rm=TRUE),
#        study_homeless_on_eligibility_begin_flag = max(homeless_on_eligbility_begin_date,
#                                                      na.rm=TRUE),
#        study_service_provided_by_cmhc_provider_flag = max(service_provided_by_cmhc_provider,
#                                                          na.rm=TRUE),
#        study_ed_visit_or_service_flag = max(ed_visit_or_service,
#                                            na.rm=TRUE),
#        study_mental_health_pharmacy_service_flag = max(mental_health_pharmacy_service,
#                                                       na.rm=TRUE),
#        study_sud_pharmacy_service_flag = max(sud_pharmacy_service,
#                                             na.rm=TRUE),
       # study_other_service_flag = max(other_service,
       #                               na.rm=TRUE)) %>% 
       # ungroup() %>%
### now de-dup by individual 
      # distinct(unique_person_id, .keep_all=TRUE) %>% 
### removing all columns that were specific to encounter- or enrollment-level data
### since we have created an individual-level file with flags from the encounter and enrollment data,
### i am removing the now irrelevant columns to avoid any confusion
      # dplyr::select(-c(...))

  
  




### join medicaid_enrollment_categories_encounters to medicaid_jail_all_counties
### we want to keep all medicaid_jail_all_counties records and only those medicaid records which join
### to join medicaid_jail_all_counties to medicaid_enrollment, 
### medicaid_jail_all_counties$booking_date must be >= medicaid_enrollment$eligibility_begin_date 
### and <= medicaid_enrollment$eligibility_end_date
medicaid_enrollment_categories_encounters_individual_jail_all <- left_join(medicaid_jail_all,
                                                                medicaid_enrollment_categories_encounters_2018_2021_individual_level,
                                                                by = "unique_person_id") 
# %>% 
  ### we have joined all jail bookings to all medicaid enrollment records by the individual
  ### however, we only want to link bookings to the specific medicaid enrollment period during
  ### which the booking took place  
  ### if an individual has a booking during a time period where they are not enrolled in medicaid, 
  ### the medicaid data will not be linked to the jail data for that given booking
  ### ***confirm this logic with team***
  # dplyr::filter(booking_date >= eligibility_begin_date,
  #               booking_date <= eligibility_end_date)
