
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
  left_join(medicaid_categories, by = "eligibility_code")

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
# clean encounters file for post-2015 records using the medicaid_dictionary_icd_10 lookup table
medicaid_encounters_post_2015 <- medicaid_encounters.xlsx %>%
  clean_names() %>%
  mutate(year = year(first_dos_dt)) %>%
  ### uma renamed some columns in the second upload; i am renaming them here as they appeared in the original pull
  rename_with(~str_remove(., '_fnl')) %>%

  ####################### TO DO: confirm that ICD-10 was used during 2015 so that logic should be >=2015
  #subset to data after 2015 which is when they started using the ICD-10 code system
  filter(year >= 2015) %>%

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
medicaid_encounters_pre_2015 <- medicaid_encounters.xlsx %>%
  clean_names() %>%
  mutate(year = year(first_dos_dt)) %>%
  ### uma renamed some columns in the second upload; i am renaming them here as they appeared in the original pull
  rename_with(~str_remove(., '_fnl')) %>%

  ####################### TO DO: confirm that ICD-10 was used during 2015 so that logic should be >=2015
  #subset to data after 2015 which is when they started using the ICD-10 code system
  filter(year < 2015) %>%

  # remove periods
  mutate_at(vars(dx_prmry_cd, dx_admtng_cd:dx_scndry_cd10), ~ str_replace(., "\\.", "")) %>%

  # currently, we only have codes for secondary diagnoses
  # merge with descriptions from data dictionary provided
  # rename icd description as "dx_prmry_desc" which is the detailesd description for the dx_prmry_cd
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
  select(-id)

# save out to external hard drive
write_rds(medicaid_jail_all,
          "D:/Analytic/medicaid_jail_all.rds")
