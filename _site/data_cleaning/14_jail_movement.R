
############################################
# Project: JRI New Hampshire
# File: jail_movement.R
# Last updated: November 29, 2022
# Author: Mari Roberts

# Check for movement between jails
############################################

# check to see if people are moving between jails
# get ids
belknap_ids <- belknap_medicaid %>%
  select(unique_person_id) %>%
  distinct()
carroll_ids <- carroll_medicaid %>%
  select(unique_person_id) %>%
  distinct()
cheshire_ids <- cheshire_medicaid %>%
  select(unique_person_id) %>%
  distinct()
coos_ids <- coos_medicaid %>%
  select(unique_person_id) %>%
  distinct()
hillsborough_ids <- hillsborough_medicaid %>%
  select(unique_person_id) %>%
  distinct()
merrimack_ids <- merrimack_medicaid %>%
  select(unique_person_id) %>%
  distinct()
rockingham_ids <- rockingham_medicaid %>%
  select(unique_person_id) %>%
  distinct()
strafford_ids <- strafford_medicaid %>%
  select(unique_person_id) %>%
  distinct()
sullivan_ids <- sullivan_medicaid %>%
  select(unique_person_id) %>%
  distinct()

# combine data
combined <- rbind(belknap_ids,
                  carroll_ids,
                  cheshire_ids,
                  coos_ids,
                  hillsborough_ids,
                  merrimack_ids,
                  rockingham_ids,
                  strafford_ids,
                  sullivan_ids)

# see duplicated rows/these are people moving between jails
duplicate_rows <- unique(combined[duplicated(combined), ])
