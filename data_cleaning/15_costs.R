
############################################
# Project: JRI New Hampshire
# File: costs.R
# Last updated: January 6, 2023
# Author: Mari Roberts

# Calculate the daily average population
# https://stackoverflow.com/questions/52304623/calculate-average-number-of-individuals-present-on-each-date-in-r
# Check for movement between jails
############################################

# Extract county budgets
county_budgets <- county_budgets.xlsx %>%
  clean_names() %>%
  select(county, doc_budget)

# DHHS data
# create temporary id for function and create fy variable
entrances_dhhs <- medicaid_jail_all %>%
  mutate(id = unique_person_id,
         fy = case_when(booking_date > "2018-06-30" & booking_date < "2019-07-01" ~ 2019,
                        booking_date > "2019-06-30" & booking_date < "2020-07-01" ~ 2020,
                        booking_date > "2020-06-30" & booking_date < "2021-07-01" ~ 2021))

# remove entrances without release dates
# remove entrances where release date is earlier than start date (one instance)
df1 <- entrances_dhhs %>% select(id, county, booking_id, booking_date, release_date) %>%
  distinct() %>%
  mutate(booking_date = as.Date(booking_date, format="%Y-%m-%d"),
         release_date = as.Date(release_date, format="%Y-%m-%d")) %>%
  mutate(lessthanstart = ifelse(release_date < booking_date, TRUE, FALSE)) %>%
  filter(!is.na(release_date)) %>%
  filter(lessthanstart == FALSE)

# unpack the start_date and end_date to individual dates
# takes 10-15 minutes to run
df2 <- data.frame()
for (i in 1:nrow(df1)){
  expand  <-  data.frame(county = df1$county[i],
                         id = df1$id[i],
                         Dates = seq.Date(df1$booking_date[i], df1$release_date[i], 1))
  df2 <- rbind(df2, expand)
}

# calculate the number of other individuals present in each county in each day
# remove dates after the study time frame (July 1, 2018 to June 30, 2021)
daily_pop_costs <- df2 %>% group_by(county, Dates) %>%
  dplyr::summarise(individuals = n()) %>%
  filter(Dates > "2020-06-30" & Dates < "2021-07-01") %>%
  group_by(county) %>%
  dplyr::summarise(avg_pop_fy21 = mean(individuals, na.rm=TRUE)) %>%
  left_join(county_budgets, by = "county") %>%
  mutate(cost_pp_per_year = doc_budget/avg_pop_fy21) %>%
  mutate(cost_pp_per_day = cost_pp_per_year/365)

# save out to external hard drive
write_rds(daily_pop_costs,
          "D:/Analytic/daily_pop_costs.rds")

################################################################################

# # check to see if people are moving between jails
# # get ids
# belknap_ids <- belknap_medicaid %>%
#   select(unique_person_id) %>%
#   distinct()
# carroll_ids <- carroll_medicaid %>%
#   select(unique_person_id) %>%
#   distinct()
# cheshire_ids <- cheshire_medicaid %>%
#   select(unique_person_id) %>%
#   distinct()
# coos_ids <- coos_medicaid %>%
#   select(unique_person_id) %>%
#   distinct()
# hillsborough_ids <- hillsborough_medicaid %>%
#   select(unique_person_id) %>%
#   distinct()
# merrimack_ids <- merrimack_medicaid %>%
#   select(unique_person_id) %>%
#   distinct()
# rockingham_ids <- rockingham_medicaid %>%
#   select(unique_person_id) %>%
#   distinct()
# strafford_ids <- strafford_medicaid %>%
#   select(unique_person_id) %>%
#   distinct()
# sullivan_ids <- sullivan_medicaid %>%
#   select(unique_person_id) %>%
#   distinct()
#
# # combine data
# combined <- rbind(belknap_ids,
#                   carroll_ids,
#                   cheshire_ids,
#                   coos_ids,
#                   hillsborough_ids,
#                   merrimack_ids,
#                   rockingham_ids,
#                   strafford_ids,
#                   sullivan_ids)
#
# # see duplicated rows/these are people moving between jails
# duplicate_rows <- unique(combined[duplicated(combined), ])
