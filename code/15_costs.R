
############################################
# Project: JRI New Hampshire
# File: costs.R
# Last updated: March 9, 2023
# Author: Mari Roberts

# Calculate the daily average population and costs
# https://stackoverflow.com/questions/52304623/calculate-average-number-of-individuals-present-on-each-date-in-r
############################################

# Get Medicaid jail data
medicaid_jail_all <- read_rds("D:/Analytic/medicaid_jail_all.rds")

# DHHS data
# rename variables
# get fiscal year based on booking date
entrances_dhhs <- medicaid_jail_all %>%
  mutate(id = unique_person_id,
         fy = case_when(booking_date > "2018-06-30" & booking_date < "2019-07-01" ~ 2019,
                        booking_date > "2019-06-30" & booking_date < "2020-07-01" ~ 2020,
                        booking_date > "2020-06-30" & booking_date < "2021-07-01" ~ 2021),
         booking_date = ymd(as_date(booking_date)),
         release_date = ymd(as_date(release_date)),
         jail_los = as.numeric(difftime(release_date,
                                        booking_date,
                                        units="days")))

# create HU variable to Tier 1 = 1 percent, Tier 2 = 2-5 percent, Tier 3 = 6-10 percent
entrances_dhhs <- fnc_hu_group_exclusive(entrances_dhhs)

# Save ids and HU types
hu_ids <- entrances_dhhs %>% select(id, hu_group_exclusive, hu_group_overall, medicaid_match_flag) %>% distinct()

# Reported costs - use these if available
reported_costs <- county_budgets_reported.xlsx %>%
  clean_names() %>%
  select(county, cost_pp_per_day_reported = reported_cost_per_person_per_day)

# Extract county budgets - use these if cost wasn't reported
county_budgets <- county_budgets_calculated.xlsx %>%
  clean_names() %>%
  select(county, doc_budget)

# Remove entrances without release dates
# Remove entrances where release date is earlier than start date (one instance)
entrances <- entrances_dhhs %>% select(id, county, booking_id, booking_date, release_date) %>%
  distinct() %>%
  mutate(booking_date = as.Date(booking_date, format="%Y-%m-%d"),
         release_date = as.Date(release_date, format="%Y-%m-%d")) %>%
  mutate(lessthanstart = ifelse(release_date < booking_date, TRUE, FALSE)) %>%
  filter(!is.na(release_date)) %>%
  filter(lessthanstart == FALSE)

# Unpack the start_date and end_date to individual dates
# Takes 10-15 minutes to run
# Each date is listed for each person
entrances_unpacked <- data.frame()
for (i in 1:nrow(entrances)){
  expand  <-  data.frame(county = entrances$county[i],
                         id = entrances$id[i],
                         Dates = seq.Date(entrances$booking_date[i], entrances$release_date[i], 1))
  entrances_unpacked <- rbind(entrances_unpacked, expand)
}

##########
# By county
##########

# Calculate the number of individuals present in each county in each day
# Filter to dates in 2019
# Measure average daily population
# Calculate average cost per person per day and per year
daily_pop_costs <- entrances_unpacked %>% group_by(county, Dates) %>%
  dplyr::summarise(individuals = n()) %>%
  filter(Dates > "2018-06-30" & Dates < "2019-07-01") %>%
  group_by(county) %>%
  dplyr::summarise(avg_pop_fy19 = mean(individuals, na.rm=TRUE)) %>%
  left_join(county_budgets, by = "county") %>%
  mutate(cost_pp_per_year_calculated = doc_budget/avg_pop_fy19) %>%
  mutate(cost_pp_per_day_calculated = cost_pp_per_year_calculated/365)

# Use reported cost if given
daily_pop_costs <- daily_pop_costs %>%
  left_join(reported_costs, "county") %>%
  mutate(cost_pp_per_day_use = ifelse(is.na(cost_pp_per_day_reported), cost_pp_per_day_calculated, cost_pp_per_day_reported)) %>%
  select(county, avg_pop_fy19, cost_pp_per_day = cost_pp_per_day_use)

# average cost per person per day in NH
avg_cost_pp_per_day <- mean(daily_pop_costs$cost_pp_per_day)

####################
# By hu
####################

entrances_unpacked_hus <- entrances_unpacked %>% left_join(hu_ids, by = "id")

# By hu
daily_pop_costs_hu <- entrances_unpacked_hus %>%
  group_by(hu_group_exclusive, Dates) %>%
  dplyr::summarise(individuals = n()) %>%
  filter(Dates > "2018-06-30" & Dates < "2019-07-01") %>%
  group_by(hu_group_exclusive) %>%
  dplyr::summarise(avg_pop_fy19 = mean(individuals, na.rm=TRUE))

# By state
daily_pop_costs_state <- entrances_unpacked_hus %>% group_by(Dates) %>%
  dplyr::summarise(individuals = n()) %>%
  filter(Dates > "2018-06-30" & Dates < "2019-07-01") %>%
  dplyr::summarise(avg_pop_fy19 = mean(individuals, na.rm=TRUE)) %>%
  mutate(hu_group_exclusive = "State")

# Add data together
daily_pop_costs_hu <- rbind(daily_pop_costs_hu, daily_pop_costs_state)
daily_pop_costs_hu <- daily_pop_costs_hu %>%
  mutate(cost_pp_per_day = avg_cost_pp_per_day,
         cost_per_year = avg_pop_fy19*365*cost_pp_per_day)

####################
# By hu and only for those who matched to Medicaid
####################

# By hu and matched to Medicaid
daily_pop_costs_medicaid_match_hu <- entrances_unpacked_hus %>%
  filter(medicaid_match_flag == 1) %>%
  group_by(hu_group_exclusive, Dates) %>%
  dplyr::summarise(individuals = n()) %>%
  filter(Dates > "2018-06-30" & Dates < "2019-07-01") %>%
  group_by(hu_group_exclusive) %>%
  dplyr::summarise(avg_pop_fy19 = mean(individuals, na.rm=TRUE))

# By state and matched to Medicaid
daily_pop_costs_medicaid_match_state <- entrances_unpacked_hus %>% group_by(Dates) %>%
  filter(medicaid_match_flag == 1) %>%
  dplyr::summarise(individuals = n()) %>%
  filter(Dates > "2018-06-30" & Dates < "2019-07-01") %>%
  dplyr::summarise(avg_pop_fy19 = mean(individuals, na.rm=TRUE)) %>%
  mutate(hu_group_exclusive = "State")

# Add data together
daily_pop_costs_medicaid_match_hu <- rbind(daily_pop_costs_medicaid_match_hu, daily_pop_costs_medicaid_match_state)
daily_pop_costs_medicaid_match_hu <- daily_pop_costs_medicaid_match_hu %>%
  mutate(cost_pp_per_day = avg_cost_pp_per_day,
         cost_per_year = avg_pop_fy19*365*cost_pp_per_day)

# Save out to external hard drive
write_rds(daily_pop_costs,                   "D:/Analytic/daily_pop_costs.rds")
write_rds(daily_pop_costs_hu,                "D:/Analytic/daily_pop_costs_hu.rds")
write_rds(daily_pop_costs_medicaid_match_hu, "D:/Analytic/daily_pop_costs_medicaid_match_hu.rds")
