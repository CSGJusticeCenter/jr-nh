
############################################
# Project: JRI New Hampshire
# File: costs.R
# Last updated: March 23, 2023
# Author: Mari Roberts

# Calculate the daily average population and costs
# https://stackoverflow.com/questions/52304623/calculate-average-number-of-individuals-present-on-each-date-in-r
############################################

# get Medicaid jail data
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

# save ids and HU types
hu_ids <- entrances_dhhs %>% select(id, hu_group_exclusive, hu_group_overall, medicaid_match_flag) %>% distinct()

# reported costs - use these if available
reported_costs <- county_budgets_reported.xlsx %>%
  clean_names() %>%
  select(county, cost_pp_per_day_reported = reported_cost_per_person_per_day)

# extract county budgets - use these if cost wasn't reported
county_budgets <- county_budgets_calculated.xlsx %>%
  clean_names() %>%
  select(county, doc_budget)

# remove entrances without release dates
# remove entrances where release date is earlier than start date (one instance)
entrances <- entrances_dhhs %>% select(id, county, booking_id, booking_date, release_date, num_entrances, fy) %>%
  distinct() %>%
  mutate(booking_date = ymd(as_date(booking_date)),
         release_date = ymd(as_date(release_date)),
         jail_los = as.numeric(difftime(release_date,
                                        booking_date,
                                        units="days"))) %>%
  mutate(lessthanstart = ifelse(release_date < booking_date, TRUE, FALSE)) %>%
  filter(!is.na(release_date)) %>%
  filter(lessthanstart == FALSE) %>%
  select(-lessthanstart)



##########
# Get average cost per person using jail data and DOC budgets
# $ 199.82
##########

# unpack the start_date and end_date to individual dates
# takes ~30 minutes to run
# probably a faster way to do this
# each date is listed for each person
entrances_unpacked <- data.frame()
for (i in 1:nrow(entrances)){
  expand  <-  data.frame(county = entrances$county[i],
                         id = entrances$id[i],
                         Dates = seq.Date(entrances$booking_date[i], entrances$release_date[i], 1))
  entrances_unpacked <- rbind(entrances_unpacked, expand)
}



# calculate the number of individuals present in each county in each day
# filter to dates in 2019
# measure average daily population
# calculate average cost per person per day and per year
daily_pop_costs <- entrances_unpacked %>%
  group_by(county, Dates) %>%
  dplyr::summarise(individuals = n_distinct(id)) %>%

  # filter(Dates > "2018-06-30" & Dates < "2019-07-01") %>%
  filter(Dates > "2019-01-01" & Dates < "2019-12-31") %>%

  group_by(county) %>%
  dplyr::summarise(avg_pop_fy19 = mean(individuals, na.rm=TRUE)) %>%
  left_join(county_budgets, by = "county") %>%
  mutate(cost_pp_per_year_calculated = doc_budget/avg_pop_fy19) %>%
  mutate(cost_pp_per_day_calculated  = cost_pp_per_year_calculated/365)

# use reported cost if given
daily_pop_costs <- daily_pop_costs %>%
  left_join(reported_costs, "county") %>%
  mutate(cost_pp_per_day_use = ifelse(is.na(cost_pp_per_day_reported), cost_pp_per_day_calculated, cost_pp_per_day_reported)) %>%
  select(county, avg_pop_fy19, cost_pp_per_day = cost_pp_per_day_use)

# average cost per person per day in NH
avg_cost_pp_per_day <- mean(daily_pop_costs$cost_pp_per_day)
avg_cost_pp_per_day <- round(avg_cost_pp_per_day, 2)



################################################################################


# Cost Per Year
# METHDOLOGY 1 - TO LOW??????       DONT USE

# Average cost to incarcerate each person in NH jails in 2019
    # $199.77*Median LOS(2)*Median number of Jail Entrances Per Person(1)
# Average cost to incarcerate 13,869 individuals in NH jails in 2019.
    # Avg Cost Per Person Per Year in 2019($399.55)* Number of People Entered Jail in 2019(13,869)


################################################################################

# ##########
# # Get Median LOS - IN PRESENTATION
# ##########
#
# # get min med mean max of jail entrances for all
# los_summary_19 <- entrances %>%
#   filter(booking_date > "2018-06-30" & booking_date < "2019-07-01") %>%
#   group_by() %>%
#   summarise(
#     min    = min(jail_los, na.rm = T),
#     median = median(jail_los, na.rm = T),
#     mean   = mean(jail_los, na.rm = T),
#     max    = max(jail_los, na.rm = T)) %>%
#   mutate(mean = round(mean, 1)) %>%
#   mutate(hu_group_exclusive = "All (HU's and non-HU's)") %>% select(hu_group_exclusive, everything())
#
# # assign median los value
# median_los_19 <- los_summary_19$median # IN PRESENTATION
# median_los_19
#
# ##########
# # Get Median Entrances Per Person
# ##########
#
# # median number of entrances/fy by state
# median_19 <- entrances %>%
#   filter(booking_date > "2018-06-30" & booking_date < "2019-07-01") %>%
#   ungroup() %>%
#   select(id, num_entrances) %>%
#   distinct() %>%
#   group_by() %>%
#   dplyr::summarize(median_entrances_19 = median(num_entrances, na.rm=TRUE))
#
# # assign median num entrances value
# median_19 <- median_19$median_entrances_19 # IN PRESENTATION
# median_19
#
# ##########
# # Calculate costs
# ##########
#
# avg_cost_pp_19 <- round(avg_cost_pp_per_day*median_los*median, 2)
#
# avg_cost_per_year <- entrances %>%
#   filter(booking_date > "2018-06-30" & booking_date < "2019-07-01") %>%
#   dplyr::summarise(num_individuals = n_distinct(id)) %>%
#   mutate(cost = num_individuals*avg_cost_pp_19)
# avg_cost_per_year <- avg_cost_per_year$cost # IN PRESENTATION
# avg_cost_per_year




################################################################################


# Cost Per Year
# METHDOLOGY 2 - DONT USE

# Average cost to incarcerate individuals in NH jails in 2019.
# Avg Cost Per Person Per Day in 2019($199.77)*Average Daily Population in 2019*365


################################################################################

# avg_pop_fy19 <- entrances_unpacked %>%
#   group_by(Dates) %>%
#   dplyr::summarise(individuals = n_distinct(id)) %>%
#   filter(Dates > "2018-06-30" & Dates < "2019-07-01") %>%
#   dplyr::summarise(avg_pop_fy19 = mean(individuals, na.rm=TRUE))
#
# avg_pop_fy19$avg_pop_fy19*avg_cost_pp_per_day*365







################################################################################


# Overall and HU Costs
# Regardless of Medicaid match
# METHDOLOGY 1 - USE


################################################################################

####################
# by hu
####################

# add details about high utilizers
entrances_unpacked_hus <- entrances_unpacked %>% left_join(hu_ids, by = "id")

# get average population by hu
daily_pop_costs_hu <- entrances_unpacked_hus %>%
  group_by(hu_group_exclusive, Dates) %>%
  dplyr::summarise(individuals = n_distinct(id)) %>%
  # filter(Dates > "2018-06-30" & Dates < "2019-07-01") %>%
  filter(Dates > "2019-01-01" & Dates < "2019-12-31") %>%
  group_by(hu_group_exclusive) %>%
  dplyr::summarise(avg_pop_fy19 = mean(individuals, na.rm=TRUE)) %>%
  mutate(avg_pop_fy19 = round(avg_pop_fy19, 0))

# get average population by state
daily_pop_costs_state <- entrances_unpacked_hus %>% group_by(Dates) %>%
  dplyr::summarise(individuals = n_distinct(id)) %>%
  # filter(Dates > "2018-06-30" & Dates < "2019-07-01") %>%
  filter(Dates > "2019-01-01" & Dates < "2019-12-31") %>%
  dplyr::summarise(avg_pop_fy19 = mean(individuals, na.rm=TRUE)) %>%
  mutate(hu_group_exclusive = "State") %>%
  mutate(avg_pop_fy19 = round(avg_pop_fy19, 0))

# add data together # IN PRESENTATION
daily_pop_costs_hu <- rbind(daily_pop_costs_hu, daily_pop_costs_state)
daily_pop_costs_hu <- daily_pop_costs_hu %>%
  mutate(cost_pp_per_day = avg_cost_pp_per_day,
         cost_per_year = avg_pop_fy19*365*cost_pp_per_day)

# total HU Cost in 2019 # IN PRESENTATION
total_hu <- daily_pop_costs_hu %>%
  filter(hu_group_exclusive == "Tier 1 HU" |
         hu_group_exclusive == "Tier 2 HU" |
         hu_group_exclusive == "Tier 3 HU") %>%
  summarise(total = sum(cost_per_year))

# HU proportion of all costs # IN PRESENTATION
total <- daily_pop_costs_hu %>%
  filter(hu_group_exclusive == "State")
round((total_hu/total$cost_per_year)*100, 0)





################################################################################


# Costs Per Year
# by hu and only for those who matched to Medicaid
# METHDOLOGY 1


################################################################################

#########
# 2019
#########

# get average population by hu and matched to Medicaid in 2019
daily_pop_costs_medicaid_match_hu_19 <- entrances_unpacked_hus %>%
  filter(medicaid_match_flag == 1) %>%
  group_by(hu_group_exclusive, Dates) %>%
  dplyr::summarise(individuals = n_distinct(id)) %>%
  filter(Dates > "2018-06-30" & Dates < "2019-07-01") %>%
  group_by(hu_group_exclusive) %>%
  dplyr::summarise(avg_pop_fy19 = mean(individuals, na.rm=TRUE)) %>%
  mutate(avg_pop_fy19 = round(avg_pop_fy19, 0))


# get average population by state and matched to Medicaid in 2019
daily_pop_costs_medicaid_match_state_19 <- entrances_unpacked_hus %>% group_by(Dates) %>%
  filter(medicaid_match_flag == 1) %>%
  dplyr::summarise(individuals = n_distinct(id)) %>%
  filter(Dates > "2018-06-30" & Dates < "2019-07-01") %>%
  dplyr::summarise(avg_pop_fy19 = mean(individuals, na.rm=TRUE)) %>%
  mutate(avg_pop_fy19 = round(avg_pop_fy19, 0)) %>%
  mutate(hu_group_exclusive = "State")

# Add data together
daily_pop_costs_medicaid_match_hu_19 <- rbind(daily_pop_costs_medicaid_match_hu_19, daily_pop_costs_medicaid_match_state_19)
daily_pop_costs_medicaid_match_hu_19 <- daily_pop_costs_medicaid_match_hu_19 %>%
  mutate(cost_pp_per_day = avg_cost_pp_per_day,
         cost_per_year = avg_pop_fy19*365*cost_pp_per_day)

#########
# 2020
#########

# get average population by hu and matched to Medicaid in 2020
daily_pop_costs_medicaid_match_hu_20 <- entrances_unpacked_hus %>%
  filter(medicaid_match_flag == 1) %>%
  group_by(hu_group_exclusive, Dates) %>%
  dplyr::summarise(individuals = n_distinct(id)) %>%
  filter(Dates > "2019-06-30" & Dates < "2020-07-01") %>%
  group_by(hu_group_exclusive) %>%
  dplyr::summarise(avg_pop_fy20 = mean(individuals, na.rm=TRUE))%>%
  mutate(avg_pop_fy20 = round(avg_pop_fy20, 0))

# get average population by state and matched to Medicaid in 2020
daily_pop_costs_medicaid_match_state_20 <- entrances_unpacked_hus %>% group_by(Dates) %>%
  filter(medicaid_match_flag == 1) %>%
  dplyr::summarise(individuals = n_distinct(id)) %>%
  filter(Dates > "2019-06-30" & Dates < "2020-07-01") %>%
  dplyr::summarise(avg_pop_fy20 = mean(individuals, na.rm=TRUE)) %>%
  mutate(hu_group_exclusive = "State")%>%
  mutate(avg_pop_fy20 = round(avg_pop_fy20, 0))

# Add data together
daily_pop_costs_medicaid_match_hu_20 <- rbind(daily_pop_costs_medicaid_match_hu_20, daily_pop_costs_medicaid_match_state_20)
daily_pop_costs_medicaid_match_hu_20 <- daily_pop_costs_medicaid_match_hu_20 %>%
  mutate(cost_pp_per_day = avg_cost_pp_per_day,
         cost_per_year = avg_pop_fy20*365*cost_pp_per_day)

#########
# 2021
#########

# get average population by hu and matched to Medicaid in 2021
daily_pop_costs_medicaid_match_hu_21 <- entrances_unpacked_hus %>%
  filter(medicaid_match_flag == 1) %>%
  group_by(hu_group_exclusive, Dates) %>%
  dplyr::summarise(individuals = n_distinct(id)) %>%
  filter(Dates > "2020-06-30" & Dates < "2021-07-01") %>%
  group_by(hu_group_exclusive) %>%
  dplyr::summarise(avg_pop_fy21 = mean(individuals, na.rm=TRUE))%>%
  mutate(avg_pop_fy21 = round(avg_pop_fy21, 0))

# get average population by state and matched to Medicaid in 2021
daily_pop_costs_medicaid_match_state_21 <- entrances_unpacked_hus %>% group_by(Dates) %>%
  filter(medicaid_match_flag == 1) %>%
  dplyr::summarise(individuals = n_distinct(id)) %>%
  filter(Dates > "2020-06-30" & Dates < "2021-07-01") %>%
  dplyr::summarise(avg_pop_fy21 = mean(individuals, na.rm=TRUE)) %>%
  mutate(hu_group_exclusive = "State")%>%
  mutate(avg_pop_fy21 = round(avg_pop_fy21, 0))

# Add data together
daily_pop_costs_medicaid_match_hu_21 <- rbind(daily_pop_costs_medicaid_match_hu_21, daily_pop_costs_medicaid_match_state_21)
daily_pop_costs_medicaid_match_hu_21 <- daily_pop_costs_medicaid_match_hu_21 %>%
  mutate(cost_pp_per_day = avg_cost_pp_per_day,
         cost_per_year = avg_pop_fy21*365*cost_pp_per_day)



#########
# Total cost for HU's on Medicaid from 2019 to 2021
#########

# cost in 2019 for HU's
cost_2019 <- daily_pop_costs_medicaid_match_hu_19 %>%
  filter(hu_group_exclusive == "Tier 1 HU" |
         hu_group_exclusive == "Tier 2 HU" |
         hu_group_exclusive == "Tier 3 HU") %>%
  summarise(total = sum(cost_per_year))

# cost in 2020 for HU's
cost_2020 <- daily_pop_costs_medicaid_match_hu_20 %>%
  filter(hu_group_exclusive == "Tier 1 HU" |
         hu_group_exclusive == "Tier 2 HU" |
         hu_group_exclusive == "Tier 3 HU") %>%
  summarise(total = sum(cost_per_year))

# cost in 2021 for HU's
cost_2021 <- daily_pop_costs_medicaid_match_hu_21 %>%
  filter(hu_group_exclusive == "Tier 1 HU" |
         hu_group_exclusive == "Tier 2 HU" |
         hu_group_exclusive == "Tier 3 HU") %>%
  summarise(total = sum(cost_per_year))

# IN PRESENTATION
total_cost <- cost_2019$total + cost_2020$total + cost_2021$total

#########
# Save out to external hard drive
#########

write_rds(daily_pop_costs,                      "D:/Analytic/daily_pop_costs.rds")
write_rds(daily_pop_costs_hu,                   "D:/Analytic/daily_pop_costs_hu.rds")
write_rds(daily_pop_costs_medicaid_match_hu_19, "D:/Analytic/daily_pop_costs_medicaid_match_hu_19.rds")
write_rds(daily_pop_costs_medicaid_match_hu_20, "D:/Analytic/daily_pop_costs_medicaid_match_hu_20.rds")
write_rds(daily_pop_costs_medicaid_match_hu_21, "D:/Analytic/daily_pop_costs_medicaid_match_hu_21.rds")
