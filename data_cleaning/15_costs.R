
############################################
# Project: JRI New Hampshire
# File: costs.R
# Last updated: January 30, 2023
# Author: Mari Roberts

# Calculate the daily average population
# https://stackoverflow.com/questions/52304623/calculate-average-number-of-individuals-present-on-each-date-in-r
# Check for movement between jails
############################################

# Extract county budgets
county_budgets <- county_budgets.xlsx %>%
  clean_names() %>%
  select(county, doc_budget)

# Remove entrances without release dates
# Remove entrances where release date is earlier than start date (one instance)
df1 <- adm_all %>% select(id, county, booking_id, booking_date, release_date) %>%
  distinct() %>%
  mutate(booking_date = as.Date(booking_date, format="%Y-%m-%d"),
         release_date = as.Date(release_date, format="%Y-%m-%d")) %>%
  mutate(lessthanstart = ifelse(release_date < booking_date, TRUE, FALSE)) %>%
  filter(!is.na(release_date)) %>%
  filter(lessthanstart == FALSE)

# Unpack the start_date and end_date to individual dates
# Takes 10-15 minutes to run
# Each date is listed for each person
df2 <- data.frame()
for (i in 1:nrow(df1)){
  expand  <-  data.frame(county = df1$county[i],
                         id = df1$id[i],
                         Dates = seq.Date(df1$booking_date[i], df1$release_date[i], 1))
  df2 <- rbind(df2, expand)
}

# Calculate the number of individuals present in each county in each day
# Filter to dates in 2019
daily_pop_costs_2019 <- df2 %>% group_by(county, Dates) %>%
  dplyr::summarise(individuals = n()) %>%
  filter(Dates > "2018-06-30" & Dates < "2019-07-01") %>%
  group_by(county) %>%
  dplyr::summarise(avg_pop_fy19 = mean(individuals, na.rm=TRUE)) %>%
  left_join(county_budgets, by = "county") %>%
  mutate(cost_pp_per_year = doc_budget/avg_pop_fy19) %>%
  mutate(cost_pp_per_day = cost_pp_per_year/365)

# Calculate the number of individuals present in each county in each day
# Filter to dates in 2020
daily_pop_costs_2020 <- df2 %>% group_by(county, Dates) %>%
  dplyr::summarise(individuals = n()) %>%
  filter(Dates > "2019-06-30" & Dates < "2020-07-01") %>%
  group_by(county) %>%
  dplyr::summarise(avg_pop_fy20 = mean(individuals, na.rm=TRUE)) %>%
  left_join(county_budgets, by = "county") %>%
  mutate(cost_pp_per_year = doc_budget/avg_pop_fy20) %>%
  mutate(cost_pp_per_day = cost_pp_per_year/365)


# Save out to external hard drive
write_rds(daily_pop_costs_2020,
          "D:/Analytic/daily_pop_costs.rds")
