############################################
# Project: JRI New Hampshire
# File: demographics.R
# Last updated: September 30, 2022
# Author: Mari Roberts

# Tables, graphs, and numbers for demographics page
############################################

#################
# Age
#################

# age category table
df_nh_age_category <- fnc_variable_table(bookings_entrances_19, bookings_entrances_20, bookings_entrances_21, "race")
df_nh_age_category <- df_nh_age_category %>%
  arrange(variable_name) %>%
  filter(variable_name != "Total") %>%
  select(new_variable_name = variable_name, everything()) %>%
  droplevels()

# create reactable table for age categories by fiscal year
nh_age_category <- fnc_reactable_fy(df_nh_age_category,
                                    metric_label = "test",
                                    reactable_counties = counties,
                                    label_width = 150,
                                    note = "test")
nh_age_category
