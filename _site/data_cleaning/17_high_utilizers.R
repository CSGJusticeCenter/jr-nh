############################################
# Project: JRI New Hampshire
# File: high_utilizers.R
# Last updated: August 31, 2022
# Author: Mari Roberts

# Tables, graphs, and numbers for high utilizers analysis page
############################################


############################################
# High Utilizers Based on Jail Bookings by State

# Explore top 1%, 3%, and 5% (i.e. 99th percentile, etc.) of all bookings
#     For each of these definitions, what is the average # of bookings per 3 years, for high utilizers, vs. for non-HUs?

# We could do a stacked bar plot with % HU vs. non-HU bookings, per year, and an average percentage for the 3 year period

# What percent of the standing jail population did high utilizers account for?
############################################

#---------------------Number, # average bookings, average LOS, average age------
# High Utilizer 1%
# High Utilizer 3%
# High Utilizer 5%
# Non High Utilizer
# Overall
#-------------------------------------------------------------------------------

#-------------------------Number,         # average bookings,     # prop of pop
#                    2019, 2020, 2020      2019, 2020, 2020       2019, 2020, 2020
# High Utilizer 1%
# High Utilizer 3%
# High Utilizer 5%
# Non High Utilizer
# Overall
#-------------------------------------------------------------------------------

#------------------------1 %---------3 %---------5 %---------Overall
#                     HU  Non-HU  HU  Non-HU   HU  Non-HU
# Number
# Proportion of pop
# Avg Bookings
# Avg LOS

##############
# 1%
##############

temp <-
  mutate(percentile = num_bookings < obs * 0.05)

hu_avg_bookings_1_pct <- nh_booking %>%
  filter(high_utilizer_1_pct == TRUE) %>%
  group_by(fy) %>%
  summarise_at(vars(num_bookings), list(avg_num_bookings_1_pct = mean))

hu_num_bookings_1_pct <- nh_booking %>%
  filter(high_utilizer_1_pct == TRUE) %>%
  group_by(fy) %>%
  summarise(num_bookings_1_pct = n())

hu_avg_bookings_3_pct <- nh_booking %>%
  filter(high_utilizer_3_pct == TRUE) %>%
  group_by(fy) %>%
  summarise_at(vars(num_bookings), list(avg_num_bookings_1_pct = mean))
