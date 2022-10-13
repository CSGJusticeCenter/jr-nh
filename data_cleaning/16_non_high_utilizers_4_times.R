############################################
# Project: JRI New Hampshire
# File:  non_high_utilizers_4_times.R
# Last updated: October 12, 2022
# Author: Mari Roberts

# Tables, graphs, and numbers for high utilizers analysis page
############################################

##########

# Min med mean max df for bookings and entrances of non-HU's

##########

# df for table
df_non_hu_4_times_summary <- fnc_hus_descriptive_summary(bookings_entrances, "high_utilizer_4_times", "No", "Coos (bookings only)")

# reactable table
table_non_hu_4_times_summary <- fnc_reactable_descriptive_summary(df_non_hu_4_times_summary, "HU", "Entrances")
