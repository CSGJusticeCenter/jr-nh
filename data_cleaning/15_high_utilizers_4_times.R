############################################
# Project: JRI New Hampshire
# File:  high_utilizers_4_times.R
# Last updated: October 12, 2022
# Author: Mari Roberts

# High Utilizers Based on Jail Bookings by State
# Explore HU's defined as those booked 4 or more times in a FY

# Tables, graphs, and numbers for high utilizers analysis page
############################################

##########

# Min med mean max df for bookings and entrances of HU's

##########

# df for table
df_hu_4_times_summary <- fnc_hus_descriptive_summary(bookings_entrances, "high_utilizer_4_times", "Yes", "Coos (bookings only)")

# reactable table
table_hu_4_times_summary <- fnc_reactable_descriptive_summary(df_hu_4_times_summary, "HU", "Entrances")
