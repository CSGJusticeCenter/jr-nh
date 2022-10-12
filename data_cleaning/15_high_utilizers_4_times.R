############################################
# Project: JRI New Hampshire
# File:  high_utilizers_4_times.R
# Last updated: October 12, 2022
# Author: Mari Roberts

# Tables, graphs, and numbers for high utilizers analysis page
############################################

############################################
# High Utilizers Based on Jail Bookings by State

# Explore HU's defined as those booked 4 or more times in a FY
############################################

dim(nh_booking_entrances); length(unique(nh_booking_entrances$booking_id))

temp <- nh_booking_entrances %>%
