################################################################################

# LOS

# What was the average length of incarceration in jail?
#
# Statistics: First, break up into PC vs. criminal charge admissions
# Then, calculate the minimum, mean, median, and maximum jails stays
#
# Graphic: show distribution (density plot?), with vertical lines for average
# (we can use median is the data are skewed, or mean if they’re somewhat normally distributed)
#
# I wonder if we could potentially have a global distribution density plot,
# but then have 9 lines showing the average for each county as a visual to compare across counties.
# This is a low priority though.

################################################################################

# count freq of los for non-PC Holds
# remove strafford because they don't have pc data
# keep coos because all the data we were given was non-PC Holds

df_los_no_pc_hold <- booking_no_pc_hold %>%
  select(fy,
         county,
         booking_id,
         los,
         los_category,
         high_utilizer_4_times,
         high_utilizer_1_pct,
         high_utilizer_5_pct,
         high_utilizer_10_pct) %>%
  distinct() %>%
  filter(!is.na(los))
dim(df_los_no_pc_hold); length(unique(df_los_no_pc_hold$booking_id)) # 31769, 31769

# average lOS for non-PC bookings
avg_los_no_pc_hold <- df_los_no_pc_hold %>%
  group_by() %>%
  dplyr::summarize(mean = mean(los, na.rm=TRUE))
avg_los_no_pc_hold <- as.numeric(avg_los_no_pc_hold)

# overall LOS for all non-PC Holds
df_los_no_pc_hold_summary <- fnc_summary(df_los_no_pc_hold, "los")
df_los_no_pc_hold_summary <- df_los_no_pc_hold_summary %>% mutate(type = "All (HU's and non-HU's)") %>% select(type, everything())

##########

# Reactable table for LOS for non-PC holds

##########

# reactable table for LOS summary statistics by HU type
table_los_no_pc_hold_summary <- reactable(df_los_no_pc_hold_summary,
                                          pagination = FALSE,
                                          style = list(fontFamily = "Franklin Gothic Book"),
                                          theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
                                          defaultColDef = reactable::colDef(
                                            format = colFormat(separators = TRUE), align = "left"),
                                          compact = TRUE,
                                          fullWidth = FALSE,
                                          columns = list(
                                            type   = colDef(minWidth = 190, name = "",
                                                            style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),
                                            min    = colDef(minWidth = 90, name = "Minimum"),
                                            median = colDef(minWidth = 90, name = "Median"),
                                            mean   = colDef(minWidth = 90, name = "Mean"),
                                            max    = colDef(minWidth = 90, name = "Maximum")))

# count freq of los for entrances (including Coos bookings and Strafford)
df_los_entrances_with_coos <- bookings_entrances %>%
  select(fy,
         county,
         booking_id,
         los,
         high_utilizer_4_times,
         high_utilizer_1_pct, high_utilizer_5_pct, high_utilizer_10_pct) %>%
  distinct() %>%
  filter(!is.na(los))
dim(df_los_entrances_with_coos); length(unique(df_los_entrances_with_coos$booking_id)) # 51349, 51349 will be less because of NAs in LOS's

# average lOS for non-PC bookings
avg_los_entrances_with_coos <- df_los_entrances_with_coos %>%
  group_by() %>%
  dplyr::summarize(mean = mean(los, na.rm=TRUE))
avg_los_entrances_with_coos <- as.numeric(avg_los_entrances_with_coos)

# overall LOS for all non-PC Holds
df_los_entrances_with_coos_summary <- fnc_summary(df_los_entrances_with_coos, "los")
df_los_entrances_with_coos_summary <- df_los_entrances_with_coos_summary %>% mutate(type = "All (HU's and non-HU's)") %>% select(type, everything())

##########

# Reactable table for LOS for entrances (including Coos bookings and Strafford)

##########

# reactable table for LOS summary statistics by HU type
table_los_entrances_with_coos_summary <- reactable(df_los_entrances_with_coos_summary,
                                                   pagination = FALSE,
                                                   style = list(fontFamily = "Franklin Gothic Book"),
                                                   theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
                                                   defaultColDef = reactable::colDef(
                                                     format = colFormat(separators = TRUE), align = "left"),
                                                   compact = TRUE,
                                                   fullWidth = FALSE,
                                                   columns = list(
                                                     type   = colDef(minWidth = 190, name = "",
                                                                     style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),
                                                     min    = colDef(minWidth = 90, name = "Minimum"),
                                                     median = colDef(minWidth = 90, name = "Median"),
                                                     mean   = colDef(minWidth = 90, name = "Mean"),
                                                     max    = colDef(minWidth = 90, name = "Maximum")))

