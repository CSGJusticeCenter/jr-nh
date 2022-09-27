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

############################################################################################################
# Booking patterns by fiscal year
############################################################################################################

######
# 1% by FY
######

hu_avg_bookings_1_pct <- nh_booking %>%
  filter(high_utilizer_1_pct == TRUE) %>%
  select(county, booking_id, num_bookings, high_utilizer_1_pct, fy) %>%
  distinct() %>%
  group_by(fy) %>%
  summarise_at(vars(num_bookings), list(avg_num_bookings_1_pct = mean))

hu_num_bookings_1_pct <- nh_booking %>%
  select(county, booking_id, num_bookings, high_utilizer_1_pct, fy) %>%
  distinct() %>%
  filter(high_utilizer_1_pct == TRUE) %>%
  group_by(fy) %>%
  summarise(num_bookings_1_pct = n())

hu_bookings_fy <- nh_booking %>%
  select(county, booking_id, num_bookings, high_utilizer_1_pct, fy) %>%
  distinct() %>%
  group_by(fy) %>%
  summarise(total_bookings = n())

######
# 3% by FY
######

hu_avg_bookings_3_pct <- nh_booking %>%
  filter(high_utilizer_3_pct == TRUE) %>%
  group_by(fy) %>%
  summarise_at(vars(num_bookings), list(avg_num_bookings_3_pct = mean))

hu_num_bookings_3_pct <- nh_booking %>%
  filter(high_utilizer_3_pct == TRUE) %>%
  group_by(fy) %>%
  summarise(num_bookings_3_pct = n())

######
# 5% by FY
######

hu_avg_bookings_5_pct <- nh_booking %>%
  filter(high_utilizer_5_pct == TRUE) %>%
  group_by(fy) %>%
  summarise_at(vars(num_bookings), list(avg_num_bookings_5_pct = mean))

hu_num_bookings_5_pct <- nh_booking %>%
  filter(high_utilizer_5_pct == TRUE) %>%
  group_by(fy) %>%
  summarise(num_bookings_5_pct = n())

######
# 1% Totals
######

hu_avg_bookings_1_pct_3yr <- nh_booking %>%
  filter(high_utilizer_1_pct == TRUE) %>%
  select(county, booking_id, num_bookings, high_utilizer_1_pct, fy) %>%
  distinct() %>%
  group_by() %>%
  summarise_at(vars(num_bookings), list(avg_num_bookings_1_pct = mean))

hu_num_bookings_1_pct_3yr <- nh_booking %>%
  select(county, booking_id, num_bookings, high_utilizer_1_pct, fy) %>%
  distinct() %>%
  filter(high_utilizer_1_pct == TRUE) %>%
  group_by() %>%
  summarise(num_bookings_1_pct = n())

hu_bookings_fy_3yr <- nh_booking %>%
  select(county, booking_id, num_bookings, high_utilizer_1_pct, fy) %>%
  distinct() %>%
  group_by() %>%
  summarise(total_bookings = n())

######
# 3% Totals
######

hu_avg_bookings_3_pct_3yr <- nh_booking %>%
  filter(high_utilizer_3_pct == TRUE) %>%
  group_by() %>%
  summarise_at(vars(num_bookings), list(avg_num_bookings_3_pct = mean))

hu_num_bookings_3_pct_3yr <- nh_booking %>%
  filter(high_utilizer_3_pct == TRUE) %>%
  group_by() %>%
  summarise(num_bookings_3_pct = n())

######
# 5% Totals
######

hu_avg_bookings_5_pct_3yr <- nh_booking %>%
  filter(high_utilizer_5_pct == TRUE) %>%
  group_by() %>%
  summarise_at(vars(num_bookings), list(avg_num_bookings_5_pct = mean))

hu_num_bookings_5_pct_3yr <- nh_booking %>%
  filter(high_utilizer_5_pct == TRUE) %>%
  group_by() %>%
  summarise(num_bookings_5_pct = n())

df_hu_bookings_table_totals <- cbind(hu_avg_bookings_1_pct_3yr, hu_num_bookings_1_pct_3yr, hu_avg_bookings_3_pct_3yr, hu_num_bookings_3_pct_3yr, hu_avg_bookings_5_pct_3yr, hu_num_bookings_5_pct_3yr,  hu_bookings_fy_3yr)
df_hu_bookings_table_totals <- df_hu_bookings_table_totals %>%
  mutate(fy = "Total",
         prop_bookings_1_pct = num_bookings_1_pct/total_bookings,
         prop_bookings_3_pct = num_bookings_3_pct/total_bookings,
         prop_bookings_5_pct = num_bookings_5_pct/total_bookings) %>%
  select(fy,
         num_bookings_1_pct, prop_bookings_1_pct, avg_num_bookings_1_pct,
         num_bookings_3_pct, prop_bookings_3_pct, avg_num_bookings_3_pct,
         num_bookings_5_pct, prop_bookings_5_pct, avg_num_bookings_5_pct,
         total_bookings)

df_hu_bookings_table <-
  hu_avg_bookings_1_pct %>%
  left_join(hu_num_bookings_1_pct,  by = c("fy")) %>%
  left_join(hu_avg_bookings_3_pct,  by = c("fy")) %>%
  left_join(hu_num_bookings_3_pct,  by = c("fy")) %>%
  left_join(hu_avg_bookings_5_pct,  by = c("fy")) %>%
  left_join(hu_num_bookings_5_pct,  by = c("fy")) %>%
  left_join(hu_bookings_fy, by = c("fy")) %>%

  mutate(prop_bookings_1_pct = num_bookings_1_pct/total_bookings,
         prop_bookings_3_pct = num_bookings_3_pct/total_bookings,
         prop_bookings_5_pct = num_bookings_5_pct/total_bookings) %>%

  select(fy,
         num_bookings_1_pct, prop_bookings_1_pct, avg_num_bookings_1_pct,
         num_bookings_3_pct, prop_bookings_3_pct, avg_num_bookings_3_pct,
         num_bookings_5_pct, prop_bookings_5_pct, avg_num_bookings_5_pct,
         total_bookings)

df_hu_bookings_table <- rbind(df_hu_bookings_table, df_hu_bookings_table_totals)
df_hu_bookings_table <- df_hu_bookings_table %>%
  mutate(
    avg_num_bookings_1_pct = round(avg_num_bookings_1_pct, 1),
    avg_num_bookings_3_pct = round(avg_num_bookings_3_pct, 1),
    avg_num_bookings_5_pct = round(avg_num_bookings_5_pct, 1))

hu_bookings_table <- reactable(df_hu_bookings_table,
                               pagination = FALSE,
                               theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
                               defaultColDef = reactable::colDef(
                                 format = colFormat(separators = TRUE), align = "left"),
                               compact = TRUE,
                               fullWidth = FALSE,
                               rowStyle = function(index) {
                                 if (index %in% c(4)) {
                                   list(`border-top` = "thin solid",
                                        fontWeight = "bold")
                                 }
                               },
                               columnGroups = list(
                                 colGroup(name = "Top 1%", columns = c("num_bookings_1_pct", "prop_bookings_1_pct", "avg_num_bookings_1_pct")),
                                 colGroup(name = "Top 3%", columns = c("num_bookings_3_pct", "prop_bookings_3_pct", "avg_num_bookings_3_pct")),
                                 colGroup(name = "Top 5%", columns = c("num_bookings_5_pct", "prop_bookings_5_pct", "avg_num_bookings_5_pct"))
                               ),
                               columns = list(
                                 fy                     = colDef(minWidth = 80, name = "FY", style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),
                                 num_bookings_1_pct     = colDef(minWidth = 80, name = "#"),
                                 prop_bookings_1_pct    = colDef(minWidth = 80, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                 avg_num_bookings_1_pct = colDef(minWidth = 80, name = "Avg/Yr", style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                 num_bookings_3_pct     = colDef(minWidth = 80, name = "#"),
                                 prop_bookings_3_pct    = colDef(minWidth = 80, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                 avg_num_bookings_3_pct = colDef(minWidth = 80, name = "Avg/Yr", style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                 num_bookings_5_pct     = colDef(minWidth = 80, name = "#"),
                                 prop_bookings_5_pct    = colDef(minWidth = 80, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                 avg_num_bookings_5_pct = colDef(minWidth = 80, name = "Avg/Yr", style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),

                                 total_bookings         = colDef(minWidth = 80, name = "Total")))


############################################################################################################
# Booking patterns by county
############################################################################################################

######
# 1% by FY
######

hu_avg_bookings_1_pct <- nh_booking %>%
  filter(high_utilizer_1_pct == TRUE) %>%
  select(county, booking_id, num_bookings, high_utilizer_1_pct, fy) %>%
  distinct() %>%
  group_by(fy, county) %>%
  summarise_at(vars(num_bookings), list(avg_num_bookings_1_pct = mean))

hu_num_bookings_1_pct <- nh_booking %>%
  select(county, booking_id, num_bookings, high_utilizer_1_pct, fy) %>%
  distinct() %>%
  filter(high_utilizer_1_pct == TRUE) %>%
  group_by(fy, county) %>%
  summarise(num_bookings_1_pct = n())

hu_bookings_fy <- nh_booking %>%
  select(county, booking_id, num_bookings, high_utilizer_1_pct, fy) %>%
  distinct() %>%
  group_by(fy, county) %>%
  summarise(total_bookings = n())

######
# 3% by FY
######

hu_avg_bookings_3_pct <- nh_booking %>%
  filter(high_utilizer_3_pct == TRUE) %>%
  group_by(fy, county) %>%
  summarise_at(vars(num_bookings), list(avg_num_bookings_3_pct = mean))

hu_num_bookings_3_pct <- nh_booking %>%
  filter(high_utilizer_3_pct == TRUE) %>%
  group_by(fy, county) %>%
  summarise(num_bookings_3_pct = n())

######
# 5% by FY
######

hu_avg_bookings_5_pct <- nh_booking %>%
  filter(high_utilizer_5_pct == TRUE) %>%
  group_by(fy, county) %>%
  summarise_at(vars(num_bookings), list(avg_num_bookings_5_pct = mean))

hu_num_bookings_5_pct <- nh_booking %>%
  filter(high_utilizer_5_pct == TRUE) %>%
  group_by(fy, county) %>%
  summarise(num_bookings_5_pct = n())

######
# 1% Totals
######

hu_avg_bookings_1_pct_3yr <- nh_booking %>%
  filter(high_utilizer_1_pct == TRUE) %>%
  select(county, booking_id, num_bookings, high_utilizer_1_pct, fy) %>%
  distinct() %>%
  group_by(county) %>%
  summarise_at(vars(num_bookings), list(avg_num_bookings_1_pct = mean))

hu_num_bookings_1_pct_3yr <- nh_booking %>%
  select(county, booking_id, num_bookings, high_utilizer_1_pct, fy) %>%
  distinct() %>%
  filter(high_utilizer_1_pct == TRUE) %>%
  group_by(county) %>%
  summarise(num_bookings_1_pct = n())

hu_bookings_fy_3yr <- nh_booking %>%
  select(county, booking_id, num_bookings, high_utilizer_1_pct, fy) %>%
  distinct() %>%
  group_by(county) %>%
  summarise(total_bookings = n())

######
# 3% Totals
######

hu_avg_bookings_3_pct_3yr <- nh_booking %>%
  filter(high_utilizer_3_pct == TRUE) %>%
  group_by(county) %>%
  summarise_at(vars(num_bookings), list(avg_num_bookings_3_pct = mean))

hu_num_bookings_3_pct_3yr <- nh_booking %>%
  filter(high_utilizer_3_pct == TRUE) %>%
  group_by(county) %>%
  summarise(num_bookings_3_pct = n())

######
# 5% Totals
######

hu_avg_bookings_5_pct_3yr <- nh_booking %>%
  filter(high_utilizer_5_pct == TRUE) %>%
  group_by(county) %>%
  summarise_at(vars(num_bookings), list(avg_num_bookings_5_pct = mean))

hu_num_bookings_5_pct_3yr <- nh_booking %>%
  filter(high_utilizer_5_pct == TRUE) %>%
  group_by(county) %>%
  summarise(num_bookings_5_pct = n())


# df_hu_bookings_table_totals <- cbind(hu_avg_bookings_1_pct_3yr, hu_num_bookings_1_pct_3yr,
#                                      hu_avg_bookings_3_pct_3yr, hu_num_bookings_3_pct_3yr,
#                                      hu_avg_bookings_5_pct_3yr, hu_num_bookings_5_pct_3yr,
#                                      hu_bookings_fy_3yr)
df_hu_bookings_table_totals <-
  hu_avg_bookings_1_pct_3yr %>%
  left_join(hu_num_bookings_1_pct_3yr,  by = c("county")) %>%
  left_join(hu_avg_bookings_3_pct_3yr,  by = c("county")) %>%
  left_join(hu_num_bookings_3_pct_3yr,  by = c("county")) %>%
  left_join(hu_avg_bookings_5_pct_3yr,  by = c("county")) %>%
  left_join(hu_num_bookings_5_pct_3yr,  by = c("county")) %>%
  left_join(hu_bookings_fy_3yr,  by = c("county"))

df_hu_bookings_table_totals <- df_hu_bookings_table_totals %>%
  mutate(fy = "Total",
         prop_bookings_1_pct = num_bookings_1_pct/total_bookings,
         prop_bookings_3_pct = num_bookings_3_pct/total_bookings,
         prop_bookings_5_pct = num_bookings_5_pct/total_bookings) %>%
  select(county,
         fy,
         num_bookings_1_pct, prop_bookings_1_pct, avg_num_bookings_1_pct,
         num_bookings_3_pct, prop_bookings_3_pct, avg_num_bookings_3_pct,
         num_bookings_5_pct, prop_bookings_5_pct, avg_num_bookings_5_pct,
         total_bookings)

df_hu_bookings_table <-
  hu_avg_bookings_1_pct %>%
  left_join(hu_num_bookings_1_pct,  by = c("county", "fy")) %>%
  left_join(hu_avg_bookings_3_pct,  by = c("county", "fy")) %>%
  left_join(hu_num_bookings_3_pct,  by = c("county", "fy")) %>%
  left_join(hu_avg_bookings_5_pct,  by = c("county", "fy")) %>%
  left_join(hu_num_bookings_5_pct,  by = c("county", "fy")) %>%
  left_join(hu_bookings_fy, by = c("county", "fy")) %>%

  mutate(prop_bookings_1_pct = num_bookings_1_pct/total_bookings,
         prop_bookings_3_pct = num_bookings_3_pct/total_bookings,
         prop_bookings_5_pct = num_bookings_5_pct/total_bookings) %>%

  select(county,
         fy,
         num_bookings_1_pct, prop_bookings_1_pct, avg_num_bookings_1_pct,
         num_bookings_3_pct, prop_bookings_3_pct, avg_num_bookings_3_pct,
         num_bookings_5_pct, prop_bookings_5_pct, avg_num_bookings_5_pct,
         total_bookings)

df_hu_bookings_table <- rbind(df_hu_bookings_table, df_hu_bookings_table_totals)
df_hu_bookings_table <- df_hu_bookings_table %>%
  mutate(
    avg_num_bookings_1_pct = round(avg_num_bookings_1_pct, 1),
    avg_num_bookings_3_pct = round(avg_num_bookings_3_pct, 1),
    avg_num_bookings_5_pct = round(avg_num_bookings_5_pct, 1))

df_hu_bookings_table_by_county <- df_hu_bookings_table %>% filter(fy == "Total") %>% ungroup() %>%  dplyr::select(-fy)

hu_bookings_table_by_county <- reactable(df_hu_bookings_table_by_county,
                               pagination = FALSE,
                               theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
                               defaultColDef = reactable::colDef(
                                 format = colFormat(separators = TRUE), align = "left"),
                               compact = TRUE,
                               fullWidth = FALSE,
                               # rowStyle = function(index) {
                               #   if (index %in% c(4)) {
                               #     list(`border-top` = "thin solid",
                               #          fontWeight = "bold")
                               #   }
                               # },
                               columnGroups = list(
                                 colGroup(name = "Top 1%", columns = c("num_bookings_1_pct", "prop_bookings_1_pct", "avg_num_bookings_1_pct")),
                                 colGroup(name = "Top 3%", columns = c("num_bookings_3_pct", "prop_bookings_3_pct", "avg_num_bookings_3_pct")),
                                 colGroup(name = "Top 5%", columns = c("num_bookings_5_pct", "prop_bookings_5_pct", "avg_num_bookings_5_pct"))
                               ),
                               columns = list(
                                 county                 = colDef(minWidth = 150, name = "County", style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),
                                 num_bookings_1_pct     = colDef(minWidth = 75, name = "#"),
                                 prop_bookings_1_pct    = colDef(minWidth = 75, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                 avg_num_bookings_1_pct = colDef(minWidth = 75, name = "Avg/Yr", style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                 num_bookings_3_pct     = colDef(minWidth = 75, name = "#"),
                                 prop_bookings_3_pct    = colDef(minWidth = 75, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                 avg_num_bookings_3_pct = colDef(minWidth = 75, name = "Avg/Yr", style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                 num_bookings_5_pct     = colDef(minWidth = 75, name = "#"),
                                 prop_bookings_5_pct    = colDef(minWidth = 75, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                 avg_num_bookings_5_pct = colDef(minWidth = 75, name = "Avg/Yr", style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),

                                 total_bookings         = colDef(minWidth = 75, name = "Total")))


############################################################################################################
# Save to SP
############################################################################################################

save(hu_bookings_table,           file=paste0(sp_data_path, "/Data/r_data/hu_bookings_table.Rda",           sep = ""))
save(hu_bookings_table_by_county, file=paste0(sp_data_path, "/Data/r_data/hu_bookings_table_by_county.Rda", sep = ""))



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
