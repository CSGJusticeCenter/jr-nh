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
#     For each of these definitions, what is the average # of bookings per year and per 3 years?

# We could do a stacked bar plot with % HU vs. non-HU bookings, per year, and an average percentage for the 3 year period

# What percent of the standing jail population did high utilizers account for?
############################################

############################################################################################################
# High Utilizers: Booking patterns by FY
############################################################################################################

##################
# 1%, 3%, 5% by FY
##################

# calculate the number of bookings by FY, will use for proportions
bookings_fy <- nh_booking %>%
  select(county, booking_id, num_bookings, fy) %>%
  distinct() %>%
  group_by(fy) %>%
  dplyr::summarise(total_bookings = n())

# calculate the average number of bookings per year for HU 1%
non_hu_avg_bookings_1_pct <- fnc_avg_bookings_fy(nh_booking, "high_utilizer_1_pct", "No")
non_hu_avg_bookings_1_pct <- non_hu_avg_bookings_1_pct  %>% dplyr::rename(avg_num_bookings_1_pct = new_variable_name)

# calculate the number of bookings per year for HU 1%
non_hu_num_bookings_1_pct <- fnc_num_bookings_fy(nh_booking, "high_utilizer_1_pct", "No")
non_hu_num_bookings_1_pct <- non_hu_num_bookings_1_pct  %>% dplyr::rename(num_bookings_1_pct = new_variable_name)

# calculate the average number of bookings per year for HU 3%
non_hu_avg_bookings_3_pct <- fnc_avg_bookings_fy(nh_booking, "high_utilizer_3_pct", "No")
non_hu_avg_bookings_3_pct <- non_hu_avg_bookings_3_pct  %>% dplyr::rename(avg_num_bookings_3_pct = new_variable_name)

# calculate the number of bookings per year for HU 3%
non_hu_num_bookings_3_pct <- fnc_num_bookings_fy(nh_booking, "high_utilizer_3_pct", "No")
non_hu_num_bookings_3_pct <- non_hu_num_bookings_3_pct  %>% dplyr::rename(num_bookings_3_pct = new_variable_name)

# calculate the average number of bookings per year for HU 5%
non_hu_avg_bookings_5_pct <- fnc_avg_bookings_fy(nh_booking, "high_utilizer_5_pct", "No")
non_hu_avg_bookings_5_pct <- non_hu_avg_bookings_5_pct  %>% dplyr::rename(avg_num_bookings_5_pct = new_variable_name)

# calculate the number of bookings per year for HU 5%
non_hu_num_bookings_5_pct <- fnc_num_bookings_fy(nh_booking, "high_utilizer_5_pct", "No")
non_hu_num_bookings_5_pct <- non_hu_num_bookings_5_pct  %>% dplyr::rename(num_bookings_5_pct = new_variable_name)

##################
# 1%, 3%, 5% Totals
##################

# calculate the number of bookings total, will use for proportions
bookings_fy_3yr <- nh_booking %>%
  select(county, booking_id, num_bookings, fy) %>%
  distinct() %>%
  group_by() %>%
  dplyr::summarise(total_bookings = n())

# calculate the average number of bookings for all three years for HU 1%
non_hu_avg_bookings_1_pct_3yr <- fnc_avg_bookings_3yr(nh_booking, "high_utilizer_1_pct", "No")
non_hu_avg_bookings_1_pct_3yr <- non_hu_avg_bookings_1_pct_3yr  %>% dplyr::rename(avg_num_bookings_1_pct = new_variable_name)

# calculate the number of bookings for all three years for HU 1%
non_hu_num_bookings_1_pct_3yr <- fnc_num_bookings_3yr(nh_booking, "high_utilizer_1_pct", "No")
non_hu_num_bookings_1_pct_3yr <- non_hu_num_bookings_1_pct_3yr  %>% dplyr::rename(num_bookings_1_pct = new_variable_name)

# calculate the average number of bookings for all three years for HU 3%
non_hu_avg_bookings_3_pct_3yr <- fnc_avg_bookings_3yr(nh_booking, "high_utilizer_3_pct", "No")
non_hu_avg_bookings_3_pct_3yr <- non_hu_avg_bookings_3_pct_3yr  %>% dplyr::rename(avg_num_bookings_3_pct = new_variable_name)

# calculate the number of bookings for all three years for HU 3%
non_hu_num_bookings_3_pct_3yr <- fnc_num_bookings_3yr(nh_booking, "high_utilizer_3_pct", "No")
non_hu_num_bookings_3_pct_3yr <- non_hu_num_bookings_3_pct_3yr  %>% dplyr::rename(num_bookings_3_pct = new_variable_name)

# calculate the average number of bookings for all three years for HU 5%
non_hu_avg_bookings_5_pct_3yr <- fnc_avg_bookings_3yr(nh_booking, "high_utilizer_5_pct", "No")
non_hu_avg_bookings_5_pct_3yr <- non_hu_avg_bookings_5_pct_3yr  %>% dplyr::rename(avg_num_bookings_5_pct = new_variable_name)

# calculate the number of bookings for all three years for HU 5%
non_hu_num_bookings_5_pct_3yr <- fnc_num_bookings_3yr(nh_booking, "high_utilizer_5_pct", "No")
non_hu_num_bookings_5_pct_3yr <- non_hu_num_bookings_5_pct_3yr  %>% dplyr::rename(num_bookings_5_pct = new_variable_name)

##################
# Combine data
##################

# combine all data together - totals
df_non_hu_bookings_table_totals <- cbind(non_hu_avg_bookings_1_pct_3yr,
                                     non_hu_num_bookings_1_pct_3yr,
                                     non_hu_avg_bookings_3_pct_3yr,
                                     non_hu_num_bookings_3_pct_3yr,
                                     non_hu_avg_bookings_5_pct_3yr,
                                     non_hu_num_bookings_5_pct_3yr,
                                     bookings_fy_3yr
)

df_non_hu_bookings_table_totals <- df_non_hu_bookings_table_totals %>%
  mutate(fy = "Total",
         prop_bookings_1_pct = num_bookings_1_pct/total_bookings,
         prop_bookings_3_pct = num_bookings_3_pct/total_bookings,
         prop_bookings_5_pct = num_bookings_5_pct/total_bookings) %>%
  select(fy,
         num_bookings_1_pct, prop_bookings_1_pct, avg_num_bookings_1_pct,
         num_bookings_3_pct, prop_bookings_3_pct, avg_num_bookings_3_pct,
         num_bookings_5_pct, prop_bookings_5_pct, avg_num_bookings_5_pct,
         total_bookings)

# combine all data together - 1%, 3%, 5%
# get proportions out of total bookings
# rearrange data
df_non_hu_bookings_table <-
  non_hu_avg_bookings_1_pct %>%
  left_join(non_hu_num_bookings_1_pct,  by = c("fy")) %>%
  left_join(non_hu_avg_bookings_3_pct,  by = c("fy")) %>%
  left_join(non_hu_num_bookings_3_pct,  by = c("fy")) %>%
  left_join(non_hu_avg_bookings_5_pct,  by = c("fy")) %>%
  left_join(non_hu_num_bookings_5_pct,  by = c("fy")) %>%
  left_join(bookings_fy, by = c("fy")) %>%

  mutate(prop_bookings_1_pct = num_bookings_1_pct/total_bookings,
         prop_bookings_3_pct = num_bookings_3_pct/total_bookings,
         prop_bookings_5_pct = num_bookings_5_pct/total_bookings) %>%

  select(fy,
         num_bookings_1_pct, prop_bookings_1_pct, avg_num_bookings_1_pct,
         num_bookings_3_pct, prop_bookings_3_pct, avg_num_bookings_3_pct,
         num_bookings_5_pct, prop_bookings_5_pct, avg_num_bookings_5_pct,
         total_bookings)

# combine tables
# round values
df_non_hu_bookings_table <- rbind(df_non_hu_bookings_table, df_non_hu_bookings_table_totals)
df_non_hu_bookings_table <- df_non_hu_bookings_table %>%
  mutate(
    avg_num_bookings_1_pct = round(avg_num_bookings_1_pct, 1),
    avg_num_bookings_3_pct = round(avg_num_bookings_3_pct, 1),
    avg_num_bookings_5_pct = round(avg_num_bookings_5_pct, 1))

# get totals
df_non_hu_bookings_table_total_row <- df_non_hu_bookings_table %>% filter(fy == "Total") %>% select(county = fy, everything())

#######
# create reactable table
#######

non_hu_bookings_table <- reactable(df_non_hu_bookings_table,
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
                                 avg_num_bookings_1_pct = colDef(minWidth = 80, name = "Avg/Yr", format = colFormat(percent = FALSE, digits = 1), style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                 num_bookings_3_pct     = colDef(minWidth = 80, name = "#"),
                                 prop_bookings_3_pct    = colDef(minWidth = 80, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                 avg_num_bookings_3_pct = colDef(minWidth = 80, name = "Avg/Yr", format = colFormat(percent = FALSE, digits = 1), style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                 num_bookings_5_pct     = colDef(minWidth = 80, name = "#"),
                                 prop_bookings_5_pct    = colDef(minWidth = 80, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                 avg_num_bookings_5_pct = colDef(minWidth = 80, name = "Avg/Yr", format = colFormat(percent = FALSE, digits = 1), style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),

                                 total_bookings         = colDef(minWidth = 80, name = "Total")))


############################################################################################################
# High Utilizers: Booking patterns by fiscal year by county
############################################################################################################

######
# 1%, 3%, 5% by FY and county
######

# calculate the total number of bookings per FY per county, will use for proportions
bookings_fy <- nh_booking %>%
  select(county, booking_id, num_bookings, high_utilizer_1_pct, fy) %>%
  distinct() %>%
  group_by(fy, county) %>%
  dplyr::summarise(total_bookings = n())

# calculate the average number of bookings per county for HU 1%
non_hu_avg_bookings_1_pct <- fnc_avg_bookings_fy_county(nh_booking, "high_utilizer_1_pct", "No")
non_hu_avg_bookings_1_pct <- non_hu_avg_bookings_1_pct %>% dplyr::rename(avg_num_bookings_1_pct = new_variable_name)

# calculate the total number of bookings per county for HU 1%
non_hu_num_bookings_1_pct <- fnc_num_bookings_fy_county(nh_booking, "high_utilizer_1_pct", "No")
non_hu_num_bookings_1_pct <- non_hu_num_bookings_1_pct %>% dplyr::rename(num_bookings_1_pct = new_variable_name)

# calculate the average number of bookings per county for HU 3%
non_hu_avg_bookings_3_pct <- fnc_avg_bookings_fy_county(nh_booking, "high_utilizer_3_pct", "No")
non_hu_avg_bookings_3_pct <- non_hu_avg_bookings_3_pct  %>% dplyr::rename(avg_num_bookings_3_pct = new_variable_name)

# calculate the total number of bookings per county for HU 3%
non_hu_num_bookings_3_pct <- fnc_num_bookings_fy_county(nh_booking, "high_utilizer_3_pct", "No")
non_hu_num_bookings_3_pct <- non_hu_num_bookings_3_pct  %>% dplyr::rename(num_bookings_3_pct = new_variable_name)

# calculate the average number of bookings per county for HU 5%
non_hu_avg_bookings_5_pct <- fnc_avg_bookings_fy_county(nh_booking, "high_utilizer_5_pct", "No")
non_hu_avg_bookings_5_pct <- non_hu_avg_bookings_5_pct  %>% dplyr::rename(avg_num_bookings_5_pct = new_variable_name)

# calculate the total number of bookings per county for HU 5%
non_hu_num_bookings_5_pct <- fnc_num_bookings_fy_county(nh_booking, "high_utilizer_5_pct", "No")
non_hu_num_bookings_5_pct <- non_hu_num_bookings_5_pct  %>% dplyr::rename(num_bookings_5_pct = new_variable_name)

######
# 1%, 3%, 5% Totals
######

# calculate the total number of bookings per county, will use for proportions
bookings_fy_3yr <- nh_booking %>%
  select(county, booking_id, num_bookings, high_utilizer_1_pct, fy) %>%
  distinct() %>%
  group_by(county) %>%
  dplyr::summarise(total_bookings = n())

# calculate the average number of bookings for all three years for HU 1% by county
non_hu_avg_bookings_1_pct_3yr <- fnc_avg_bookings_3yr_county(nh_booking, "high_utilizer_1_pct", "No")
non_hu_avg_bookings_1_pct_3yr <- non_hu_avg_bookings_1_pct_3yr  %>% dplyr::rename(avg_num_bookings_1_pct = new_variable_name)

# calculate the number of bookings for all three years for HU 1% by county
non_hu_num_bookings_1_pct_3yr <- fnc_num_bookings_3yr_county(nh_booking, "high_utilizer_1_pct", "No")
non_hu_num_bookings_1_pct_3yr <- non_hu_num_bookings_1_pct_3yr  %>% dplyr::rename(num_bookings_1_pct = new_variable_name)

# calculate the average number of bookings for all three years for HU 3% by county
non_hu_avg_bookings_3_pct_3yr <- fnc_avg_bookings_3yr_county(nh_booking, "high_utilizer_3_pct", "No")
non_hu_avg_bookings_3_pct_3yr <- non_hu_avg_bookings_3_pct_3yr  %>% dplyr::rename(avg_num_bookings_3_pct = new_variable_name)

# calculate the number of bookings for all three years for HU 3% by county
non_hu_num_bookings_3_pct_3yr <- fnc_num_bookings_3yr_county(nh_booking, "high_utilizer_3_pct", "No")
non_hu_num_bookings_3_pct_3yr <- non_hu_num_bookings_3_pct_3yr  %>% dplyr::rename(num_bookings_3_pct = new_variable_name)

# calculate the average number of bookings for all three years for HU 5% by county
non_hu_avg_bookings_5_pct_3yr <- fnc_avg_bookings_3yr_county(nh_booking, "high_utilizer_5_pct", "No")
non_hu_avg_bookings_5_pct_3yr <- non_hu_avg_bookings_5_pct_3yr  %>% dplyr::rename(avg_num_bookings_5_pct = new_variable_name)

# calculate the number of bookings for all three years for HU 5% by county
non_hu_num_bookings_5_pct_3yr <- fnc_num_bookings_3yr_county(nh_booking, "high_utilizer_5_pct", "No")
non_hu_num_bookings_5_pct_3yr <- non_hu_num_bookings_5_pct_3yr  %>% dplyr::rename(num_bookings_5_pct = new_variable_name)

######
# combine data together - 1%, 3%, 5% by FY by county
######

df_non_hu_bookings_table_totals <-
  non_hu_avg_bookings_1_pct_3yr %>%
  left_join(non_hu_num_bookings_1_pct_3yr,  by = c("county")) %>%
  left_join(non_hu_avg_bookings_3_pct_3yr,  by = c("county")) %>%
  left_join(non_hu_num_bookings_3_pct_3yr,  by = c("county")) %>%
  left_join(non_hu_avg_bookings_5_pct_3yr,  by = c("county")) %>%
  left_join(non_hu_num_bookings_5_pct_3yr,  by = c("county")) %>%
  left_join(bookings_fy_3yr,  by = c("county"))

# calculate prop of bookings that are HU's for totals
df_non_hu_bookings_table_totals <- df_non_hu_bookings_table_totals %>%
  mutate(fy = "Total",
         prop_bookings_1_pct = num_bookings_1_pct/total_bookings,
         prop_bookings_3_pct = num_bookings_3_pct/total_bookings,
         prop_bookings_5_pct = num_bookings_5_pct/total_bookings) %>%
  select(county,
         #fy,
         num_bookings_1_pct, prop_bookings_1_pct, avg_num_bookings_1_pct,
         num_bookings_3_pct, prop_bookings_3_pct, avg_num_bookings_3_pct,
         num_bookings_5_pct, prop_bookings_5_pct, avg_num_bookings_5_pct,
         total_bookings)

# combine data
# calculate prop of bookings that are HU's
df_non_hu_bookings_table_county <-
  non_hu_avg_bookings_1_pct %>%
  left_join(non_hu_num_bookings_1_pct,  by = c("county", "fy")) %>%
  left_join(non_hu_avg_bookings_3_pct,  by = c("county", "fy")) %>%
  left_join(non_hu_num_bookings_3_pct,  by = c("county", "fy")) %>%
  left_join(non_hu_avg_bookings_5_pct,  by = c("county", "fy")) %>%
  left_join(non_hu_num_bookings_5_pct,  by = c("county", "fy")) %>%
  left_join(bookings_fy, by = c("county", "fy")) %>%

  mutate(prop_bookings_1_pct = num_bookings_1_pct/total_bookings,
         prop_bookings_3_pct = num_bookings_3_pct/total_bookings,
         prop_bookings_5_pct = num_bookings_5_pct/total_bookings) %>%

  dplyr::select(county,
                fy,
                num_bookings_1_pct, prop_bookings_1_pct, avg_num_bookings_1_pct,
                num_bookings_3_pct, prop_bookings_3_pct, avg_num_bookings_3_pct,
                num_bookings_5_pct, prop_bookings_5_pct, avg_num_bookings_5_pct,
                total_bookings)

# combine data
# round data
df_non_hu_bookings_table_county <- rbind(df_non_hu_bookings_table_county, df_non_hu_bookings_table_totals)
df_non_hu_bookings_table_county <- df_non_hu_bookings_table_county %>%
  mutate(
    avg_num_bookings_1_pct = round(avg_num_bookings_1_pct, 1),
    avg_num_bookings_3_pct = round(avg_num_bookings_3_pct, 1),
    avg_num_bookings_5_pct = round(avg_num_bookings_5_pct, 1))

df_non_hu_bookings_table_totals <- rbind(df_non_hu_bookings_table_totals, df_non_hu_bookings_table_total_row)

######
# reactable of all 3 years, not separated out by FY but by county
######

non_hu_bookings_table_by_county <- reactable(df_non_hu_bookings_table_totals,
                                         pagination = FALSE,
                                         theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
                                         defaultColDef = reactable::colDef(
                                           format = colFormat(separators = TRUE), align = "left"
                                           # footer = function(values, name) {
                                           #   if (name %in% c("num_bookings_1_pct", "num_bookings_3_pct", "num_bookings_5_pct", "total_bookings")) {
                                           #     htmltools::div(paste0("", formatC(
                                           #       x = sum(values),
                                           #       digits = 0,
                                           #       big.mark = ",",
                                           #       format = "f"
                                           #     )))
                                           #   }
                                           # },
                                           # footerStyle = list(fontWeight = "bold")
                                         ),
                                         rowStyle = function(index) {
                                           if (index %in% c(10)) {
                                             list(`border-top` = "thin solid",
                                                  fontWeight = "bold")
                                           }
                                         },
                                         compact = TRUE,
                                         fullWidth = FALSE,
                                         columnGroups = list(
                                           colGroup(name = "Top 1%", columns = c("num_bookings_1_pct", "prop_bookings_1_pct", "avg_num_bookings_1_pct")),
                                           colGroup(name = "Top 3%", columns = c("num_bookings_3_pct", "prop_bookings_3_pct", "avg_num_bookings_3_pct")),
                                           colGroup(name = "Top 5%", columns = c("num_bookings_5_pct", "prop_bookings_5_pct", "avg_num_bookings_5_pct"))
                                         ),
                                         columns = list(
                                           county                 = colDef(minWidth = 150, name = "County", style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),
                                           #county                 = colDef(minWidth = 150, name = "County", style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                           num_bookings_1_pct     = colDef(minWidth = 75, name = "#"),
                                           prop_bookings_1_pct    = colDef(minWidth = 75, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                           avg_num_bookings_1_pct = colDef(minWidth = 75, name = "Avg/Yr", format = colFormat(percent = FALSE, digits = 1), style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                           num_bookings_3_pct     = colDef(minWidth = 75, name = "#"),
                                           prop_bookings_3_pct    = colDef(minWidth = 75, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                           avg_num_bookings_3_pct = colDef(minWidth = 75, name = "Avg/Yr", format = colFormat(percent = FALSE, digits = 1), style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                           num_bookings_5_pct     = colDef(minWidth = 75, name = "#"),
                                           prop_bookings_5_pct    = colDef(minWidth = 75, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                           avg_num_bookings_5_pct = colDef(minWidth = 75, name = "Avg/Yr", format = colFormat(percent = FALSE, digits = 1), style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),

                                           total_bookings         = colDef(minWidth = 75, name = "Total")))

##################
# Proportion of HU bookings that are PC holds
##################

# select variables
df_non_hu_pc_holds <- nh_booking %>%
  filter(county != "Coos" & county != "Strafford") %>%
  select(county, fy, booking_id, high_utilizer_1_pct, high_utilizer_3_pct, high_utilizer_5_pct, pc_hold_in_booking) %>%
  distinct() %>%
  filter(!is.na(pc_hold_in_booking))

# get bookings of high utilizers
df_non_hu_pc_holds_fy_1_pct <- df_non_hu_pc_holds %>% filter(high_utilizer_1_pct == "Yes") %>% group_by(fy, pc_hold_in_booking) %>% summarise(total = n())
df_non_hu_pc_holds_fy_3_pct <- df_non_hu_pc_holds %>% filter(high_utilizer_3_pct == "Yes") %>% group_by(fy, pc_hold_in_booking) %>% summarise(total = n())
df_non_hu_pc_holds_fy_5_pct <- df_non_hu_pc_holds %>% filter(high_utilizer_5_pct == "Yes") %>% group_by(fy, pc_hold_in_booking) %>% summarise(total = n())

# gg plots of proportion of bookings that are PC holds by FY
non_hu_pc_holds_fy_1_pct_gg <- fnc_pct_grouped_bar_chart(df_non_hu_pc_holds_fy_1_pct, "gray", jri_light_blue)
non_hu_pc_holds_fy_3_pct_gg <- fnc_pct_grouped_bar_chart(df_non_hu_pc_holds_fy_3_pct, "gray", jri_green)
non_hu_pc_holds_fy_5_pct_gg <- fnc_pct_grouped_bar_chart(df_non_hu_pc_holds_fy_5_pct, "gray", jri_orange)

# get bookings of high utilizers
df_non_hu_pc_holds_1_pct <- df_non_hu_pc_holds %>% filter(high_utilizer_1_pct == "Yes") %>%
  group_by(pc_hold_in_booking) %>% summarise(total = n()) %>%
  mutate(hu = "Top 1%")
df_non_hu_pc_holds_1_pct <- group_by(df_non_hu_pc_holds_1_pct) %>% mutate(pct = round(total/sum(total)*100, 1))
df_non_hu_pc_holds_1_pct <- as.data.frame(df_non_hu_pc_holds_1_pct)

df_non_hu_pc_holds_3_pct <- df_non_hu_pc_holds %>% filter(high_utilizer_3_pct == "Yes") %>%
  group_by(pc_hold_in_booking) %>% summarise(total = n()) %>%
  mutate(hu = "Top 3%")
df_non_hu_pc_holds_3_pct <- group_by(df_non_hu_pc_holds_3_pct) %>% mutate(pct = round(total/sum(total)*100, 1))
df_non_hu_pc_holds_3_pct <- as.data.frame(df_non_hu_pc_holds_3_pct)

df_non_hu_pc_holds_5_pct <- df_non_hu_pc_holds %>% filter(high_utilizer_5_pct == "Yes") %>%
  group_by(pc_hold_in_booking) %>% summarise(total = n()) %>%
  mutate(hu = "Top 5%")
df_non_hu_pc_holds_5_pct <- group_by(df_non_hu_pc_holds_5_pct) %>% mutate(pct = round(total/sum(total)*100, 1))
df_non_hu_pc_holds_5_pct <- as.data.frame(df_non_hu_pc_holds_5_pct)

df_non_hu_pc_holds_135_pct <- rbind(df_non_hu_pc_holds_1_pct, df_non_hu_pc_holds_3_pct, df_non_hu_pc_holds_5_pct)

# gg plots of proportion of bookings that are PC holds by FY
temp <- df_non_hu_pc_holds_135_pct %>% mutate(pct = comma(pct, digits = 1)) %>% mutate(pct = paste0(pct, "%"))
non_hu_pc_holds_135_pct_gg <- ggplot(temp, aes(x = hu, y = total, fill = pc_hold_in_booking)) +
  geom_col(colour = NA, position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("gray",jri_red), labels = c("Non-PC      ","PC")) +
  geom_text(aes(label = pct, fontface = 'bold'), position = position_fill(vjust = 0.5),
            size = 7.5, family = "Franklin Gothic Book",
            color = ifelse(temp$pc_hold_in_booking == "Non-PC Hold Booking", "black", "white")) +
  theme_axes +
  theme(legend.position = "top",
        legend.justification = c(0, 0),
        legend.title=element_blank(),
        axis.title.y = element_blank())

##########
# reactable tables with top 1%, 3%, 5% details - data in high_utilizers.R
##########

temp <- df_non_hu_bookings_table %>% select(-total_bookings)
temp <- temp %>% mutate(fy = ifelse(fy == "Total", "FY2019-FY2021", fy)) %>% filter(fy == "FY2019-FY2021")

pres_non_hu_bookings_table <- reactable(temp,
                                        pagination = FALSE,
                                        style = list(fontFamily = "Franklin Gothic Book"),
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
                                          fy                     = colDef(minWidth = 150, name = "", style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),
                                          num_bookings_1_pct     = colDef(minWidth = 80, name = "#"),
                                          prop_bookings_1_pct    = colDef(minWidth = 80, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                          avg_num_bookings_1_pct = colDef(minWidth = 80, name = "Avg/Yr", format = colFormat(percent = FALSE, digits = 1), style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                          num_bookings_3_pct     = colDef(minWidth = 80, name = "#"),
                                          prop_bookings_3_pct    = colDef(minWidth = 80, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                          avg_num_bookings_3_pct = colDef(minWidth = 80, name = "Avg/Yr", format = colFormat(percent = FALSE, digits = 1), style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                          num_bookings_5_pct     = colDef(minWidth = 80, name = "#"),
                                          prop_bookings_5_pct    = colDef(minWidth = 80, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                          avg_num_bookings_5_pct = colDef(minWidth = 80, name = "Avg/Yr", format = colFormat(percent = FALSE, digits = 1), style = list(position = "sticky", borderRight = "1px solid #d3d3d3"))))


############################################################################################################
# Save to SP
############################################################################################################

save(non_hu_bookings_table,           file=paste0(sp_data_path, "/Data/r_data/non_hu_bookings_table.Rda",           sep = ""))
save(non_hu_bookings_table_by_county, file=paste0(sp_data_path, "/Data/r_data/non_hu_bookings_table_by_county.Rda", sep = ""))
save(pres_non_hu_bookings_table,      file=paste0(sp_data_path, "/Data/r_data/pres_non_hu_bookings_table.Rda",      sep = ""))
