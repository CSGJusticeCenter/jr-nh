############################################
# Project: JRI New Hampshire
# File: high_utilizers.R
# Last updated: August 31, 2022
# Author: Mari Roberts

# Tables, graphs, and numbers for high utilizers analysis page
############################################

############################################
# High Utilizers Based on Jail Bookings by State

# Explore Notin Top 1%, 5%, and 10% (i.e. 99th percentile, etc.) of all bookings
#     For each of these definitions, what is the average # of bookings per year and per 3 years?

# We could do a stacked bar plot with % HU vs. non-HU bookings, per year, and an average percentage for the 3 year period

# What percent of the standing jail population did high utilizers account for?
############################################

################################################################################

# Bookings 1%, 5%, 10% by FY

################################################################################

# calculate the number of bookings by FY, will use for proportions
bookings_fy <- nh_booking_no_pc_hold %>%
  select(county, booking_id, num_bookings, fy) %>%
  distinct() %>%
  group_by(fy) %>%
  dplyr::summarise(total_bookings = n())

# calculate the average number of bookings per year for HU 1%
non_hu_avg_bookings_1_pct <- fnc_avg_bookings_fy(nh_booking_no_pc_hold, "high_utilizer_1_pct", "No")
non_hu_avg_bookings_1_pct <- non_hu_avg_bookings_1_pct  %>% dplyr::rename(avg_num_bookings_1_pct = new_variable_name)

# calculate the number of bookings per year for HU 1%
non_hu_num_bookings_1_pct <- fnc_num_bookings_fy(nh_booking_no_pc_hold, "high_utilizer_1_pct", "No")
non_hu_num_bookings_1_pct <- non_hu_num_bookings_1_pct  %>% dplyr::rename(num_bookings_1_pct = new_variable_name)

# calculate the average number of bookings per year for HU 5%
non_hu_avg_bookings_5_pct <- fnc_avg_bookings_fy(nh_booking_no_pc_hold, "high_utilizer_5_pct", "No")
non_hu_avg_bookings_5_pct <- non_hu_avg_bookings_5_pct  %>% dplyr::rename(avg_num_bookings_5_pct = new_variable_name)

# calculate the number of bookings per year for HU 5%
non_hu_num_bookings_5_pct <- fnc_num_bookings_fy(nh_booking_no_pc_hold, "high_utilizer_5_pct", "No")
non_hu_num_bookings_5_pct <- non_hu_num_bookings_5_pct  %>% dplyr::rename(num_bookings_5_pct = new_variable_name)

# calculate the average number of bookings per year for HU 10%
non_hu_avg_bookings_10_pct <- fnc_avg_bookings_fy(nh_booking_no_pc_hold, "high_utilizer_10_pct", "No")
non_hu_avg_bookings_10_pct <- non_hu_avg_bookings_10_pct  %>% dplyr::rename(avg_num_bookings_10_pct = new_variable_name)

# calculate the number of bookings per year for HU 10%
non_hu_num_bookings_10_pct <- fnc_num_bookings_fy(nh_booking_no_pc_hold, "high_utilizer_10_pct", "No")
non_hu_num_bookings_10_pct <- non_hu_num_bookings_10_pct  %>% dplyr::rename(num_bookings_10_pct = new_variable_name)

##################
# 1%, 5%, 10% Totals
##################

# calculate the number of bookings total, will use for proportions
bookings_fy_3yr <- nh_booking_no_pc_hold %>%
  select(county, booking_id, num_bookings, fy) %>%
  distinct() %>%
  group_by() %>%
  dplyr::summarise(total_bookings = n())

# calculate the average number of bookings for all three years for HU 1%
non_hu_avg_bookings_1_pct_3yr <- fnc_avg_bookings_3yr(nh_booking_no_pc_hold, "high_utilizer_1_pct", "No")
non_hu_avg_bookings_1_pct_3yr <- non_hu_avg_bookings_1_pct_3yr  %>% dplyr::rename(avg_num_bookings_1_pct = new_variable_name)

# calculate the number of bookings for all three years for HU 1%
non_hu_num_bookings_1_pct_3yr <- fnc_num_bookings_3yr(nh_booking_no_pc_hold, "high_utilizer_1_pct", "No")
non_hu_num_bookings_1_pct_3yr <- non_hu_num_bookings_1_pct_3yr  %>% dplyr::rename(num_bookings_1_pct = new_variable_name)

# calculate the average number of bookings for all three years for HU 5%
non_hu_avg_bookings_5_pct_3yr <- fnc_avg_bookings_3yr(nh_booking_no_pc_hold, "high_utilizer_5_pct", "No")
non_hu_avg_bookings_5_pct_3yr <- non_hu_avg_bookings_5_pct_3yr  %>% dplyr::rename(avg_num_bookings_5_pct = new_variable_name)

# calculate the number of bookings for all three years for HU 5%
non_hu_num_bookings_5_pct_3yr <- fnc_num_bookings_3yr(nh_booking_no_pc_hold, "high_utilizer_5_pct", "No")
non_hu_num_bookings_5_pct_3yr <- non_hu_num_bookings_5_pct_3yr  %>% dplyr::rename(num_bookings_5_pct = new_variable_name)

# calculate the average number of bookings for all three years for HU 10%
non_hu_avg_bookings_10_pct_3yr <- fnc_avg_bookings_3yr(nh_booking_no_pc_hold, "high_utilizer_10_pct", "No")
non_hu_avg_bookings_10_pct_3yr <- non_hu_avg_bookings_10_pct_3yr  %>% dplyr::rename(avg_num_bookings_10_pct = new_variable_name)

# calculate the number of bookings for all three years for HU 10%
non_hu_num_bookings_10_pct_3yr <- fnc_num_bookings_3yr(nh_booking_no_pc_hold, "high_utilizer_10_pct", "No")
non_hu_num_bookings_10_pct_3yr <- non_hu_num_bookings_10_pct_3yr  %>% dplyr::rename(num_bookings_10_pct = new_variable_name)

##################
# Combine data
##################

# combine all data together - totals
df_non_hu_bookings_totals <- cbind(non_hu_avg_bookings_1_pct_3yr,
                               non_hu_num_bookings_1_pct_3yr,
                               non_hu_avg_bookings_5_pct_3yr,
                               non_hu_num_bookings_5_pct_3yr,
                               non_hu_avg_bookings_10_pct_3yr,
                               non_hu_num_bookings_10_pct_3yr,
                               bookings_fy_3yr
)

df_non_hu_bookings_totals <- df_non_hu_bookings_totals %>%
  mutate(fy = "Total",
         prop_bookings_1_pct = num_bookings_1_pct/total_bookings,
         prop_bookings_5_pct = num_bookings_5_pct/total_bookings,
         prop_bookings_10_pct = num_bookings_10_pct/total_bookings) %>%
  select(fy,
         num_bookings_1_pct, prop_bookings_1_pct, avg_num_bookings_1_pct,
         num_bookings_5_pct, prop_bookings_5_pct, avg_num_bookings_5_pct,
         num_bookings_10_pct, prop_bookings_10_pct, avg_num_bookings_10_pct,
         total_bookings)

# combine all data together - 1%, 5%, 10%
# get proportions out of total bookings
# rearrange data
df_non_hu_bookings_fy <-
  non_hu_avg_bookings_1_pct %>%
  left_join(non_hu_num_bookings_1_pct,  by = c("fy")) %>%
  left_join(non_hu_avg_bookings_5_pct,  by = c("fy")) %>%
  left_join(non_hu_num_bookings_5_pct,  by = c("fy")) %>%
  left_join(non_hu_avg_bookings_10_pct,  by = c("fy")) %>%
  left_join(non_hu_num_bookings_10_pct,  by = c("fy")) %>%
  left_join(bookings_fy, by = c("fy")) %>%

  mutate(prop_bookings_1_pct = num_bookings_1_pct/total_bookings,
         prop_bookings_5_pct = num_bookings_5_pct/total_bookings,
         prop_bookings_10_pct = num_bookings_10_pct/total_bookings) %>%

  select(fy,
         num_bookings_1_pct, prop_bookings_1_pct, avg_num_bookings_1_pct,
         num_bookings_5_pct, prop_bookings_5_pct, avg_num_bookings_5_pct,
         num_bookings_10_pct, prop_bookings_10_pct, avg_num_bookings_10_pct,
         total_bookings)

# combine tables
# round values
df_non_hu_bookings_fy <- rbind(df_non_hu_bookings_fy, df_non_hu_bookings_totals)
df_non_hu_bookings_fy <- df_non_hu_bookings_fy %>%
  mutate(
    avg_num_bookings_1_pct = round(avg_num_bookings_1_pct, 1),
    avg_num_bookings_5_pct = round(avg_num_bookings_5_pct, 1),
    avg_num_bookings_10_pct = round(avg_num_bookings_10_pct, 1))

# get totals
df_non_hu_bookings_total_row <- df_non_hu_bookings_fy %>% filter(fy == "Total") %>% select(county = fy, everything())

#######
# create highcharter of HU bookings over time
#######

df_hc_non_hu_bookings_month_year <- nh_booking_no_pc_hold %>%
  select(fy, county, booking_id, month_year, month_year_text,
         high_utilizer_1_pct, high_utilizer_5_pct, high_utilizer_10_pct) %>%
  distinct()
df_hc_non_hu_bookings_month_year <- gather(df_hc_non_hu_bookings_month_year, hu, type, high_utilizer_1_pct:high_utilizer_10_pct, factor_key=TRUE)
df_hc_non_hu_bookings_month_year <- df_hc_non_hu_bookings_month_year %>%
  group_by(month_year, month_year_text,
           hu, type) %>%
  summarise(total = n()) %>%
  filter(type == "No") %>%
  mutate(hu = case_when(hu == "high_utilizer_1_pct" ~ "Not in Top 1%",
                        hu == "high_utilizer_5_pct" ~ "Not in Top 5%",
                        hu == "high_utilizer_10_pct" ~ "Not in Top 10%")) %>%
  mutate(hu = factor(hu, levels = c("Not in Top 1%",  "Not in Top 5%",  "Not in Top 10%")))

hc_non_hu_bookings_month_year <-
  hchart(df_hc_non_hu_bookings_month_year, "line", hcaes(x = month_year_text, y = total, group = (hu))) %>%
  hc_setup() %>%
  hc_xAxis(
    #categories = list("Not in Top 1%", "Not in Top 5%", "Not in Top 10%"),
    title = list(text = "Month and Year", style = list(color =  "#000000", fontWeight = "bold")),
    plotLines = list(list(label = list(text = "COVID-19 Start"), color = "gray", width = 1, value = 20, zIndex = 1))
  ) %>%
  hc_yAxis(
    title = list(text = "Number of Bookings", style = list(color =  "#000000", fontWeight = "bold"))
  ) %>%
  hc_add_theme(hc_theme_jc)

#######
# create percent bar charts of HU bookings over time
#######

# data
df_non_hu_bookings_fy_pct <- nh_booking_no_pc_hold %>%
  select(fy, county, booking_id,
         high_utilizer_1_pct, high_utilizer_5_pct, high_utilizer_10_pct) %>%
  distinct()
df_non_hu_bookings_fy_pct <- gather(df_non_hu_bookings_fy_pct, hu, type, high_utilizer_1_pct:high_utilizer_10_pct, factor_key=TRUE)
df_non_hu_bookings_fy_pct <- df_non_hu_bookings_fy_pct %>%
  group_by(fy, hu, type) %>%
  summarise(total = n()) %>%
  mutate(hu = case_when(hu == "high_utilizer_1_pct" ~ "Not in Top 1%",
                        hu == "high_utilizer_5_pct" ~ "Not in Top 5%",
                        hu == "high_utilizer_10_pct" ~ "Not in Top 10%"))

# Not in Top 1% proportion of bookings that are HU's by FY
temp <- df_non_hu_bookings_fy_pct %>% filter(hu == "Not in Top 1%")
gg_non_hu_bookings_fy_1_pct <- fnc_hu_pct_grouped_bar_chart(temp, "gray", jri_light_blue)

# Not in Top 5% proportion of bookings that are HU's by FY
temp <- df_non_hu_bookings_fy_pct %>% filter(hu == "Not in Top 5%")
gg_non_hu_bookings_fy_5_pct <- fnc_hu_pct_grouped_bar_chart(temp, "gray", jri_green)

# Not in Top 10% proportion of bookings that are HU's by FY
temp <- df_non_hu_bookings_fy_pct %>% filter(hu == "Not in Top 10%")
gg_non_hu_bookings_fy_10_pct <- fnc_hu_pct_grouped_bar_chart(temp, "gray", jri_orange)

#######
# create reactable table of number of HU bookings, % HU bookings, and avg bookings/yr by FY
#######

# reactable table
table_non_hu_bookings_fy <- reactable(df_non_hu_bookings_fy,
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
                                    colGroup(name = "Not in Top 1%", columns = c("num_bookings_1_pct", "prop_bookings_1_pct", "avg_num_bookings_1_pct")),
                                    colGroup(name = "Not in Top 5%", columns = c("num_bookings_5_pct", "prop_bookings_5_pct", "avg_num_bookings_5_pct")),
                                    colGroup(name = "Not in Top 10%", columns = c("num_bookings_10_pct", "prop_bookings_10_pct", "avg_num_bookings_10_pct"))
                                  ),
                                  columns = list(
                                    fy                     = colDef(minWidth = 80, name = "FY", style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),
                                    num_bookings_1_pct     = colDef(minWidth = 80, name = "#"),
                                    prop_bookings_1_pct    = colDef(minWidth = 80, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                    avg_num_bookings_1_pct = colDef(minWidth = 80, name = "Avg/Yr", format = colFormat(percent = FALSE, digits = 1), style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                    num_bookings_5_pct     = colDef(minWidth = 80, name = "#"),
                                    prop_bookings_5_pct    = colDef(minWidth = 80, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                    avg_num_bookings_5_pct = colDef(minWidth = 80, name = "Avg/Yr", format = colFormat(percent = FALSE, digits = 1), style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                    num_bookings_10_pct     = colDef(minWidth = 80, name = "#"),
                                    prop_bookings_10_pct    = colDef(minWidth = 80, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                    avg_num_bookings_10_pct = colDef(minWidth = 80, name = "Avg/Yr", format = colFormat(percent = FALSE, digits = 1), style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),

                                    total_bookings         = colDef(minWidth = 80, name = "Total")))


################################################################################

# High Utilizers: Booking patterns by fiscal year by county

################################################################################

######
# 1%, 5%, 10% by FY and county
######

# calculate the total number of bookings per FY per county, will use for proportions
bookings_fy <- nh_booking_no_pc_hold %>%
  select(county, booking_id, num_bookings, high_utilizer_1_pct, fy) %>%
  distinct() %>%
  group_by(fy, county) %>%
  dplyr::summarise(total_bookings = n())

# calculate the average number of bookings per county for HU 1%
non_hu_avg_bookings_1_pct <- fnc_avg_bookings_fy_county(nh_booking_no_pc_hold, "high_utilizer_1_pct", "No")
non_hu_avg_bookings_1_pct <- non_hu_avg_bookings_1_pct %>% dplyr::rename(avg_num_bookings_1_pct = new_variable_name)

# calculate the total number of bookings per county for HU 1%
non_hu_num_bookings_1_pct <- fnc_num_bookings_fy_county(nh_booking_no_pc_hold, "high_utilizer_1_pct", "No")
non_hu_num_bookings_1_pct <- non_hu_num_bookings_1_pct %>% dplyr::rename(num_bookings_1_pct = new_variable_name)

# calculate the average number of bookings per county for HU 5%
non_hu_avg_bookings_5_pct <- fnc_avg_bookings_fy_county(nh_booking_no_pc_hold, "high_utilizer_5_pct", "No")
non_hu_avg_bookings_5_pct <- non_hu_avg_bookings_5_pct  %>% dplyr::rename(avg_num_bookings_5_pct = new_variable_name)

# calculate the total number of bookings per county for HU 5%
non_hu_num_bookings_5_pct <- fnc_num_bookings_fy_county(nh_booking_no_pc_hold, "high_utilizer_5_pct", "No")
non_hu_num_bookings_5_pct <- non_hu_num_bookings_5_pct  %>% dplyr::rename(num_bookings_5_pct = new_variable_name)

# calculate the average number of bookings per county for HU 10%
non_hu_avg_bookings_10_pct <- fnc_avg_bookings_fy_county(nh_booking_no_pc_hold, "high_utilizer_10_pct", "No")
non_hu_avg_bookings_10_pct <- non_hu_avg_bookings_10_pct  %>% dplyr::rename(avg_num_bookings_10_pct = new_variable_name)

# calculate the total number of bookings per county for HU 10%
non_hu_num_bookings_10_pct <- fnc_num_bookings_fy_county(nh_booking_no_pc_hold, "high_utilizer_10_pct", "No")
non_hu_num_bookings_10_pct <- non_hu_num_bookings_10_pct  %>% dplyr::rename(num_bookings_10_pct = new_variable_name)

######
# 1%, 5%, 10% Totals
######

# calculate the total number of bookings per county, will use for proportions
bookings_fy_3yr <- nh_booking_no_pc_hold %>%
  select(county, booking_id, num_bookings, high_utilizer_1_pct, fy) %>%
  distinct() %>%
  group_by(county) %>%
  dplyr::summarise(total_bookings = n())

# calculate the average number of bookings for all three years for HU 1% by county
non_hu_avg_bookings_1_pct_3yr <- fnc_avg_bookings_3yr_county(nh_booking_no_pc_hold, "high_utilizer_1_pct", "No")
non_hu_avg_bookings_1_pct_3yr <- non_hu_avg_bookings_1_pct_3yr  %>% dplyr::rename(avg_num_bookings_1_pct = new_variable_name)

# calculate the number of bookings for all three years for HU 1% by county
non_hu_num_bookings_1_pct_3yr <- fnc_num_bookings_3yr_county(nh_booking_no_pc_hold, "high_utilizer_1_pct", "No")
non_hu_num_bookings_1_pct_3yr <- non_hu_num_bookings_1_pct_3yr  %>% dplyr::rename(num_bookings_1_pct = new_variable_name)

# calculate the average number of bookings for all three years for HU 5% by county
non_hu_avg_bookings_5_pct_3yr <- fnc_avg_bookings_3yr_county(nh_booking_no_pc_hold, "high_utilizer_5_pct", "No")
non_hu_avg_bookings_5_pct_3yr <- non_hu_avg_bookings_5_pct_3yr  %>% dplyr::rename(avg_num_bookings_5_pct = new_variable_name)

# calculate the number of bookings for all three years for HU 5% by county
non_hu_num_bookings_5_pct_3yr <- fnc_num_bookings_3yr_county(nh_booking_no_pc_hold, "high_utilizer_5_pct", "No")
non_hu_num_bookings_5_pct_3yr <- non_hu_num_bookings_5_pct_3yr  %>% dplyr::rename(num_bookings_5_pct = new_variable_name)

# calculate the average number of bookings for all three years for HU 10% by county
non_hu_avg_bookings_10_pct_3yr <- fnc_avg_bookings_3yr_county(nh_booking_no_pc_hold, "high_utilizer_10_pct", "No")
non_hu_avg_bookings_10_pct_3yr <- non_hu_avg_bookings_10_pct_3yr  %>% dplyr::rename(avg_num_bookings_10_pct = new_variable_name)

# calculate the number of bookings for all three years for HU 10% by county
non_hu_num_bookings_10_pct_3yr <- fnc_num_bookings_3yr_county(nh_booking_no_pc_hold, "high_utilizer_10_pct", "No")
non_hu_num_bookings_10_pct_3yr <- non_hu_num_bookings_10_pct_3yr  %>% dplyr::rename(num_bookings_10_pct = new_variable_name)

######
# combine data together - 1%, 5%, 10% by FY by county
######

df_non_hu_bookings_totals <-
  non_hu_avg_bookings_1_pct_3yr %>%
  left_join(non_hu_num_bookings_1_pct_3yr,  by = c("county")) %>%
  left_join(non_hu_avg_bookings_5_pct_3yr,  by = c("county")) %>%
  left_join(non_hu_num_bookings_5_pct_3yr,  by = c("county")) %>%
  left_join(non_hu_avg_bookings_10_pct_3yr,  by = c("county")) %>%
  left_join(non_hu_num_bookings_10_pct_3yr,  by = c("county")) %>%
  left_join(bookings_fy_3yr,  by = c("county"))

# calculate prop of bookings that are HU's for totals
df_non_hu_bookings_totals <- df_non_hu_bookings_totals %>%
  mutate(fy = "Total",
         prop_bookings_1_pct = num_bookings_1_pct/total_bookings,
         prop_bookings_5_pct = num_bookings_5_pct/total_bookings,
         prop_bookings_10_pct = num_bookings_10_pct/total_bookings) %>%
  select(county,
         #fy,
         num_bookings_1_pct, prop_bookings_1_pct, avg_num_bookings_1_pct,
         num_bookings_5_pct, prop_bookings_5_pct, avg_num_bookings_5_pct,
         num_bookings_10_pct, prop_bookings_10_pct, avg_num_bookings_10_pct,
         total_bookings)

# combine data
# calculate prop of bookings that are HU's
df_non_hu_bookings_fy_county <-
  non_hu_avg_bookings_1_pct %>%
  left_join(non_hu_num_bookings_1_pct,  by = c("county", "fy")) %>%
  left_join(non_hu_avg_bookings_5_pct,  by = c("county", "fy")) %>%
  left_join(non_hu_num_bookings_5_pct,  by = c("county", "fy")) %>%
  left_join(non_hu_avg_bookings_10_pct,  by = c("county", "fy")) %>%
  left_join(non_hu_num_bookings_10_pct,  by = c("county", "fy")) %>%
  left_join(bookings_fy, by = c("county", "fy")) %>%

  mutate(prop_bookings_1_pct = num_bookings_1_pct/total_bookings,
         prop_bookings_5_pct = num_bookings_5_pct/total_bookings,
         prop_bookings_10_pct = num_bookings_10_pct/total_bookings) %>%

  dplyr::select(county,
                fy,
                num_bookings_1_pct, prop_bookings_1_pct, avg_num_bookings_1_pct,
                num_bookings_5_pct, prop_bookings_5_pct, avg_num_bookings_5_pct,
                num_bookings_10_pct, prop_bookings_10_pct, avg_num_bookings_10_pct,
                total_bookings)

# combine data
# round data
df_non_hu_bookings_fy_county <- rbind(df_non_hu_bookings_fy_county, df_non_hu_bookings_totals)
df_non_hu_bookings_fy_county <- df_non_hu_bookings_fy_county %>%
  mutate(
    avg_num_bookings_1_pct = round(avg_num_bookings_1_pct, 1),
    avg_num_bookings_5_pct = round(avg_num_bookings_5_pct, 1),
    avg_num_bookings_10_pct = round(avg_num_bookings_10_pct, 1))

df_non_hu_bookings_totals <- rbind(df_non_hu_bookings_totals, df_non_hu_bookings_total_row)
df_non_hu_bookings_totals <- df_non_hu_bookings_totals %>% select(county, total_bookings, everything())

######
# reactable of all 3 years, not separated out by FY but by county
######

table_non_hu_bookings_county <- reactable(df_non_hu_bookings_totals,
                                      pagination = FALSE,
                                      style = list(fontFamily = "Franklin Gothic Book"),
                                      theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
                                      defaultColDef = reactable::colDef(
                                        format = colFormat(separators = TRUE), align = "center"
                                      ),
                                      rowStyle = function(index) {
                                        if (index %in% c(9)) {
                                          list(`border-top` = "thin solid",
                                               fontWeight = "bold")
                                        }
                                      },
                                      compact = TRUE,
                                      fullWidth = FALSE,
                                      columnGroups = list(
                                        colGroup(name = "Total Bookings", columns = c("total_bookings")),
                                        colGroup(name = "Not in Top 1%", columns = c("num_bookings_1_pct", "prop_bookings_1_pct", "avg_num_bookings_1_pct")),
                                        colGroup(name = "Not in Top 5%", columns = c("num_bookings_5_pct", "prop_bookings_5_pct", "avg_num_bookings_5_pct")),
                                        colGroup(name = "Not in Top 10%", columns = c("num_bookings_10_pct", "prop_bookings_10_pct", "avg_num_bookings_10_pct"))
                                      ),
                                      columns = list(
                                        county                 = colDef(minWidth = 130, align = "left", name = "County", style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),
                                        #county                 = colDef(minWidth = 150, name = "County", style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                        total_bookings         = colDef(minWidth = 130, name = "FY 2019-2021", style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                        num_bookings_1_pct     = colDef(minWidth = 75, name = "#"),
                                        prop_bookings_1_pct    = colDef(minWidth = 75, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                        avg_num_bookings_1_pct = colDef(minWidth = 75, name = "Avg/Yr", format = colFormat(percent = FALSE, digits = 1), style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                        num_bookings_5_pct     = colDef(minWidth = 75, name = "#"),
                                        prop_bookings_5_pct    = colDef(minWidth = 75, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                        avg_num_bookings_5_pct = colDef(minWidth = 75, name = "Avg/Yr", format = colFormat(percent = FALSE, digits = 1), style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                        num_bookings_10_pct     = colDef(minWidth = 75, name = "#"),
                                        prop_bookings_10_pct    = colDef(minWidth = 75, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                        avg_num_bookings_10_pct = colDef(minWidth = 75, name = "Avg/Yr", format = colFormat(percent = FALSE, digits = 1),
                                                                         #style = list(position = "sticky", borderRight = "1px solid #d3d3d3")
                                        )
                                      ))

################################################################################

# Proportion of HU bookings that are PC holds

################################################################################

# select variables
df_non_hu_pc_holds <- nh_booking %>%
  filter(county != "Coos" & county != "Strafford") %>%
  select(county, fy, booking_id, high_utilizer_1_pct, high_utilizer_5_pct, high_utilizer_10_pct, pc_hold_in_booking) %>%
  distinct() %>%
  filter(!is.na(pc_hold_in_booking))

# get bookings of high utilizers
df_non_hu_pc_holds_fy_1_pct <- df_non_hu_pc_holds %>% filter(high_utilizer_1_pct == "No") %>% group_by(fy, pc_hold_in_booking) %>% summarise(total = n())
df_non_hu_pc_holds_fy_5_pct <- df_non_hu_pc_holds %>% filter(high_utilizer_5_pct == "No") %>% group_by(fy, pc_hold_in_booking) %>% summarise(total = n())
df_non_hu_pc_holds_fy_10_pct <- df_non_hu_pc_holds %>% filter(high_utilizer_10_pct == "No") %>% group_by(fy, pc_hold_in_booking) %>% summarise(total = n())

# gg plots of proportion of bookings that are PC holds by FY
gg_non_hu_pc_holds_fy_1_pct <- fnc_pct_grouped_bar_chart(df_non_hu_pc_holds_fy_1_pct, "gray", jri_red)
gg_non_hu_pc_holds_fy_5_pct <- fnc_pct_grouped_bar_chart(df_non_hu_pc_holds_fy_5_pct, "gray", jri_red)
gg_non_hu_pc_holds_fy_10_pct <- fnc_pct_grouped_bar_chart(df_non_hu_pc_holds_fy_10_pct, "gray", jri_red)

# get bookings of high utilizers
df_non_hu_pc_holds_1_pct <- df_non_hu_pc_holds %>% filter(high_utilizer_1_pct == "No") %>%
  group_by(pc_hold_in_booking) %>% summarise(total = n()) %>%
  mutate(hu = "Not in Top 1%")
df_non_hu_pc_holds_1_pct <- group_by(df_non_hu_pc_holds_1_pct) %>% mutate(pct = round(total/sum(total)*100, 1))
df_non_hu_pc_holds_1_pct <- as.data.frame(df_non_hu_pc_holds_1_pct)

df_non_hu_pc_holds_5_pct <- df_non_hu_pc_holds %>% filter(high_utilizer_5_pct == "No") %>%
  group_by(pc_hold_in_booking) %>% summarise(total = n()) %>%
  mutate(hu = "Not in Top 5%")
df_non_hu_pc_holds_5_pct <- group_by(df_non_hu_pc_holds_5_pct) %>% mutate(pct = round(total/sum(total)*100, 1))
df_non_hu_pc_holds_5_pct <- as.data.frame(df_non_hu_pc_holds_5_pct)

df_non_hu_pc_holds_10_pct <- df_non_hu_pc_holds %>% filter(high_utilizer_10_pct == "No") %>%
  group_by(pc_hold_in_booking) %>% summarise(total = n()) %>%
  mutate(hu = "Not in Top 10%")
df_non_hu_pc_holds_10_pct <- group_by(df_non_hu_pc_holds_10_pct) %>% mutate(pct = round(total/sum(total)*100, 1))
df_non_hu_pc_holds_10_pct <- as.data.frame(df_non_hu_pc_holds_10_pct)

df_non_hu_pc_holds_1510_pct <- rbind(df_non_hu_pc_holds_1_pct, df_non_hu_pc_holds_5_pct, df_non_hu_pc_holds_10_pct)

# gg plots of proportion of bookings that are PC holds by FY
temp <- df_non_hu_pc_holds_1510_pct %>% mutate(pct = comma(pct, digits = 1)) %>% mutate(pct = paste0(pct, "%"))
temp$hu <- factor(temp$hu, levels=c("Not in Top 1%", "Not in Top 5%", "Not in Top 10%"))
gg_non_hu_pc_holds_1510_pct <- ggplot(temp, aes(x = hu, y = total, fill = pc_hold_in_booking)) +
  geom_col(colour = NA, position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("gray",jri_red), labels = c("Non-PC      ","PC")) +
  geom_text(aes(label = pct, fontface = 'bold'), position = position_fill(vjust = 0.5),
            size = 7.5, family = "Franklin Gothic Book",
            color = ifelse(temp$pc_hold_in_booking == "Non-PC Hold", "black", "white")) +
  theme_axes +
  theme(legend.position = "top",
        legend.justification = c(0, 0),
        legend.title=element_blank(),
        axis.title.y = element_blank())

##########
# reactable tables with Not in Top 1%, 5%, 10% details - data in high_utilizers.R
##########

temp <- df_non_hu_bookings_fy
temp <- temp %>% mutate(fy = ifelse(fy == "Total", "FY2019-FY2021", fy)) %>% filter(fy == "FY2019-FY2021") %>%
  select(fy, total_bookings, everything())
table_non_hu_bookings_total <- reactable(temp,
                                     pagination = FALSE,
                                     style = list(fontFamily = "Franklin Gothic Book"),
                                     theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
                                     defaultColDef = reactable::colDef(
                                       format = colFormat(separators = TRUE), align = "center"),
                                     compact = TRUE,
                                     fullWidth = FALSE,
                                     rowStyle = function(index) {
                                       if (index %in% c(4)) {
                                         list(`border-top` = "thin solid",
                                              fontWeight = "bold")
                                       }
                                     },
                                     columnGroups = list(
                                       colGroup(name = "Total Bookings (including non-HU's)", columns = c("total_bookings")),

                                       colGroup(name = "Not in Top 1%", columns = c("num_bookings_1_pct", "prop_bookings_1_pct", "avg_num_bookings_1_pct")),
                                       colGroup(name = "Not in Top 5%", columns = c("num_bookings_5_pct", "prop_bookings_5_pct", "avg_num_bookings_5_pct")),
                                       colGroup(name = "Not in Top 10%", columns = c("num_bookings_10_pct", "prop_bookings_10_pct", "avg_num_bookings_10_pct"))
                                     ),
                                     columns = list(
                                       total_bookings         = colDef(minWidth = 170, name = "#",style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                       fy                     = colDef(#minWidth = 150, name = "", style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")
                                         show = F),
                                       num_bookings_1_pct     = colDef(minWidth = 80, name = "#"),
                                       prop_bookings_1_pct    = colDef(minWidth = 80, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                       avg_num_bookings_1_pct = colDef(minWidth = 80, name = "Avg/Yr", format = colFormat(percent = FALSE, digits = 1), style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                       num_bookings_5_pct     = colDef(minWidth = 80, name = "#"),
                                       prop_bookings_5_pct    = colDef(minWidth = 80, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                       avg_num_bookings_5_pct = colDef(minWidth = 80, name = "Avg/Yr", format = colFormat(percent = FALSE, digits = 1), style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                       num_bookings_10_pct     = colDef(minWidth = 80, name = "#"),
                                       prop_bookings_10_pct    = colDef(minWidth = 80, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                       avg_num_bookings_10_pct = colDef(minWidth = 80, name = "Avg/Yr", format = colFormat(percent = FALSE, digits = 1))
                                     ))

temp <- df_non_hu_bookings_totals %>% select(-c(num_bookings_1_pct, num_bookings_5_pct, num_bookings_10_pct, total_bookings))
table_non_hu_booking_summary_county <- reactable(temp,
                                             pagination = FALSE,
                                             style = list(fontFamily = "Franklin Gothic Book"),
                                             theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
                                             defaultColDef = reactable::colDef(
                                               format = colFormat(separators = TRUE), align = "left"),
                                             rowStyle = function(index) {
                                               if (index %in% c(10)) {
                                                 list(`border-top` = "thin solid",
                                                      fontWeight = "bold")
                                               }
                                             },
                                             compact = TRUE,
                                             fullWidth = FALSE,
                                             columnGroups = list(
                                               colGroup(name = "Not in Top 1%", columns = c("prop_bookings_1_pct", "avg_num_bookings_1_pct")),
                                               colGroup(name = "Not in Top 5%", columns = c("prop_bookings_5_pct", "avg_num_bookings_5_pct")),
                                               colGroup(name = "Not in Top 10%", columns = c("prop_bookings_10_pct", "avg_num_bookings_10_pct"))
                                             ),
                                             columns = list(
                                               county                 = colDef(minWidth = 150, name = "County", style = list(fontWeight = "bold")),
                                               prop_bookings_1_pct    = colDef(minWidth = 80, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                               avg_num_bookings_1_pct = colDef(minWidth = 80, name = "Avg/Yr", format = colFormat(percent = FALSE, digits = 1), style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                               prop_bookings_5_pct    = colDef(minWidth = 80, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                               avg_num_bookings_5_pct = colDef(minWidth = 80, name = "Avg/Yr", format = colFormat(percent = FALSE, digits = 1), style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                               prop_bookings_10_pct    = colDef(minWidth = 80, name = "%", format = colFormat(percent = TRUE, digits = 1)),
                                               avg_num_bookings_10_pct = colDef(minWidth = 80, name = "Avg/Yr", format = colFormat(percent = FALSE, digits = 1))))

################################################################################

# Reactable table showing LOS by HU type

################################################################################

# overall LOS for all non-PC hold bookings for 1% HU's
temp <- df_los %>% filter(high_utilizer_1_pct == "No") %>% select(los)
df_los_summary_1_pct <- fnc_los_summary(temp)
df_los_summary_1_pct <- df_los_summary_1_pct %>% mutate(hu = "Not in Top 1%") %>% select(hu, everything())

# overall LOS for all non-PC hold bookings for 5% HU's
temp <- df_los %>% filter(high_utilizer_5_pct == "No") %>% select(los)
df_los_summary_5_pct <- fnc_los_summary(temp)
df_los_summary_5_pct <- df_los_summary_5_pct %>% mutate(hu = "Not in Top 5%") %>% select(hu, everything())

# overall LOS for all non-PC hold bookings for 10% HU's
temp <- df_los %>% filter(high_utilizer_10_pct == "No") %>% select(los)
df_los_summary_10_pct <- fnc_los_summary(temp)
df_los_summary_10_pct <- df_los_summary_10_pct %>% mutate(hu = "Not in Top 10%") %>% select(hu, everything())

# add data together
df_los_summary_1510_pct <- rbind(df_los_summary_1_pct, df_los_summary_5_pct, df_los_summary_10_pct)

# reactable table for LOS summary statistics by HU type
table_los_summary_1510_pct <- reactable(df_los_summary_1510_pct,
                                        pagination = FALSE,
                                        style = list(fontFamily = "Franklin Gothic Book"),
                                        theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
                                        defaultColDef = reactable::colDef(
                                          format = colFormat(separators = TRUE), align = "left"),
                                        compact = TRUE,
                                        fullWidth = FALSE,
                                        columns = list(
                                          hu     = colDef(minWidth = 190, name = "HU",
                                                          style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),
                                          min    = colDef(minWidth = 90, name = "Minimum"),
                                          median = colDef(minWidth = 90, name = "Median"),
                                          mean   = colDef(minWidth = 90, name = "Mean"),
                                          max    = colDef(minWidth = 90, name = "Maximum")))

##########
# ggplot showing histogram of LOS
##########

df_los_1_pct <- df_los %>%
  filter(high_utilizer_1_pct == "No") %>% group_by(los_category) %>% summarise(total = n()) %>%
  mutate(hu = "Not in Top 1%")
df_los_5_pct <- df_los %>%
  filter(high_utilizer_5_pct == "No") %>% group_by(los_category) %>% summarise(total = n()) %>%
  mutate(hu = "Not in Top 5%")
df_los_10_pct <- df_los %>%
  filter(high_utilizer_10_pct == "No") %>% group_by(los_category) %>% summarise(total = n()) %>%
  mutate(hu = "Not in Top 10%")
df_los_pct <- rbind(df_los_1_pct, df_los_5_pct, df_los_10_pct)

gg_los_category_by_hu <- ggplot(df_los_pct, aes(los_category, total)) +
  geom_bar(aes(fill = hu), position = "dodge", stat="identity", width = 0.75) +
  scale_fill_manual(values=c(jri_light_blue, jri_green, jri_orange),
                    labels = c("Not in Top 1%      ","Not in Top 5%      ", "Not in Top 10%")) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  xlab("Length of Stay (Days)") + ylab("Count") +
  theme_minimal(base_family = "Franklin Gothic Book") +
  theme(
    axis.text.x = element_text(size = 18, color = "black", angle = 45, hjust = 0.75),
    axis.text.y = element_text(size = 18, color = "black"),
    axis.title.x = element_text(size = 18, color = "black"),
    axis.title.y = element_text(size = 18, color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "top",
    legend.justification = c(0, 0),
    legend.title=element_blank(),
    legend.text = element_text(family = "Franklin Gothic Book", size = 18, color = "black")
  )

# get % of bookings that are 0-10 days
pct_los_between_0_10_days <- df_los %>% group_by(los_category) %>%
  filter(high_utilizer_10_pct == "No") %>%
  summarise(total = n()) %>%
  mutate(pct = round(total/sum(total)*100, 1)) %>%
  mutate(los_category = as.character(los_category))
pct_los_between_0_10_days = pct_los_between_0_10_days[-c(6:10),]
pct_los_between_0_10_days <- sum(pct_los_between_0_10_days$pct)

# get % of bookings that are 0-1 days
pct_los_between_0_1_days <- df_los %>% group_by(los_category) %>%
  filter(high_utilizer_10_pct == "No") %>%
  summarise(total = n()) %>%
  mutate(pct = round(total/sum(total)*100, 1)) %>%
  mutate(los_category = as.character(los_category))
pct_los_between_0_1_days = pct_los_between_0_1_days[-c(3:10),]
pct_los_between_0_1_days <- sum(pct_los_between_0_1_days$pct)

################################################################################

# What is interesting about people cycling through for 0-1 day?

################################################################################

##########
# Booking types for 1% by FY
##########

df_one_day_booking_types_1_pct <- nh_booking %>%
  filter(los == 1 | los == 0) %>%
  filter(county != "Strafford") %>%
  filter(pc_hold_in_booking == "Non-PC Hold") %>%
  filter(high_utilizer_1_pct == "No") %>%
  filter(!is.na(los)) %>%
  select(county, fy, booking_id, booking_type_standard, los) %>%
  distinct()

# custom functions to find the number of booking types by fiscal year
df_19 <- df_one_day_booking_types_1_pct %>% filter(fy == 2019)
df_20 <- df_one_day_booking_types_1_pct %>% filter(fy == 2020)
df_21 <- df_one_day_booking_types_1_pct %>% filter(fy == 2021)
df_one_day_booking_types_1_pct <- fnc_variable_table(df_19, df_20, df_21, "booking_type_standard")
df_one_day_booking_types_1_pct <- fnc_variable_table_desc(df_one_day_booking_types_1_pct)
df_one_day_booking_types_1_pct <- df_one_day_booking_types_1_pct %>% filter(variable_name != "Total") %>%
  select(booking_type_standard = variable_name, everything())

##########
# Booking types for 5% by FY
##########

df_one_day_booking_types_5_pct <- nh_booking %>%
  filter(los == 1 | los == 0) %>%
  filter(county != "Strafford") %>%
  filter(pc_hold_in_booking == "Non-PC Hold") %>%
  filter(high_utilizer_5_pct == "No") %>%
  filter(!is.na(los)) %>%
  select(county, fy, booking_id, booking_type_standard, los) %>%
  distinct()

# custom functions to find the number of booking types by fiscal year
df_19 <- df_one_day_booking_types_5_pct %>% filter(fy == 2019)
df_20 <- df_one_day_booking_types_5_pct %>% filter(fy == 2020)
df_21 <- df_one_day_booking_types_5_pct %>% filter(fy == 2021)
df_one_day_booking_types_5_pct <- fnc_variable_table(df_19, df_20, df_21, "booking_type_standard")
df_one_day_booking_types_5_pct <- fnc_variable_table_desc(df_one_day_booking_types_5_pct)
df_one_day_booking_types_5_pct <- df_one_day_booking_types_5_pct %>% filter(variable_name != "Total") %>%
  select(booking_type_standard = variable_name, everything())

##########
# Booking types for 10% by FY
##########

df_one_day_booking_types_10_pct <- nh_booking %>%
  filter(los == 1 | los == 0) %>%
  filter(county != "Strafford") %>%
  filter(pc_hold_in_booking == "Non-PC Hold") %>%
  filter(high_utilizer_10_pct == "No") %>%
  filter(!is.na(los)) %>%
  select(county, fy, booking_id, booking_type_standard, los) %>%
  distinct()

# custom functions to find the number of booking types by fiscal year
df_19 <- df_one_day_booking_types_10_pct %>% filter(fy == 2019)
df_20 <- df_one_day_booking_types_10_pct %>% filter(fy == 2020)
df_21 <- df_one_day_booking_types_10_pct %>% filter(fy == 2021)
df_one_day_booking_types_10_pct <- fnc_variable_table(df_19, df_20, df_21, "booking_type_standard")
df_one_day_booking_types_10_pct <- fnc_variable_table_desc(df_one_day_booking_types_10_pct)
df_one_day_booking_types_10_pct <- df_one_day_booking_types_10_pct %>% filter(variable_name != "Total") %>%
  select(booking_type_standard = variable_name, everything())

##########
# Booking types for 1%, 5%, 10% for FY2019-FY2021
##########

temp_1_pct <- df_one_day_booking_types_1_pct %>% select(booking_type_standard, total_1_pct = total, freq_1_pct = freq)
temp_5_pct <- df_one_day_booking_types_5_pct %>% select(booking_type_standard, total_5_pct = total, freq_5_pct = freq)
temp_10_pct <- df_one_day_booking_types_10_pct %>% select(booking_type_standard, total_10_pct = total, freq_10_pct = freq)
df_one_day_booking_types_1510_pct <- temp_1_pct %>%
  left_join(temp_5_pct, by = "booking_type_standard") %>%
  left_join(temp_10_pct, by = "booking_type_standard")

# reactable table
table_one_day_booking_types_1510_pct <-
  reactable(df_one_day_booking_types_1510_pct,
            pagination = FALSE,
            theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
            defaultColDef = reactable::colDef(
              format = colFormat(separators = TRUE), align = "center",
              footer = function(values, name) {
                if (name %in% c("total_1_pct", "total_5_pct", "total_10_pct")) {
                  htmltools::div(paste0("", formatC(
                    x = sum(values),
                    digits = 0,
                    big.mark = ",",
                    format = "f"
                  )))
                }
              },
              footerStyle = list(fontWeight = "bold")
            ),
            compact = TRUE,
            fullWidth = FALSE,
            rowStyle = function(index) {
              if (index %in% c(16)) {
                list(`border-top` = "thin solid",
                     fontWeight = "bold")
              }
            },
            columnGroups = list(
              colGroup(name = "Not in Top 1%", columns = c("total_1_pct", "freq_1_pct")),
              colGroup(name = "Not in Top 5%", columns = c("total_5_pct", "freq_5_pct")),
              colGroup(name = "Not in Top 10%", columns = c("total_10_pct", "freq_10_pct"))
            ),
            columns = list(
              booking_type_standard = colDef(footer = "Total", name = "Booking Type", align = "left", minWidth = 275),
              total_1_pct           = colDef(minWidth = 80, name = "#"),
              freq_1_pct            = colDef(minWidth = 80, name = "%", format = colFormat(percent = TRUE, digits = 1)),

              total_5_pct           = colDef(minWidth = 80, name = "#"),
              freq_5_pct            = colDef(minWidth = 80, name = "%", format = colFormat(percent = TRUE, digits = 1)),

              total_10_pct           = colDef(minWidth = 80, name = "#"),
              freq_10_pct            = colDef(minWidth = 80, name = "%", format = colFormat(percent = TRUE, digits = 1))
            ))

############################################################################################################
# Save to SP
############################################################################################################

# HU overview
save(table_non_hu_bookings_fy,            file=paste0(sp_data_path, "/Data/r_data/table_non_hu_bookings_fy.Rda",               sep = ""))
save(table_non_hu_bookings_county,        file=paste0(sp_data_path, "/Data/r_data/table_non_hu_bookings_county.Rda",     sep = ""))
save(hc_non_hu_bookings_month_year,       file=paste0(sp_data_path, "/Data/r_data/hc_hc_non_hu_bookings_month_year.Rda",          sep = ""))
save(table_non_hu_bookings_total,         file=paste0(sp_data_path, "/Data/r_data/table_non_hu_bookings_total.Rda",         sep = ""))
save(table_non_hu_booking_summary_county, file=paste0(sp_data_path, "/Data/r_data/table_non_hu_booking_summary_county.Rda", sep = ""))

# Prop bookings
save(gg_non_hu_bookings_fy_1_pct,         file=paste0(sp_data_path, "/Data/r_data/gg_non_hu_bookings_fy_1_pct.Rda",            sep = ""))
save(gg_non_hu_bookings_fy_5_pct,         file=paste0(sp_data_path, "/Data/r_data/gg_non_hu_bookings_fy_5_pct.Rda",            sep = ""))
save(gg_non_hu_bookings_fy_10_pct,        file=paste0(sp_data_path, "/Data/r_data/gg_non_hu_bookings_fy_10_pct.Rda",            sep = ""))

# PC holds
save(gg_non_hu_pc_holds_fy_1_pct,         file=paste0(sp_data_path, "/Data/r_data/gg_non_hu_pc_holds_fy_1_pct.Rda",         sep = ""))
save(gg_non_hu_pc_holds_fy_5_pct,         file=paste0(sp_data_path, "/Data/r_data/gg_non_hu_pc_holds_fy_5_pct.Rda",         sep = ""))
save(gg_non_hu_pc_holds_fy_10_pct,        file=paste0(sp_data_path, "/Data/r_data/gg_non_hu_pc_holds_fy_10_pct.Rda",         sep = ""))
save(gg_non_hu_pc_holds_1510_pct,         file=paste0(sp_data_path, "/Data/r_data/gg_non_hu_pc_holds_1510_pct.Rda",          sep = ""))

# LOS
save(table_los_summary_1510_pct,      file=paste0(sp_data_path, "/Data/r_data/table_los_summary_1510_pct.Rda",             sep = ""))
save(gg_los_category_by_hu,           file=paste0(sp_data_path, "/Data/r_data/gg_los_category_by_hu.Rda",           sep = ""))
save(table_one_day_booking_types_1510_pct, file=paste0(sp_data_path, "/Data/r_data/table_one_day_booking_types_1510_pct.Rda",   sep = ""))
save(pct_los_between_0_10_days,       file=paste0(sp_data_path, "/Data/r_data/pct_los_between_0_10_days.Rda",       sep = ""))
save(pct_los_between_0_1_days,        file=paste0(sp_data_path, "/Data/r_data/pct_los_between_0_1_days.Rda",        sep = ""))
