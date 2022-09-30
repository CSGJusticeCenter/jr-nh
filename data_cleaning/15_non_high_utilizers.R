############################################
# Project: JRI New Hampshire
# File: non_high_utilizers.R
# Last updated: September 29, 2022
# Author: Mari Roberts

# Tables, graphs, and numbers for high utilizers page
# showing characteristics of non-high utilizers
############################################

############################################
# Non-High Utilizers Based on Jail Bookings by State

# Explore non-high utilizers based on top 1%, 3%, and 5% HU's s
#     For each of these definitions, what is the average # of bookings per 3 years, for non-high utilizers?
############################################

############################################################################################################
# Non-High Utilizers: Booking patterns by FY
############################################################################################################

##################
# 1%, 3%, 5% by FY
##################

# calculate the number of bookings by FY, will use for proportions
bookings_fy <- nh_booking %>%
  select(county, booking_id, num_bookings, high_utilizer_1_pct, fy) %>%
  distinct() %>%
  group_by(fy) %>%
  dplyr::summarise(total_bookings = n())

# calculate the average number of bookings per year for HU 1%
nonhu_avg_bookings_1_pct <- fnc_avg_bookings_fy(nh_booking, "high_utilizer_1_pct", "Yes")
nonhu_avg_bookings_1_pct <- nonhu_avg_bookings_1_pct %>% dplyr::rename(avg_num_bookings_1_pct = new_variable_name)

# calculate the number of bookings per year for HU 1%
nonhu_num_bookings_1_pct <- fnc_num_bookings_fy(nh_booking, "high_utilizer_1_pct", "Yes")
nonhu_num_bookings_1_pct <- nonhu_num_bookings_1_pct %>% dplyr::rename(num_bookings_1_pct = new_variable_name)

# calculate the average number of bookings per year for HU 3%
nonhu_avg_bookings_3_pct <- fnc_avg_bookings_fy(nh_booking, "high_utilizer_3_pct", "Yes")
nonhu_avg_bookings_3_pct <- nonhu_avg_bookings_3_pct %>% dplyr::rename(avg_num_bookings_3_pct = new_variable_name)

# calculate the number of bookings per year for HU 3%
nonhu_num_bookings_3_pct <- fnc_num_bookings_fy(nh_booking, "high_utilizer_3_pct", "Yes")
nonhu_num_bookings_3_pct <- nonhu_num_bookings_3_pct %>% dplyr::rename(num_bookings_3_pct = new_variable_name)

# calculate the average number of bookings per year for HU 5%
nonhu_avg_bookings_5_pct <- fnc_avg_bookings_fy(nh_booking, "high_utilizer_5_pct", "Yes")
nonhu_avg_bookings_5_pct <- nonhu_avg_bookings_5_pct %>% dplyr::rename(avg_num_bookings_5_pct = new_variable_name)

# calculate the number of bookings per year for HU 5%
nonhu_num_bookings_5_pct <- fnc_num_bookings_fy(nh_booking, "high_utilizer_5_pct", "Yes")
nonhu_num_bookings_5_pct <- nonhu_num_bookings_5_pct %>% dplyr::rename(num_bookings_5_pct = new_variable_name)

##################
# 1%, 3%, 5% Totals
##################

# calculate the number of bookings total, will use for proportions
bookings_fy_3yr <- nh_booking %>%
  select(county, booking_id, num_bookings, high_utilizer_1_pct, fy) %>%
  distinct() %>%
  group_by() %>%
  summarise(total_bookings = n())

# calculate the average number of bookings for all three years for HU 1%
nonhu_avg_bookings_1_pct_3yr <- fnc_avg_bookings_3yr(nh_booking, "high_utilizer_1_pct", "Yes")
nonhu_avg_bookings_1_pct_3yr <- nonhu_avg_bookings_1_pct_3yr %>% dplyr::rename(avg_num_bookings_1_pct = new_variable_name)

# calculate the number of bookings for all three years for HU 1%
nonhu_num_bookings_1_pct_3yr <- fnc_num_bookings_3yr(nh_booking, "high_utilizer_1_pct", "Yes")
nonhu_num_bookings_1_pct_3yr <- nonhu_num_bookings_1_pct_3yr %>% dplyr::rename(num_bookings_1_pct = new_variable_name)

# calculate the average number of bookings for all three years for HU 3%
nonhu_avg_bookings_3_pct_3yr <- fnc_avg_bookings_3yr(nh_booking, "high_utilizer_3_pct", "Yes")
nonhu_avg_bookings_3_pct_3yr <- nonhu_avg_bookings_3_pct_3yr %>% dplyr::rename(avg_num_bookings_3_pct = new_variable_name)

# calculate the number of bookings for all three years for HU 3%
nonhu_num_bookings_3_pct_3yr <- fnc_num_bookings_3yr(nh_booking, "high_utilizer_3_pct", "Yes")
nonhu_num_bookings_3_pct_3yr <- nonhu_num_bookings_3_pct_3yr %>% dplyr::rename(num_bookings_3_pct = new_variable_name)

# calculate the average number of bookings for all three years for HU 5%
nonhu_avg_bookings_5_pct_3yr <- fnc_avg_bookings_3yr(nh_booking, "high_utilizer_5_pct", "Yes")
nonhu_avg_bookings_5_pct_3yr <- nonhu_avg_bookings_5_pct_3yr %>% dplyr::rename(avg_num_bookings_5_pct = new_variable_name)

# calculate the number of bookings for all three years for HU 5%
nonhu_num_bookings_5_pct_3yr <- fnc_num_bookings_3yr(nh_booking, "high_utilizer_5_pct", "Yes")
nonhu_num_bookings_5_pct_3yr <- nonhu_num_bookings_5_pct_3yr %>% dplyr::rename(num_bookings_5_pct = new_variable_name)

##################
# Combine data
##################

# combine all data together - totals
df_nonhu_bookings_table_totals <- cbind(nonhu_avg_bookings_1_pct_3yr,
                                        nonhu_num_bookings_1_pct_3yr,
                                        nonhu_avg_bookings_3_pct_3yr,
                                        nonhu_num_bookings_3_pct_3yr,
                                        nonhu_avg_bookings_5_pct_3yr,
                                        nonhu_num_bookings_5_pct_3yr,
                                        bookings_fy_3yr
)

# calculate prop of bookings that are non-hu's
df_nonhu_bookings_table_totals <- df_nonhu_bookings_table_totals %>%
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
df_nonhu_bookings_table <-
  nonhu_avg_bookings_1_pct %>%
  left_join(nonhu_num_bookings_1_pct,  by = c("fy")) %>%
  left_join(nonhu_avg_bookings_3_pct,  by = c("fy")) %>%
  left_join(nonhu_num_bookings_3_pct,  by = c("fy")) %>%
  left_join(nonhu_avg_bookings_5_pct,  by = c("fy")) %>%
  left_join(nonhu_num_bookings_5_pct,  by = c("fy")) %>%
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
df_nonhu_bookings_table <- rbind(df_nonhu_bookings_table, df_nonhu_bookings_table_totals)
df_nonhu_bookings_table <- df_nonhu_bookings_table %>%
  mutate(
    avg_num_bookings_1_pct = round(avg_num_bookings_1_pct, 1),
    avg_num_bookings_3_pct = round(avg_num_bookings_3_pct, 1),
    avg_num_bookings_5_pct = round(avg_num_bookings_5_pct, 1))

#######
# create reactable table
#######

nonhu_bookings_table <- reactable(df_nonhu_bookings_table,
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
  summarise(total_bookings = n())

# calculate the average number of bookings per county for HU 1%
nonhu_avg_bookings_1_pct <- fnc_avg_bookings_fy_county(nh_booking, "high_utilizer_1_pct", "Yes")
nonhu_avg_bookings_1_pct <- nonhu_avg_bookings_1_pct %>% dplyr::rename(avg_num_bookings_1_pct = new_variable_name)

# calculate the total number of bookings per county for HU 1%
nonhu_num_bookings_1_pct <- fnc_num_bookings_fy_county(nh_booking, "high_utilizer_1_pct", "Yes")
nonhu_num_bookings_1_pct <- nonhu_num_bookings_1_pct %>% dplyr::rename(num_bookings_1_pct = new_variable_name)

# calculate the average number of bookings per county for HU 3%
nonhu_avg_bookings_3_pct <- fnc_avg_bookings_fy_county(nh_booking, "high_utilizer_3_pct", "Yes")
nonhu_avg_bookings_3_pct <- nonhu_avg_bookings_3_pct %>% dplyr::rename(avg_num_bookings_3_pct = new_variable_name)

# calculate the total number of bookings per county for HU 3%
nonhu_num_bookings_3_pct <- fnc_num_bookings_fy_county(nh_booking, "high_utilizer_3_pct", "Yes")
nonhu_num_bookings_3_pct <- nonhu_num_bookings_3_pct %>% dplyr::rename(num_bookings_3_pct = new_variable_name)

# calculate the average number of bookings per county for HU 5%
nonhu_avg_bookings_5_pct <- fnc_avg_bookings_fy_county(nh_booking, "high_utilizer_5_pct", "Yes")
nonhu_avg_bookings_5_pct <- nonhu_avg_bookings_5_pct %>% dplyr::rename(avg_num_bookings_5_pct = new_variable_name)

# calculate the total number of bookings per county for HU 5%
nonhu_num_bookings_5_pct <- fnc_num_bookings_fy_county(nh_booking, "high_utilizer_5_pct", "Yes")
nonhu_num_bookings_5_pct <- nonhu_num_bookings_5_pct %>% dplyr::rename(num_bookings_5_pct = new_variable_name)

######
# 1%, 3%, 5% Totals
######

# calculate the total number of bookings per county, will use for proportions
bookings_fy_3yr <- nh_booking %>%
  select(county, booking_id, num_bookings, high_utilizer_1_pct, fy) %>%
  distinct() %>%
  group_by(county) %>%
  summarise(total_bookings = n())

# calculate the average number of bookings for all three years for HU 1% by county
nonhu_avg_bookings_1_pct_3yr <- fnc_avg_bookings_3yr_county(nh_booking, "high_utilizer_1_pct", "Yes")
nonhu_avg_bookings_1_pct_3yr <- nonhu_avg_bookings_1_pct_3yr %>% dplyr::rename(avg_num_bookings_1_pct = new_variable_name)

# calculate the number of bookings for all three years for HU 1% by county
nonhu_num_bookings_1_pct_3yr <- fnc_num_bookings_3yr_county(nh_booking, "high_utilizer_1_pct", "Yes")
nonhu_num_bookings_1_pct_3yr <- nonhu_num_bookings_1_pct_3yr %>% dplyr::rename(num_bookings_1_pct = new_variable_name)

# calculate the average number of bookings for all three years for HU 3% by county
nonhu_avg_bookings_3_pct_3yr <- fnc_avg_bookings_3yr_county(nh_booking, "high_utilizer_3_pct", "Yes")
nonhu_avg_bookings_3_pct_3yr <- nonhu_avg_bookings_3_pct_3yr %>% dplyr::rename(avg_num_bookings_3_pct = new_variable_name)

# calculate the number of bookings for all three years for HU 3% by county
nonhu_num_bookings_3_pct_3yr <- fnc_num_bookings_3yr_county(nh_booking, "high_utilizer_3_pct", "Yes")
nonhu_num_bookings_3_pct_3yr <- nonhu_num_bookings_3_pct_3yr %>% dplyr::rename(num_bookings_3_pct = new_variable_name)

# calculate the average number of bookings for all three years for HU 5% by county
nonhu_avg_bookings_5_pct_3yr <- fnc_avg_bookings_3yr_county(nh_booking, "high_utilizer_5_pct", "Yes")
nonhu_avg_bookings_5_pct_3yr <- nonhu_avg_bookings_5_pct_3yr %>% dplyr::rename(avg_num_bookings_5_pct = new_variable_name)

# calculate the number of bookings for all three years for HU 5% by county
nonhu_num_bookings_5_pct_3yr <- fnc_num_bookings_3yr_county(nh_booking, "high_utilizer_5_pct", "Yes")
nonhu_num_bookings_5_pct_3yr <- nonhu_num_bookings_5_pct_3yr %>% dplyr::rename(num_bookings_5_pct = new_variable_name)

######
# combine data together - 1%, 3%, 5% by FY by county
######

df_nonhu_bookings_table_totals <-
  nonhu_avg_bookings_1_pct_3yr %>%
  left_join(nonhu_num_bookings_1_pct_3yr,  by = c("county")) %>%
  left_join(nonhu_avg_bookings_3_pct_3yr,  by = c("county")) %>%
  left_join(nonhu_num_bookings_3_pct_3yr,  by = c("county")) %>%
  left_join(nonhu_avg_bookings_5_pct_3yr,  by = c("county")) %>%
  left_join(nonhu_num_bookings_5_pct_3yr,  by = c("county")) %>%
  left_join(bookings_fy_3yr,  by = c("county"))

# calculate prop of bookings that are HU's for totals
df_nonhu_bookings_table_totals <- df_nonhu_bookings_table_totals %>%
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

# combine data
# calculate prop of bookings that are HU's
df_nonhu_bookings_table <-
  nonhu_avg_bookings_1_pct %>%
  left_join(nonhu_num_bookings_1_pct,  by = c("county", "fy")) %>%
  left_join(nonhu_avg_bookings_3_pct,  by = c("county", "fy")) %>%
  left_join(nonhu_num_bookings_3_pct,  by = c("county", "fy")) %>%
  left_join(nonhu_avg_bookings_5_pct,  by = c("county", "fy")) %>%
  left_join(nonhu_num_bookings_5_pct,  by = c("county", "fy")) %>%
  left_join(bookings_fy, by = c("county", "fy")) %>%
  
  mutate(prop_bookings_1_pct = num_bookings_1_pct/total_bookings,
         prop_bookings_3_pct = num_bookings_3_pct/total_bookings,
         prop_bookings_5_pct = num_bookings_5_pct/total_bookings) %>%
  
  select(county,
         fy,
         num_bookings_1_pct, prop_bookings_1_pct, avg_num_bookings_1_pct,
         num_bookings_3_pct, prop_bookings_3_pct, avg_num_bookings_3_pct,
         num_bookings_5_pct, prop_bookings_5_pct, avg_num_bookings_5_pct,
         total_bookings)

# combine data
# round data
df_nonhu_bookings_table <- rbind(df_nonhu_bookings_table, df_nonhu_bookings_table_totals)
df_nonhu_bookings_table <- df_nonhu_bookings_table %>%
  mutate(
    avg_num_bookings_1_pct = round(avg_num_bookings_1_pct, 1),
    avg_num_bookings_3_pct = round(avg_num_bookings_3_pct, 1),
    avg_num_bookings_5_pct = round(avg_num_bookings_5_pct, 1))

# get totals
df_nonhu_bookings_table_by_county <- df_nonhu_bookings_table %>% filter(fy == "Total") %>% ungroup() %>%  dplyr::select(-fy)

######
# reactable of all 3 years, not separated out by FY but by county
######

nonhu_bookings_table_by_county <- reactable(elementId = "nonhu_bookings_table_by_county",
                                            df_nonhu_bookings_table_by_county,
                                            pagination = FALSE,
                                            theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
                                            defaultColDef = reactable::colDef(
                                              format = colFormat(separators = TRUE), align = "left",
                                              footer = function(values, name) {
                                                if (name %in% c("num_bookings_1_pct", "num_bookings_3_pct", "num_bookings_5_pct", "total_bookings")) {
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
                                            columnGroups = list(
                                              colGroup(name = "Top 1%", columns = c("num_bookings_1_pct", "prop_bookings_1_pct", "avg_num_bookings_1_pct")),
                                              colGroup(name = "Top 3%", columns = c("num_bookings_3_pct", "prop_bookings_3_pct", "avg_num_bookings_3_pct")),
                                              colGroup(name = "Top 5%", columns = c("num_bookings_5_pct", "prop_bookings_5_pct", "avg_num_bookings_5_pct"))
                                            ),
                                            columns = list(
                                              county                 = colDef(footer = "Total", minWidth = 150, name = "County", style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),
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

save(nonhu_bookings_table,           file=paste0(sp_data_path, "/Data/r_data/nonhu_bookings_table.Rda",           sep = ""))
save(nonhu_bookings_table_by_county, file=paste0(sp_data_path, "/Data/r_data/nonhu_bookings_table_by_county.Rda", sep = ""))
