############################################
# Project: JRI New Hampshire
# File: high_utilizers_1510_pct.R
# Last updated: October 12, 2022
# Author: Mari Roberts

# High Utilizers Based on Jail Bookings by State

# Explore top 1%, 5%, and 10% (i.e. 99th percentile, etc.) of all bookings
# For each of these definitions, what is the average # of bookings per year and per 3 years?

# Tables, graphs, and numbers for high utilizers analysis page
############################################

##########

# Min med mean max df for entrances of 1%, 5%, 10% HU's

##########

# df for tables
df_hu_1_pct_summary  <- fnc_hus_descriptive_summary(bookings_entrances, "high_utilizer_1_pct",  "Yes", "Coos (bookings only)")
df_hu_5_pct_summary  <- fnc_hus_descriptive_summary(bookings_entrances, "high_utilizer_5_pct",  "Yes", "Coos (bookings only)")
df_hu_10_pct_summary <- fnc_hus_descriptive_summary(bookings_entrances, "high_utilizer_10_pct", "Yes", "Coos (bookings only)")

# reactable table
table_hu_1_pct_summary  <- fnc_reactable_descriptive_summary(df_hu_1_pct_summary,  "1% HU",  "Entrances")
table_hu_5_pct_summary  <- fnc_reactable_descriptive_summary(df_hu_5_pct_summary,  "5% HU",  "Entrances")
table_hu_10_pct_summary <- fnc_reactable_descriptive_summary(df_hu_10_pct_summary, "10% HU", "Entrances")

# combine 1%, 5%, and 10% into one df
temp_1_pct <- df_hu_1_pct_summary %>%
  mutate(range = paste(min, max, sep = " - ")) %>%
  select(county,
         total_df,
         total_hu_1_pct = total_hus,
         mean_1_pct = mean,
         range_1_pct = range,
         freq_1_pct = freq)
temp_5_pct <- df_hu_5_pct_summary %>%
  mutate(range = paste(min, max, sep = " - ")) %>%
  select(county,
         #total_df,
         total_hu_5_pct = total_hus,
         mean_5_pct = mean,
         range_5_pct = range,
         freq_5_pct = freq)
temp_10_pct <- df_hu_10_pct_summary %>%
  mutate(range = paste(min, max, sep = " - ")) %>%
  select(county,
         #total_df,
         total_hu_10_pct = total_hus,
         mean_10_pct = mean,
         range_10_pct = range,
         freq_10_pct = freq)
temp_1510_pct <- temp_1_pct %>%
  left_join(temp_5_pct, by = "county") %>%
  left_join(temp_10_pct, by = "county")

# reactable table for entrances
table_1510_pct_entrances_summary <- reactable(temp_1510_pct,
                      pagination = FALSE,
                      style = list(fontFamily = "Franklin Gothic Book"),
                      rowStyle = function(index) {
                        if (index %in% c(10)) {
                          list(`border-top` = "thin solid",
                               fontWeight = "bold")
                        }
                      },
                      columnGroups = list(
                        colGroup(name = "Top 1%",  columns = c("total_hu_1_pct",  "mean_1_pct",  "range_1_pct",  "freq_1_pct")),
                        colGroup(name = "Top 5%",  columns = c("total_hu_5_pct",  "mean_5_pct",  "range_5_pct",  "freq_5_pct")),
                        colGroup(name = "Top 10%", columns = c("total_hu_10_pct", "mean_10_pct", "range_10_pct", "freq_10_pct"))
                      ),
                      theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
                      defaultColDef = reactable::colDef(
                        format = colFormat(separators = TRUE), align = "center"),
                      compact = TRUE,
                      fullWidth = FALSE,
                      columns = list(
                        county         = colDef(minWidth = 160, name = "County", align = "left",
                                                style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),
                        total_df       = colDef(minWidth = 100, name = "# Entrances", show = T),
                        total_hu_1_pct = colDef(minWidth = 100, name = "# Entrances"),
                        mean_1_pct     = colDef(minWidth = 100, name = "Avg # of Entrances /Yr"),
                        range_1_pct    = colDef(minWidth = 100, name = "Range of # of Entrances"),
                        freq_1_pct     = colDef(style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3"), minWidth = 100, name = "% of Entrances that are HU's", format = colFormat(percent = TRUE, digits = 1)),

                        total_hu_5_pct = colDef(minWidth = 100, name = "# Entrances",),
                        mean_5_pct     = colDef(minWidth = 100, name = "Avg # of Entrances /Yr"),
                        range_5_pct    = colDef(minWidth = 100, name = "Range of # of Entrances"),
                        freq_5_pct     = colDef(style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3"), minWidth = 100, name = "% of Entrances that are HU's", format = colFormat(percent = TRUE, digits = 1)),

                        total_hu_10_pct = colDef(minWidth = 100, name = "# Entrances"),
                        mean_10_pct     = colDef(minWidth = 100, name = "Avg # of Entrances /Yr"),
                        range_10_pct    = colDef(minWidth = 100, name = "Range of # of Entrances"),
                        freq_10_pct     = colDef(minWidth = 100, name = "% of Entrances that are HU's", format = colFormat(percent = TRUE, digits = 1))
                      ))
table_1510_pct_entrances_summary

##########

# Min med mean max df for bookings (no pc holds, no Strafford) of 1%, 5%, 10% HU's

##########

# df for tables
df_hu_1_pct_summary  <- fnc_hus_descriptive_summary(booking_no_pc_hold, "high_utilizer_1_pct",  "Yes", "Coos")
df_hu_5_pct_summary  <- fnc_hus_descriptive_summary(booking_no_pc_hold, "high_utilizer_5_pct",  "Yes", "Coos")
df_hu_10_pct_summary <- fnc_hus_descriptive_summary(booking_no_pc_hold, "high_utilizer_10_pct", "Yes", "Coos")

# reactable table
table_hu_1_pct_summary  <- fnc_reactable_descriptive_summary(df_hu_1_pct_summary,  "1% HU",  "Booking")
table_hu_5_pct_summary  <- fnc_reactable_descriptive_summary(df_hu_5_pct_summary,  "5% HU",  "Booking")
table_hu_10_pct_summary <- fnc_reactable_descriptive_summary(df_hu_10_pct_summary, "10% HU", "Booking")

# combine 1%, 5%, and 10% into one df
temp_1_pct <- df_hu_1_pct_summary %>%
  mutate(range = paste(min, max, sep = " - ")) %>%
  select(county,
         total_df,
         total_hu_1_pct = total_hus,
         mean_1_pct = mean,
         range_1_pct = range,
         freq_1_pct = freq)
temp_5_pct <- df_hu_5_pct_summary %>%
  mutate(range = paste(min, max, sep = " - ")) %>%
  select(county,
         #total_df,
         total_hu_5_pct = total_hus,
         mean_5_pct = mean,
         range_5_pct = range,
         freq_5_pct = freq)
temp_10_pct <- df_hu_10_pct_summary %>%
  mutate(range = paste(min, max, sep = " - ")) %>%
  select(county,
         #total_df,
         total_hu_10_pct = total_hus,
         mean_10_pct = mean,
         range_10_pct = range,
         freq_10_pct = freq)
temp_1510_pct <- temp_1_pct %>%
  left_join(temp_5_pct, by = "county") %>%
  left_join(temp_10_pct, by = "county")

# reactable table for booking
table_1510_pct_booking_summary <- reactable(temp_1510_pct,
                                            pagination = FALSE,
                                            style = list(fontFamily = "Franklin Gothic Book"),
                                            rowStyle = function(index) {
                                              if (index %in% c(9)) {
                                                list(`border-top` = "thin solid",
                                                     fontWeight = "bold")
                                              }
                                            },
                                            columnGroups = list(
                                              colGroup(name = "Top 1%",  columns = c("total_hu_1_pct",  "mean_1_pct",  "range_1_pct",  "freq_1_pct")),
                                              colGroup(name = "Top 5%",  columns = c("total_hu_5_pct",  "mean_5_pct",  "range_5_pct",  "freq_5_pct")),
                                              colGroup(name = "Top 10%", columns = c("total_hu_10_pct", "mean_10_pct", "range_10_pct", "freq_10_pct"))
                                            ),
                                            theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
                                            defaultColDef = reactable::colDef(
                                              format = colFormat(separators = TRUE), align = "center"),
                                            compact = TRUE,
                                            fullWidth = FALSE,
                                            columns = list(
                                              county         = colDef(minWidth = 160, name = "County", align = "left",
                                                                      style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),
                                              total_df       = colDef(minWidth = 100, name = "# Bookings", show = T),
                                              total_hu_1_pct = colDef(minWidth = 100, name = "# Bookings"),
                                              mean_1_pct     = colDef(minWidth = 100, name = "Avg # of Bookings /Yr"),
                                              range_1_pct    = colDef(minWidth = 100, name = "Range of # of Bookings"),
                                              freq_1_pct     = colDef(style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3"), minWidth = 100, name = "% of Bookings that are HU's", format = colFormat(percent = TRUE, digits = 1)),

                                              total_hu_5_pct = colDef(minWidth = 100, name = "# Bookings",),
                                              mean_5_pct     = colDef(minWidth = 100, name = "Avg # of Bookings /Yr"),
                                              range_5_pct    = colDef(minWidth = 100, name = "Range of # of Bookings"),
                                              freq_5_pct     = colDef(style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3"), minWidth = 100, name = "% of Bookings that are HU's", format = colFormat(percent = TRUE, digits = 1)),

                                              total_hu_10_pct = colDef(minWidth = 100, name = "# Bookings"),
                                              mean_10_pct     = colDef(minWidth = 100, name = "Avg # of Bookings /Yr"),
                                              range_10_pct    = colDef(minWidth = 100, name = "Range of # of Bookings"),
                                              freq_10_pct     = colDef(minWidth = 100, name = "% of Bookings that are HU's", format = colFormat(percent = TRUE, digits = 1))
                                            ))
table_1510_pct_booking_summary

