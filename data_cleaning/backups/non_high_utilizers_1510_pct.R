############################################
# Project: JRI New Hampshire
# File:  non_high_utilizers_1510_pct.R
# Last updated: October 12, 2022
# Author: Mari Roberts

# Tables, graphs, and numbers for high utilizers analysis page
############################################

##########

# Min med mean max df for entrances of 1%, 5%, 10% non-HU's

##########

# df for tables
df_non_hu_1_pct_summary  <- fnc_hus_descriptive_summary(bookings_entrances, "high_utilizer_1_pct",  "No", "Coos (bookings only)")
df_non_hu_5_pct_summary  <- fnc_hus_descriptive_summary(bookings_entrances, "high_utilizer_5_pct",  "No", "Coos (bookings only)")
df_non_hu_10_pct_summary <- fnc_hus_descriptive_summary(bookings_entrances, "high_utilizer_10_pct", "No", "Coos (bookings only)")

# reactable table
table_non_hu_1_pct_summary  <- fnc_reactable_descriptive_summary(df_non_hu_1_pct_summary,  "Not in 1%",  "Entrances")
table_non_hu_5_pct_summary  <- fnc_reactable_descriptive_summary(df_non_hu_5_pct_summary,  "Not in 5%",  "Entrances")
table_non_hu_10_pct_summary <- fnc_reactable_descriptive_summary(df_non_hu_10_pct_summary, "Not in 10%", "Entrances")

# combine Not in 1%, 5%, and 10% into one df
temp_1_pct <- df_non_hu_1_pct_summary %>%
  mutate(range = paste(min, max, sep = " - ")) %>%
  select(county,
         total_df,
         total_non_hu_1_pct = total_hus,
         mean_1_pct = mean,
         range_1_pct = range,
         freq_1_pct = freq)
temp_5_pct <- df_non_hu_5_pct_summary %>%
  mutate(range = paste(min, max, sep = " - ")) %>%
  select(county,
         #total_df,
         total_non_hu_5_pct = total_hus,
         mean_5_pct = mean,
         range_5_pct = range,
         freq_5_pct = freq)
temp_10_pct <- df_non_hu_10_pct_summary %>%
  mutate(range = paste(min, max, sep = " - ")) %>%
  select(county,
         #total_df,
         total_non_hu_10_pct = total_hus,
         mean_10_pct = mean,
         range_10_pct = range,
         freq_10_pct = freq)
temp_1510_pct <- temp_1_pct %>%
  left_join(temp_5_pct, by = "county") %>%
  left_join(temp_10_pct, by = "county")

# reactable table for entrances
table_non_hu_1510_pct_entrances_summary <- reactable(temp_1510_pct,
                                              pagination = FALSE,
                                              style = list(fontFamily = "Franklin Gothic Book"),
                                              rowStyle = function(index) {
                                                if (index %in% c(10)) {
                                                  list(`border-top` = "thin solid",
                                                       fontWeight = "bold")
                                                }
                                              },
                                              columnGroups = list(
                                                colGroup(name = "Not in Top 1%",  columns = c("total_non_hu_1_pct",  "mean_1_pct",  "range_1_pct",  "freq_1_pct")),
                                                colGroup(name = "Not in Top 5%",  columns = c("total_non_hu_5_pct",  "mean_5_pct",  "range_5_pct",  "freq_5_pct")),
                                                colGroup(name = "Not in Top 10%", columns = c("total_non_hu_10_pct", "mean_10_pct", "range_10_pct", "freq_10_pct"))
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
                                                total_non_hu_1_pct = colDef(minWidth = 100, name = "# Entrances"),
                                                mean_1_pct     = colDef(minWidth = 100, name = "Avg # of Entrances /Yr"),
                                                range_1_pct    = colDef(minWidth = 100, name = "Range of # of Entrances"),
                                                freq_1_pct     = colDef(style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3"), minWidth = 100, name = "% of Entrances that are HU's", format = colFormat(percent = TRUE, digits = 1)),

                                                total_non_hu_5_pct = colDef(minWidth = 100, name = "# Entrances",),
                                                mean_5_pct     = colDef(minWidth = 100, name = "Avg # of Entrances /Yr"),
                                                range_5_pct    = colDef(minWidth = 100, name = "Range of # of Entrances"),
                                                freq_5_pct     = colDef(style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3"), minWidth = 100, name = "% of Entrances that are HU's", format = colFormat(percent = TRUE, digits = 1)),

                                                total_non_hu_10_pct = colDef(minWidth = 100, name = "# Entrances"),
                                                mean_10_pct     = colDef(minWidth = 100, name = "Avg # of Entrances /Yr"),
                                                range_10_pct    = colDef(minWidth = 100, name = "Range of # of Entrances"),
                                                freq_10_pct     = colDef(minWidth = 100, name = "% of Entrances that are HU's", format = colFormat(percent = TRUE, digits = 1))
                                              ))
table_non_hu_1510_pct_entrances_summary

##########

# Min med mean max df for bookings (no pc holds, no Strafford) of non-1%, 5%, 10% HU's

##########

# df for tables
df_non_hu_1_pct_summary  <- fnc_hus_descriptive_summary(booking_no_pc_hold, "high_utilizer_1_pct",  "No", "Coos")
df_non_hu_5_pct_summary  <- fnc_hus_descriptive_summary(booking_no_pc_hold, "high_utilizer_5_pct",  "No", "Coos")
df_non_hu_10_pct_summary <- fnc_hus_descriptive_summary(booking_no_pc_hold, "high_utilizer_10_pct", "No", "Coos")

# reactable table
table_non_hu_1_pct_summary  <- fnc_reactable_descriptive_summary(df_non_hu_1_pct_summary,  "Not in 1%",  "Booking")
table_non_hu_5_pct_summary  <- fnc_reactable_descriptive_summary(df_non_hu_5_pct_summary,  "Not in 5%",  "Booking")
table_non_hu_10_pct_summary <- fnc_reactable_descriptive_summary(df_non_hu_10_pct_summary, "Not in 10%", "Booking")

# combine 1%, 5%, and 10% into one df
temp_1_pct <- df_non_hu_1_pct_summary %>%
  mutate(range = paste(min, max, sep = " - ")) %>%
  select(county,
         total_df,
         total_non_hu_1_pct = total_hus,
         mean_1_pct = mean,
         range_1_pct = range,
         freq_1_pct = freq)
temp_5_pct <- df_non_hu_5_pct_summary %>%
  mutate(range = paste(min, max, sep = " - ")) %>%
  select(county,
         #total_df,
         total_non_hu_5_pct = total_hus,
         mean_5_pct = mean,
         range_5_pct = range,
         freq_5_pct = freq)
temp_10_pct <- df_non_hu_10_pct_summary %>%
  mutate(range = paste(min, max, sep = " - ")) %>%
  select(county,
         #total_df,
         total_non_hu_10_pct = total_hus,
         mean_10_pct = mean,
         range_10_pct = range,
         freq_10_pct = freq)
temp_1510_pct <- temp_1_pct %>%
  left_join(temp_5_pct, by = "county") %>%
  left_join(temp_10_pct, by = "county")

# reactable table for booking
table_non_hu_1510_pct_booking_summary <- reactable(temp_1510_pct,
                                            pagination = FALSE,
                                            style = list(fontFamily = "Franklin Gothic Book"),
                                            rowStyle = function(index) {
                                              if (index %in% c(9)) {
                                                list(`border-top` = "thin solid",
                                                     fontWeight = "bold")
                                              }
                                            },
                                            columnGroups = list(
                                              colGroup(name = "Not in Top 1%",  columns = c("total_non_hu_1_pct",  "mean_1_pct",  "range_1_pct",  "freq_1_pct")),
                                              colGroup(name = "Not in Top 5%",  columns = c("total_non_hu_5_pct",  "mean_5_pct",  "range_5_pct",  "freq_5_pct")),
                                              colGroup(name = "Not in Top 10%", columns = c("total_non_hu_10_pct", "mean_10_pct", "range_10_pct", "freq_10_pct"))
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
                                              total_non_hu_1_pct = colDef(minWidth = 100, name = "# Bookings"),
                                              mean_1_pct     = colDef(minWidth = 100, name = "Avg # of Bookings /Yr"),
                                              range_1_pct    = colDef(minWidth = 100, name = "Range of # of Bookings"),
                                              freq_1_pct     = colDef(style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3"), minWidth = 100, name = "% of Bookings that are HU's", format = colFormat(percent = TRUE, digits = 1)),

                                              total_non_hu_5_pct = colDef(minWidth = 100, name = "# Bookings",),
                                              mean_5_pct     = colDef(minWidth = 100, name = "Avg # of Bookings /Yr"),
                                              range_5_pct    = colDef(minWidth = 100, name = "Range of # of Bookings"),
                                              freq_5_pct     = colDef(style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3"), minWidth = 100, name = "% of Bookings that are HU's", format = colFormat(percent = TRUE, digits = 1)),

                                              total_non_hu_10_pct = colDef(minWidth = 100, name = "# Bookings"),
                                              mean_10_pct     = colDef(minWidth = 100, name = "Avg # of Bookings /Yr"),
                                              range_10_pct    = colDef(minWidth = 100, name = "Range of # of Bookings"),
                                              freq_10_pct     = colDef(minWidth = 100, name = "% of Bookings that are HU's", format = colFormat(percent = TRUE, digits = 1))
                                            ))
table_non_hu_1510_pct_booking_summary

################################################################################

# Reactable table showing LOS by non-HU type

################################################################################

# overall LOS for all non-PC hold bookings for 1% HU's
temp <- df_los_no_pc_hold %>% filter(high_utilizer_1_pct == "No") %>% select(los)
df_los_summary_non_hu_1_pct <- fnc_summary(temp, "los")
df_los_summary_non_hu_1_pct <- df_los_summary_non_hu_1_pct %>% mutate(hu = "Not in Top 1%") %>% select(hu, everything())

# overall LOS for all non-PC hold bookings for 5% HU's
temp <- df_los_no_pc_hold %>% filter(high_utilizer_5_pct == "No") %>% select(los)
df_los_summary_non_hu_5_pct <- fnc_summary(temp, "los")
df_los_summary_non_hu_5_pct <- df_los_summary_non_hu_5_pct %>% mutate(hu = "Not in Top 5%") %>% select(hu, everything())

# overall LOS for all non-PC hold bookings for 10% HU's
temp <- df_los_no_pc_hold %>% filter(high_utilizer_10_pct == "No") %>% select(los)
df_los_summary_non_hu_10_pct <- fnc_summary(temp, "los")
df_los_summary_non_hu_10_pct <- df_los_summary_non_hu_10_pct %>% mutate(hu = "Not in Top 10%") %>% select(hu, everything())

# add data together
df_los_summary_non_hu_1510_pct <- rbind(df_los_summary_non_hu_1_pct, df_los_summary_non_hu_5_pct, df_los_summary_non_hu_10_pct)

# reactable table for LOS summary statistics by non-HU type
table_los_summary_non_hu_1510_pct <- reactable(df_los_summary_non_hu_1510_pct,
                                        pagination = FALSE,
                                        style = list(fontFamily = "Franklin Gothic Book"),
                                        theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
                                        defaultColDef = reactable::colDef(
                                          format = colFormat(separators = TRUE), align = "center"),
                                        compact = TRUE,
                                        fullWidth = FALSE,
                                        columns = list(
                                          hu = colDef(minWidth = 190, name = "", align = "left",
                                                      style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),
                                          min     = colDef(minWidth = 100, name = "Minimum", show = F),
                                          median  = colDef(minWidth = 100, name = "Median", show = F),
                                          mean    = colDef(minWidth = 130, name = "Average",
                                                           style = list(fontWeight = "bold")),
                                          max     = colDef(minWidth = 130, name = "Maximum")))
