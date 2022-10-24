############################################
# Project: JRI New Hampshire
# File:  non_high_utilizers.R
# Last updated: October 19, 2022
# Author: Mari Roberts

# Non-High Utilizers Based on Jail Bookings by State.
# Explore non-HU's defined as people not in the 1%, 5%, and 10%.

# Tables, graphs, and numbers for high utilizers analysis page.
############################################

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Min med mean max df for bookings and entrances of non-HU's by county

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Summary info for each type of non-HU
# Ignore warnings
df_hu_4_times_summary <- fnc_hus_descriptive_summary(bookings_entrances, "high_utilizer_4_times", "No", "Coos (bookings only)")
df_hu_1_pct_summary   <- fnc_hus_descriptive_summary(bookings_entrances, "high_utilizer_1_pct",   "No", "Coos (bookings only)")
df_hu_5_pct_summary   <- fnc_hus_descriptive_summary(bookings_entrances, "high_utilizer_5_pct",   "No", "Coos (bookings only)")
df_hu_10_pct_summary  <- fnc_hus_descriptive_summary(bookings_entrances, "high_utilizer_10_pct",  "No", "Coos (bookings only)")

# Subset data for total entrances (HU and non-HU), total people, and average number of entrances a year
# Created in incarceration_patterns_entrances.R
df_entrances1 <- df_entrances_table %>% select(county, entrances_total, people_entered_total, avg_entrances)

# Add labels to metrics to identify 1, 5, and 10%
data_1_pct <- df_hu_1_pct_summary  %>% rename_with(~paste0(., "_1_pct"),  -c("county"))
data_5_pct <- df_hu_5_pct_summary  %>% rename_with(~paste0(., "_5_pct"),  -c("county"))
data_10_pct <- df_hu_10_pct_summary %>% rename_with(~paste0(., "_10_pct"), -c("county"))

# Combine county entrances and county HU entrances info
df_1_5_10 <- df_entrances1 %>%
  left_join(data_1_pct, by = "county") %>%
  left_join(data_5_pct, by = "county") %>%
  left_join(data_10_pct, by = "county") %>%
  mutate(freq_1_pct = total_hu_entrances_1_pct/entrances_total,
         freq_5_pct = total_hu_entrances_5_pct/entrances_total,
         freq_10_pct = total_hu_entrances_10_pct/entrances_total) %>%
  mutate(range_1_pct = paste(min_1_pct, max_1_pct, sep = "-"),
         range_5_pct = paste(min_5_pct, max_5_pct, sep = "-"),
         range_10_pct = paste(min_10_pct, max_10_pct, sep = "-")) %>%
  arrange(county %in% "State")

# reactable table for presentation showing the number of HU's, min, med, mean, max, etc.
PRES_non_hu_summary <- reactable(df_1_5_10,
                             pagination = FALSE,
                             style = list(fontFamily = "Franklin Gothic Book"),
                             rowStyle = function(index) {
                               if (index %in% c(10)) {
                                 list(`border-top` = "thin solid",
                                      fontWeight = "bold")
                               }
                             },
                             columnGroups = list(
                               colGroup(name = "Not in Top 1% HU's",  columns = c("total_hu_entrances_1_pct",  "total_hu_people_1_pct",  "freq_1_pct",  "min_1_pct",  "median_1_pct", "mean_1_pct", "max_1_pct", "range_1_pct")),
                               colGroup(name = "Not in Top 5% HU's",  columns = c("total_hu_entrances_5_pct",  "total_hu_people_5_pct",  "freq_5_pct",  "min_5_pct",  "median_5_pct", "mean_5_pct", "max_5_pct", "range_5_pct")),
                               colGroup(name = "Not in Top 10% HU's",  columns = c("total_hu_entrances_10_pct", "total_hu_people_10_pct", "freq_10_pct", "min_10_pct", "median_10_pct", "mean_10_pct", "max_10_pct", "range_10_pct"))
                             ),
                             theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"),
                                                    headerStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
                             defaultColDef = reactable::colDef(
                               format = colFormat(separators = TRUE), align = "center"),
                             compact = TRUE,
                             fullWidth = FALSE,
                             columns = list(
                               county                = colDef(show = T, minWidth = 190, name = "County", align = "left",
                                                              style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),
                               entrances_total       = colDef(show = F, minWidth = 100, name = "Total Entrances"),
                               people_entered_total  = colDef(show = F, minWidth = 100,  name = "Total People"),
                               avg_entrances         = colDef(show = F, minWidth = 130, name = "Avg Entrances Per Person",
                                                              format = colFormat(percent = FALSE, digits = 1),
                                                              style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),

                               total_hu_entrances_1_pct = colDef(show = F, minWidth = 100, name = "HU's (Entrances)"),
                               total_hu_people_1_pct    = colDef(show = F, minWidth = 100, name = "HU's (People)"),
                               freq_1_pct               = colDef(show = F, minWidth = 100, name = "Proportion of HU Entrances",
                                                                 style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3"),
                                                                 format = colFormat(percent = TRUE, digits = 1)),
                               min_1_pct                = colDef(show = F, minWidth = 100, name = "Minimum Number of Entrances Per Person"),
                               median_1_pct             = colDef(show = T, minWidth = 100, name = "Median Number of Entrances Per Person"),
                               mean_1_pct               = colDef(show = T, minWidth = 130, name = "Avg Number of Entrances Per Person", format = colFormat(percent = FALSE, digits = 1)),
                               range_1_pct              = colDef(show = T, minWidth = 100, name = "Min - Max Number of Entrances Per Person",
                                                                 style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),

                               max_1_pct                = colDef(show = F, minWidth = 130, name = "Maximum Number of Entrances Per Person",
                                                                 style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),

                               total_hu_entrances_5_pct = colDef(show = F, minWidth = 100, name = "HU's (Entrances)"),
                               total_hu_people_5_pct    = colDef(show = F, minWidth = 100, name = "HU's (People)"),
                               freq_5_pct               = colDef(show = F, minWidth = 100, name = "Proportion of HU Entrances",
                                                                 style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3"),
                                                                 format = colFormat(percent = TRUE, digits = 1)),
                               min_5_pct                = colDef(show = F, minWidth = 100, name = "Minimum Number of Entrances Per Person"),
                               median_5_pct             = colDef(show = T, minWidth = 100, name = "Median Number of Entrances Per Person"),
                               mean_5_pct               = colDef(show = T, minWidth = 130, name = "Avg Number of Entrances Per Person", format = colFormat(percent = FALSE, digits = 1)),
                               range_5_pct              = colDef(show = T, minWidth = 100, name = "Min - Max Number of Entrances Per Person",
                                                                 style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),

                               max_5_pct                = colDef(show = F, minWidth = 130, name = "Maximum Number of Entrances Per Person",
                                                                 style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),

                               total_hu_entrances_10_pct = colDef(show = F, minWidth = 100, name = "HU's (Entrances)"),
                               total_hu_people_10_pct    = colDef(show = F, minWidth = 100, name = "HU's (People)"),
                               freq_10_pct               = colDef(show = F, minWidth = 100, name = "Proportion of HU Entrances",
                                                                  style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3"),
                                                                  format = colFormat(percent = TRUE, digits = 1)),
                               min_10_pct                = colDef(show = F, minWidth = 100, name = "Minimum Number of Entrances Per Person"),
                               median_10_pct             = colDef(show = T, minWidth = 100, name = "Median Number of Entrances Per Person"),
                               mean_10_pct               = colDef(show = T, minWidth = 130, name = "Avg Number of Entrances Per Person", format = colFormat(percent = FALSE, digits = 1)),
                               range_10_pct              = colDef(show = T, minWidth = 100, name = "Min - Max Number of Entrances Per Person",
                                                                  style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),

                               max_10_pct                = colDef(show = F, minWidth = 130, name = "Maximum Number of Entrances Per Person",
                                                                  style = list(position = "sticky", borderRight = "1px solid #d3d3d3"))

                             ))

# same as before but showing different metrics (show = T or show = F)
PRES_non_hu_summary1 <- reactable(df_1_5_10,
                              pagination = FALSE,
                              style = list(fontFamily = "Franklin Gothic Book"),
                              rowStyle = function(index) {
                                if (index %in% c(10)) {
                                  list(`border-top` = "thin solid",
                                       fontWeight = "bold")
                                }
                              },
                              columnGroups = list(
                                colGroup(name = "Not in Top 1% HU's",  columns = c("total_hu_entrances_1_pct",  "total_hu_people_1_pct",  "freq_1_pct",  "min_1_pct",  "median_1_pct", "mean_1_pct", "max_1_pct", "range_1_pct")),
                                colGroup(name = "Not in Top 5% HU's",  columns = c("total_hu_entrances_5_pct",  "total_hu_people_5_pct",  "freq_5_pct",  "min_5_pct",  "median_5_pct", "mean_5_pct", "max_5_pct", "range_5_pct")),
                                colGroup(name = "Not in Top 10% HU's",  columns = c("total_hu_entrances_10_pct", "total_hu_people_10_pct", "freq_10_pct", "min_10_pct", "median_10_pct", "mean_10_pct", "max_10_pct", "range_10_pct"))
                              ),
                              theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"),
                                                     headerStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
                              defaultColDef = reactable::colDef(
                                format = colFormat(separators = TRUE), align = "center"),
                              compact = TRUE,
                              fullWidth = FALSE,
                              columns = list(
                                county                = colDef(show = T, minWidth = 190, name = "County", align = "left",
                                                               style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),
                                entrances_total       = colDef(show = F, minWidth = 100, name = "Total Entrances"),
                                people_entered_total  = colDef(show = F, minWidth = 100,  name = "Total People"),
                                avg_entrances         = colDef(show = F, minWidth = 130, name = "Avg Entrances Per Person",
                                                               format = colFormat(percent = FALSE, digits = 1),
                                                               style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),

                                total_hu_entrances_1_pct = colDef(show = T, minWidth = 100, name = "HU's (Entrances)"),
                                total_hu_people_1_pct    = colDef(show = T, minWidth = 100, name = "HU's (People)"),
                                freq_1_pct               = colDef(show = T, minWidth = 100, name = "Proportion of HU Entrances",
                                                                  style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3"),
                                                                  format = colFormat(percent = TRUE, digits = 1)),
                                min_1_pct                = colDef(show = F, minWidth = 100, name = "Minimum Number of Entrances Per Person"),
                                median_1_pct             = colDef(show = F, minWidth = 100, name = "Median Number of Entrances Per Person"),
                                mean_1_pct               = colDef(show = F, minWidth = 130, name = "Avg Number of Entrances Per Person", format = colFormat(percent = FALSE, digits = 1)),
                                range_1_pct              = colDef(show = F, minWidth = 100, name = "Min - Max Number of Entrances Per Person",
                                                                  style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),

                                max_1_pct                = colDef(show = F, minWidth = 130, name = "Maximum Number of Entrances Per Person",
                                                                  style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),

                                total_hu_entrances_5_pct = colDef(show = T, minWidth = 100, name = "HU's (Entrances)"),
                                total_hu_people_5_pct    = colDef(show = T, minWidth = 100, name = "HU's (People)"),
                                freq_5_pct               = colDef(show = T, minWidth = 100, name = "Proportion of HU Entrances",
                                                                  style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3"),
                                                                  format = colFormat(percent = TRUE, digits = 1)),
                                min_5_pct                = colDef(show = F, minWidth = 100, name = "Minimum Number of Entrances Per Person"),
                                median_5_pct             = colDef(show = F, minWidth = 100, name = "Median Number of Entrances Per Person"),
                                mean_5_pct               = colDef(show = F, minWidth = 130, name = "Avg Number of Entrances Per Person", format = colFormat(percent = FALSE, digits = 1)),
                                range_5_pct              = colDef(show = F, minWidth = 100, name = "Min - Max Number of Entrances Per Person",
                                                                  style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),

                                max_5_pct                = colDef(show = F, minWidth = 130, name = "Maximum Number of Entrances Per Person",
                                                                  style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),

                                total_hu_entrances_10_pct = colDef(show = T, minWidth = 100, name = "HU's (Entrances)"),
                                total_hu_people_10_pct    = colDef(show = T, minWidth = 100, name = "HU's (People)"),
                                freq_10_pct               = colDef(show = T, minWidth = 100, name = "Proportion of HU Entrances",
                                                                   style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3"),
                                                                   format = colFormat(percent = TRUE, digits = 1)),
                                min_10_pct                = colDef(show = F, minWidth = 100, name = "Minimum Number of Entrances Per Person"),
                                median_10_pct             = colDef(show = F, minWidth = 100, name = "Median Number of Entrances Per Person"),
                                mean_10_pct               = colDef(show = F, minWidth = 130, name = "Avg Number of Entrances Per Person", format = colFormat(percent = FALSE, digits = 1)),
                                range_10_pct              = colDef(show = F, minWidth = 100, name = "Min - Max Number of Entrances Per Person",
                                                                   style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),

                                max_10_pct                = colDef(show = F, minWidth = 130, name = "Maximum Number of Entrances Per Person",
                                                                   style = list(position = "sticky", borderRight = "1px solid #d3d3d3"))

                              ))
