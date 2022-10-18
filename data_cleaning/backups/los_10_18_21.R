############################################
# Project: JRI New Hampshire
# File: los.R
# Last updated: October 18, 2022
# Author: Mari Roberts

# Tables, graphs, and numbers for los
############################################

# count freq of los for entrances (including Coos bookings and Strafford)
df_los_entrances <- bookings_entrances %>%
  select(fy,
         county,
         booking_id,
         los,
         high_utilizer_4_times,
         high_utilizer_1_pct,
         high_utilizer_5_pct,
         high_utilizer_10_pct) %>%
  distinct() %>%
  filter(!is.na(los))
dim(df_los_entrances); length(unique(df_los_entrances$booking_id)) # 51349, 51349 will be less because of NAs in LOS's

# average lOS for entrances
avg_los_entrances <- df_los_entrances %>%
  group_by() %>%
  dplyr::summarize(mean = mean(los, na.rm=TRUE))
avg_los_entrances <- as.numeric(avg_los_entrances)

# overall LOS for entrances
df_los_entrances_summary <- df_los_entrances %>%
  group_by() %>%
  summarise(
    total  = n(),
    min    = min(los, na.rm = T),
    median = median(los, na.rm = T),
    mean   = mean(c(los, na.rm = T)),
    max    = max(los, na.rm = T)
  ) %>%
  mutate(mean = round(mean, 1))
df_los_entrances_summary <- df_los_entrances_summary %>% mutate(type = "All (HU's and non-HU's)") %>% select(type, everything())

##########

# Reactable table for LOS for entrances (including Coos bookings and Strafford)

##########

# reactable table for LOS summary statistics by HU type
table_los_entrances_with_coos_summary <- reactable(df_los_entrances_summary,
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

