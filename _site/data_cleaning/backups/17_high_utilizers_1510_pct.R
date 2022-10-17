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


df_hu_df <- bookings_entrances %>%
  ungroup() %>%
  filter(high_utilizer_1_pct == "Yes") %>%
  select(fy,
         county,
         id,
         booking_id,
         num_bookings,
         los,
         los_category,
         high_utilizer_4_times,
         high_utilizer_1_pct,
         high_utilizer_5_pct,
         high_utilizer_10_pct
  ) %>%
  distinct() %>%
  mutate(county = case_when(county == "Coos" ~ "Coos (bookings)", TRUE ~ county))

# summary table showing min, median, mean, and max of df
df_summary <- fnc_summary_county(df_hu_df, "num_bookings")
df_total                     <- fnc_summary(df_hu_df, "num_bookings")
df_total                     <- df_total %>% mutate(county = "State")
df_summary <- rbind(df_summary, df_total)


##########

# Min med mean max df for bookings and entrances of all people

##########

# min, median, mean, and max of df
# all people, not just HU's
# select variables
# all counties included
df_df <- bookings_entrances %>%
  ungroup() %>%
  select(fy,
         county,
         id,
         booking_id,
         num_bookings,
         los,
         los_category,
         high_utilizer_4_times,
         high_utilizer_1_pct,
         high_utilizer_5_pct,
         high_utilizer_10_pct
  ) %>%
  distinct() %>%
  mutate(county = case_when(county == "Coos" ~ "Coos (bookings)", TRUE ~ county))

# summary table showing min, median, mean, and max of df
df_num_summary <- fnc_summary_county(df_df, "num_bookings")
df_total                  <- fnc_summary(df_df, "num_bookings")
df_total                  <- df_total %>% mutate(county = "State")
df_num_summary <- rbind(df_num_summary, df_total)

# get total bookings and entrances by county
temp <- df_num_summary %>% select(county, total_df = total, mean_all = mean)
df_summary <- df_summary %>%
  left_join(temp, by = "county") %>%
  mutate(freq = total/total_df) %>%
  select(county, total_df, mean_all, total_hus = total, mean, min, max, freq)












##########

# Min med mean max df for entrances of 1%, 5%, 10% HU's

##########

# df for tables
df_hu_1_pct_summary  <- fnc_hus_descriptive_summary(bookings_entrances, "high_utilizer_1_pct",  "Yes", "Coos (bookings only)")
df_hu_5_pct_summary  <- fnc_hus_descriptive_summary(bookings_entrances, "high_utilizer_5_pct",  "Yes", "Coos (bookings only)")
df_hu_10_pct_summary <- fnc_hus_descriptive_summary(bookings_entrances, "high_utilizer_10_pct", "Yes", "Coos (bookings only)")

# # reactable table
# table_hu_1_pct_summary  <- fnc_reactable_descriptive_summary(df_hu_1_pct_summary,  "1% HU",  "Entrances")
# table_hu_5_pct_summary  <- fnc_reactable_descriptive_summary(df_hu_5_pct_summary,  "5% HU",  "Entrances")
# table_hu_10_pct_summary <- fnc_reactable_descriptive_summary(df_hu_10_pct_summary, "10% HU", "Entrances")

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

################################################################################

# Reactable table showing LOS by HU type

################################################################################

# overall LOS for all non-PC hold bookings for 1% HU's
temp <- df_los_no_pc_hold %>% filter(high_utilizer_1_pct == "Yes") %>% select(los)
df_los_summary_1_pct <- fnc_summary(temp, "los")
df_los_summary_1_pct <- df_los_summary_1_pct %>% mutate(hu = "Top 1%") %>% select(hu, everything())

# overall LOS for all non-PC hold bookings for 5% HU's
temp <- df_los_no_pc_hold %>% filter(high_utilizer_5_pct == "Yes") %>% select(los)
df_los_summary_5_pct <- fnc_summary(temp, "los")
df_los_summary_5_pct <- df_los_summary_5_pct %>% mutate(hu = "Top 5%") %>% select(hu, everything())

# overall LOS for all non-PC hold bookings for 10% HU's
temp <- df_los_no_pc_hold %>% filter(high_utilizer_10_pct == "Yes") %>% select(los)
df_los_summary_10_pct <- fnc_summary(temp, "los")
df_los_summary_10_pct <- df_los_summary_10_pct %>% mutate(hu = "Top 10%") %>% select(hu, everything())

# add data together
df_los_summary_1510_pct <- rbind(df_los_summary_1_pct, df_los_summary_5_pct, df_los_summary_10_pct)

# reactable table for LOS summary statistics by HU type
table_los_summary_1510_pct <- reactable(df_los_summary_1510_pct,
                    pagination = FALSE,
                    style = list(fontFamily = "Franklin Gothic Book"),
                    theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
                    defaultColDef = reactable::colDef(
                      format = colFormat(separators = TRUE), align = "center"),
                    compact = TRUE,
                    fullWidth = FALSE,
                    columns = list(
                      hu = colDef(minWidth = 190, name = header_name, align = "left",
                                                 style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),
                      min     = colDef(minWidth = 100, name = "Minimum", show = F),
                      median  = colDef(minWidth = 100, name = "Median", show = F),
                      mean    = colDef(minWidth = 130, name = mean1_name,
                                       style = list(fontWeight = "bold")),
                      max     = colDef(minWidth = 130, name = max1_name)))

################################################################################

# Histogram showing LOS by HU type

################################################################################

df_los_1_pct <- df_los_no_pc_hold %>%
  filter(high_utilizer_1_pct == "Yes") %>% group_by(los_category) %>% summarise(total = n()) %>%
  mutate(hu = "Top 1%")
df_los_5_pct <- df_los_no_pc_hold %>%
  filter(high_utilizer_5_pct == "Yes") %>% group_by(los_category) %>% summarise(total = n()) %>%
  mutate(hu = "Top 5%")
df_los_10_pct <- df_los_no_pc_hold %>%
  filter(high_utilizer_10_pct == "Yes") %>% group_by(los_category) %>% summarise(total = n()) %>%
  mutate(hu = "Top 10%")
df_los_pct <- rbind(df_los_1_pct, df_los_5_pct, df_los_10_pct)

gg_los_category_by_hu <- ggplot(df_los_pct, aes(los_category, total)) +
  geom_bar(aes(fill = hu), position = "dodge", stat="identity", width = 0.75) +
  scale_fill_manual(values=c(jri_light_blue, jri_green, jri_orange),
                    labels = c("Top 1%      ","Top 5%      ", "Top 10%")) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  xlab("Length of Stay (Days)") + ylab("Count\n") +
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
pct_los_between_0_10_days <- df_los_no_pc_hold %>% group_by(los_category) %>%
  filter(high_utilizer_1_pct == "Yes") %>%
  summarise(total = n()) %>%
  mutate(pct = round(total/sum(total)*100, 1)) %>%
  mutate(los_category = as.character(los_category))
pct_los_between_0_10_days = pct_los_between_0_10_days[-c(6:10),]
pct_los_between_0_10_days <- sum(pct_los_between_0_10_days$pct)

# get % of bookings that are 0-1 days
pct_los_between_0_1_days <- df_los_no_pc_hold %>% group_by(los_category) %>%
  filter(high_utilizer_1_pct == "Yes") %>%
  summarise(total = n()) %>%
  mutate(pct = round(total/sum(total)*100, 1)) %>%
  mutate(los_category = as.character(los_category))
pct_los_between_0_1_days = pct_los_between_0_1_days[-c(3:10),]
pct_los_between_0_1_days <- sum(pct_los_between_0_1_days$pct)

