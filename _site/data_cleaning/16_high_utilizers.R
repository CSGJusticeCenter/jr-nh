############################################
# Project: JRI New Hampshire
# File:  high_utilizers.R
# Last updated: October 19, 2022
# Author: Mari Roberts

# High Utilizers Based on Jail Entrances by State
# Explore HU's defined as 1%, 5%, and 10%

# Tables, graphs, and numbers for high utilizers analysis page
############################################

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Find the min med mean max number of entrances for HU's

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Summary info for each type of HU
# Ignore warnings
df_hu_4_times_summary <- fnc_hus_descriptive_summary(bookings_entrances, "high_utilizer_4_times", "Yes", "Coos (bookings only)")
df_hu_1_pct_summary   <- fnc_hus_descriptive_summary(bookings_entrances, "high_utilizer_1_pct",   "Yes", "Coos (bookings only)")
df_hu_5_pct_summary   <- fnc_hus_descriptive_summary(bookings_entrances, "high_utilizer_5_pct",   "Yes", "Coos (bookings only)")
df_hu_10_pct_summary  <- fnc_hus_descriptive_summary(bookings_entrances, "high_utilizer_10_pct",  "Yes", "Coos (bookings only)")

# reactable tables by HU (will combine for presentations)
table_hu_4_times_summary <- fnc_reactable_hus_descriptive_summary(df_hu_4_times_summary)
table_hu_1_pct_summary   <- fnc_reactable_hus_descriptive_summary(df_hu_1_pct_summary)
table_hu_5_pct_summary   <- fnc_reactable_hus_descriptive_summary(df_hu_5_pct_summary)
table_hu_10_pct_summary  <- fnc_reactable_hus_descriptive_summary(df_hu_10_pct_summary)

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

# Reactable table for presentation showing the number of HU's, min, med, mean, max, etc.
PRES_hu_summary <- reactable(df_1_5_10,
                    pagination = FALSE,
                    style = list(fontFamily = "Franklin Gothic Book", fontSize = "1.0rem"),
                    rowStyle = function(index) {
                      if (index %in% c(10)) {
                        list(`border-top` = "thin solid",
                             fontWeight = "bold")
                      }
                    },
                    columnGroups = list(
                      colGroup(name = "Top 1% HU's",  columns = c("total_hu_entrances_1_pct",  "total_hu_people_1_pct",  "freq_1_pct",  "min_1_pct",  "median_1_pct", "mean_1_pct", "max_1_pct", "range_1_pct")),
                      colGroup(name = "Top 5% HU's",  columns = c("total_hu_entrances_5_pct",  "total_hu_people_5_pct",  "freq_5_pct",  "min_5_pct",  "median_5_pct", "mean_5_pct", "max_5_pct", "range_5_pct")),
                      colGroup(name = "Top 10% HU's",  columns = c("total_hu_entrances_10_pct", "total_hu_people_10_pct", "freq_10_pct", "min_10_pct", "median_10_pct", "mean_10_pct", "max_10_pct", "range_10_pct"))
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

# Same as before but showing different metrics (show = T or show = F)
PRES_hu_summary1 <- reactable(df_1_5_10,
                              pagination = FALSE,
                              style = list(fontFamily = "Franklin Gothic Book", fontSize = "1.0rem"),
                              rowStyle = function(index) {
                                if (index %in% c(10)) {
                                  list(`border-top` = "thin solid",
                                       fontWeight = "bold")
                                }
                              },
                              columnGroups = list(
                                colGroup(name = "Top 1% HU's",  columns = c("total_hu_entrances_1_pct",  "total_hu_people_1_pct",  "freq_1_pct",  "min_1_pct",  "median_1_pct", "mean_1_pct", "max_1_pct", "range_1_pct")),
                                colGroup(name = "Top 5% HU's",  columns = c("total_hu_entrances_5_pct",  "total_hu_people_5_pct",  "freq_5_pct",  "min_5_pct",  "median_5_pct", "mean_5_pct", "max_5_pct", "range_5_pct")),
                                colGroup(name = "Top 10% HU's",  columns = c("total_hu_entrances_10_pct", "total_hu_people_10_pct", "freq_10_pct", "min_10_pct", "median_10_pct", "mean_10_pct", "max_10_pct", "range_10_pct"))
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

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Number of entrances and cut offs for 1, 5, 10% in a ggplot

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Get average number of entrances depending on HU
top_1_pct_avg_entrances <- df_1_5_10 %>% filter(county == "State") %>% select(mean_1_pct)
top_1_pct_avg_entrances <- as.numeric(top_1_pct_avg_entrances$mean_1_pct)
top_5_pct_avg_entrances <- df_1_5_10 %>% filter(county == "State") %>% select(mean_5_pct)
top_5_pct_avg_entrances <- as.numeric(top_5_pct_avg_entrances$mean_5_pct)
top_10_pct_avg_entrances <- df_1_5_10 %>% filter(county == "State") %>% select(mean_10_pct)
top_10_pct_avg_entrances <- as.numeric(top_10_pct_avg_entrances$mean_10_pct)

# get minimum number of entrances depending on HU
top_1_pct_min_entrances <- df_1_5_10 %>% filter(county == "State") %>% select(min_1_pct)
top_1_pct_min_entrances <- as.numeric(top_1_pct_min_entrances$min_1_pct)
top_5_pct_min_entrances <- df_1_5_10 %>% filter(county == "State") %>% select(min_5_pct)
top_5_pct_min_entrances <- as.numeric(top_5_pct_min_entrances$min_5_pct)
top_10_pct_min_entrances <- df_1_5_10 %>% filter(county == "State") %>% select(min_10_pct)
top_10_pct_min_entrances <- as.numeric(top_10_pct_min_entrances$min_10_pct)

# subset data and create categories for number of entrances
data1 <- bookings_entrances %>% ungroup() %>% select(id, num_entrances) %>% distinct() %>%
  mutate(num_entrances_category = case_when(num_entrances == 1 ~ "1",
                                           num_entrances == 2 ~ "2",
                                           num_entrances == 3 ~ "3",
                                           num_entrances == 4 ~ "4",
                                           num_entrances == 5 ~ "5",
                                           num_entrances == 6 ~ "6",
                                           num_entrances == 7 ~ "7",
                                           num_entrances == 8 ~ "8",
                                           num_entrances == 9 ~ "9",
                                           num_entrances == 10 ~ "10",
                                           num_entrances == 11 ~ "11",
                                           num_entrances == 12 ~ "12",
                                           num_entrances == 13 ~ "13",
                                           num_entrances == 14 ~ "14",
                                           num_entrances == 15 ~ "15",
                                           num_entrances == 16 ~ "16",
                                           num_entrances == 17 ~ "17",
                                           num_entrances == 18 ~ "18",
                                           num_entrances == 19 ~ "19",
                                           num_entrances >= 20 ~ "20+")) %>%
  group_by(num_entrances_category) %>% summarise(total = n())

data1$num_entrances_category <- factor(data1$num_entrances_category,
                                      levels = c("1",
                                                 "2",
                                                 "3",
                                                 "4",
                                                 "5",
                                                 "6",
                                                 "7",
                                                 "8",
                                                 "9",
                                                 "10",
                                                 "11",
                                                 "12",
                                                 "13",
                                                 "14",
                                                 "15",
                                                 "16",
                                                 "17",
                                                 "18",
                                                 "19",
                                                 "20+"))

# ggplot showing the cut off for top 1, 5, and 10% HU's depending on number of entrances
PRES_gg_hu_percentile_explanation <- ggplot(data1)+

  geom_rect(xmin="3",
            xmax="20+",
            ymin=0,
            ymax=23945,
            fill=jri_orange, alpha = 0.01) +
  geom_rect(xmin="4",
            xmax="20+",
            ymin=0,
            ymax=23945,
            fill=jri_green, alpha = 0.01) +
  geom_rect(xmin="8",
            xmax="20+",
            ymin=0,
            ymax=23945,
            fill=jri_light_blue, alpha = 0.01) +

  geom_bar(data=data1, aes(x=num_entrances_category, y=total),
           stat="identity",
           fill = "gray",
           colour= "gray", lwd=0.5) +
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3, big.mark = ","),
                     expand = c(0,0),
                     limits = c(0,26500)) +
  theme_axes +
  xlab("\nNumber of Entrances") + ylab("Number of People\n") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        legend.position = "none") +

  geom_vline(aes(xintercept=top_1_pct_min_entrances), colour=jri_light_blue) +
  geom_richtext(aes(x=top_1_pct_min_entrances+0.4, y=19600, label="Top 1%"),
                color = "white",
                size = 7.5,
                fontface = 'bold',
                fill = jri_light_blue,
                label.r = unit(0, "lines"),
                angle = 90) +

  geom_vline(aes(xintercept=top_5_pct_min_entrances), colour=jri_green) +
  geom_richtext(aes(x=top_5_pct_min_entrances+0.4, y=19600, label="Top 5%"),
                color = "white",
                size = 7.5,
                fontface = 'bold',
                fill = jri_green,
                label.r = unit(0, "lines"),
                angle = 90) +

  geom_vline(aes(xintercept=top_10_pct_min_entrances), colour=jri_orange) +
  geom_richtext(aes(x=top_10_pct_min_entrances+0.4, y=19100, label="Top 10%"),
                color = "white",
                size = 7.5,
                fontface = 'bold',
                fill = jri_orange,
                label.r = unit(0, "lines"),
                angle = 90)

# reshape data for lollipop graph showing the min and max number of entrances for each type of HU
data1 <- df_1_5_10 %>% filter(county == "State") %>%
  select(county,
         min_1_pct,
         max_1_pct,
         min_5_pct,
         max_5_pct,
         min_10_pct,
         max_10_pct)
data1 <- gather(data1, min_max, total, min_1_pct:max_10_pct, factor_key=TRUE) %>%
  mutate(hu = case_when(min_max == "min_1_pct"  | min_max == "max_1_pct"  ~ "Top 1%",
                        min_max == "min_5_pct"  | min_max == "max_5_pct"  ~ "Top 5%",
                        min_max == "min_10_pct" | min_max == "max_10_pct" ~ "Top 10%"),
         min_max = case_when(min_max == "min_1_pct" | min_max == "min_5_pct" | min_max == "min_10_pct"  ~ "Min",
                             min_max == "max_1_pct" | min_max == "max_5_pct" | min_max == "max_10_pct"  ~ "Max"))
data1 <- spread(data1, min_max, total)
data1 <- data1 %>%
  mutate(hu = factor(hu, levels = c("Top 1%", "Top 5%", "Top 10%")))

# sep by hu type
data_1_pct <- data1 %>% filter(hu == "Top 1%")
data_5_pct <- data1 %>% filter(hu == "Top 5%")
data_10_pct <- data1 %>% filter(hu == "Top 10%")

library(hrbrthemes)
ggplot() +
  # Top 1%
  geom_segment(data = data_1_pct, aes(x=hu, xend=hu, y=Min, yend=Max), color=jri_light_blue, size = 2) +
  geom_point(data = data_1_pct, aes(x=hu, y=Min), color=jri_light_blue, size=5) +
  geom_point(data = data_1_pct, aes(x=hu, y=Max), color=jri_light_blue, size=5) +
  geom_text(data = data_1_pct, aes(x=hu, y=Min, label = Min), hjust = 2, size = 8, color = "black", family = "Franklin Gothic Book") +
  geom_text(data = data_1_pct, aes(x=hu, y=Max, label = Max), hjust = -0.5, size = 8, color = "black", family = "Franklin Gothic Book") +

  # Top 5%
  geom_segment(data = data_5_pct, aes(x=hu, xend=hu, y=Min, yend=Max), color=jri_green, size = 2) +
  geom_point(data = data_5_pct, aes(x=hu, y=Min), color=jri_green, size=5) +
  geom_point(data = data_5_pct, aes(x=hu, y=Max), color=jri_green, size=5) +
  geom_text(data = data_5_pct, aes(x=hu, y=Min, label = Min), hjust = 2, size = 8, color = "black", family = "Franklin Gothic Book") +
  geom_text(data = data_5_pct, aes(x=hu, y=Max, label = Max), hjust = -0.5, size = 8, color = "black", family = "Franklin Gothic Book") +

  # Top 10%
  geom_segment(data = data_10_pct, aes(x=hu, xend=hu, y=Min, yend=Max), color=jri_orange, size = 2) +
  geom_point(data = data_10_pct, aes(x=hu, y=Min), color=jri_orange, size=5) +
  geom_point(data = data_10_pct, aes(x=hu, y=Max), color=jri_orange, size=5) +
  geom_text(data = data_10_pct, aes(x=hu, y=Min, label = Min), hjust = 2, size = 8, color = "black", family = "Franklin Gothic Book") +
  geom_text(data = data_10_pct, aes(x=hu, y=Max, label = Max), hjust = -0.5, size = 8, color = "black", family = "Franklin Gothic Book") +

  coord_flip() +
  scale_x_discrete(drop = FALSE, expand = c(0,0.3)) +
  scale_y_continuous(limits = c(0,80)) +
  theme_ipsum() +
  theme_axes +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Franklin Gothic Book", face = "bold", size = 20)) +
  xlab("") +
  ylab("Minimum and Maximum Number of Entrances")

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Line graph showing HU entrances

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

data1 <- bookings_entrances %>%
  filter(high_utilizer_10_pct == "Yes") %>%
  select(id, booking_id, fy, county, month_year, month_year_text) %>%
  distinct()

gg_10_pct_entrances_month_year <- fnc_covid_time_highchart(data1, "", "", jri_orange)

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Proportion of PC holds that are 10% high utilizers

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Select variables
data1 <- bookings_entrances %>%
  filter(county != "Coos" & county != "Strafford") %>%
  select(county,
         fy,
         id,
         booking_id,
         high_utilizer_1_pct,
         high_utilizer_5_pct,
         high_utilizer_10_pct,
         pc_hold_in_booking) %>%
  distinct() %>%
  filter(!is.na(pc_hold_in_booking)) %>%
  filter(pc_hold_in_booking == "PC Hold") %>%
  group_by(pc_hold_in_booking, high_utilizer_10_pct) %>%
  summarise(total = n())

data1 <- group_by(data1) %>% mutate(pct = total/sum(total)*100) %>%
  mutate(pct = round(pct, 1))
data1 <- as.data.frame(data1)
data1 <- data1 %>% mutate(pct = paste0(pct, "%"))

PRES_gg_10_pct_pc_holds <- ggplot(data1, aes(x = pc_hold_in_booking, y = total, fill = high_utilizer_10_pct)) +
  geom_col(colour = NA, position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("gray", jri_orange), labels = c("Non-HU      ","HU")) +
  geom_text(aes(label = pct, fontface = 'bold'), position = position_fill(vjust = 0.5),
            size = 7.5, family = "Franklin Gothic Book",
            color = ifelse(data1$high_utilizer_10_pct == "No", "black", "white")) +
  theme_axes +
  theme(legend.position = "top",
        legend.justification = c(0, 0),
        legend.title=element_blank(),
        axis.title.y = element_blank())

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Table showing proportion of PC holds that are 10% high utilizers

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

data1total <- bookings_entrances %>%
  filter(county != "Coos" & county != "Strafford") %>%
  select(county,
         fy,
         id,
         booking_id,
         high_utilizer_1_pct,
         high_utilizer_5_pct,
         high_utilizer_10_pct,
         pc_hold_in_booking) %>%
  distinct() %>%
  filter(!is.na(pc_hold_in_booking)) %>%
  group_by(pc_hold_in_booking, high_utilizer_10_pct) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  filter(high_utilizer_10_pct == "Yes") %>%
  filter(pc_hold_in_booking == "PC Hold") %>%
  select(total_hu_pc_holds = total) %>%
  mutate(county = "State")

# Select variables
data1 <- bookings_entrances %>%
  filter(county != "Coos" & county != "Strafford") %>%
  select(county,
         fy,
         id,
         booking_id,
         high_utilizer_1_pct,
         high_utilizer_5_pct,
         high_utilizer_10_pct,
         pc_hold_in_booking) %>%
  distinct() %>%
  filter(!is.na(pc_hold_in_booking)) %>%
  group_by(county, pc_hold_in_booking, high_utilizer_10_pct) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  filter(high_utilizer_10_pct == "Yes") %>%
  filter(pc_hold_in_booking == "PC Hold") %>%
  select(county, total_hu_pc_holds = total)
data1 <- rbind(data1, data1total)

data2 <- df_entrances_with_pc_holds %>% select(county, total_pc_holds) %>%
  left_join(data1, by = "county") %>%
  mutate(freq = total_hu_pc_holds/total_pc_holds)

PRES_table_pc_hold_hus <-
  reactable(data2,
           pagination = FALSE,
           style = list(fontFamily = "Franklin Gothic Book", fontSize = "1.0rem"),
           rowStyle = function(index) {
             if (index %in% c(10)) {
               list(`border-top` = "thin solid",
                    fontWeight = "bold")
             }
           },
           theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"),
                                  headerStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
           defaultColDef = reactable::colDef(
             format = colFormat(separators = TRUE), align = "center"),
           compact = TRUE,
           fullWidth = FALSE,
           columns = list(
             county               = colDef(show = T, minWidth = 190, name = "County", align = "left",
                                           style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),
             total_pc_holds       = colDef(show = T, minWidth = 120, name = "Total PC Holds (HU's and non-HU's)"),
             total_hu_pc_holds    = colDef(show = T, minWidth = 120, name = "PC Holds (HU's only)"),

             freq = colDef(show = T, minWidth = 120, name = "Proportion of HU PC Holds",
                                  style = list(fontWeight = "bold"),
                                  format = colFormat(percent = TRUE, digits = 1))
           ))


# Select variables
data1 <- bookings_entrances %>%
  select(county,
         fy,
         id,
         booking_id,
         high_utilizer_1_pct,
         high_utilizer_5_pct,
         high_utilizer_10_pct,
         pc_hold_in_booking) %>%
  distinct() %>%
  #filter(!is.na(pc_hold_in_booking)) %>%
  group_by(county, pc_hold_in_booking, high_utilizer_10_pct) %>%
  summarise(total = n()) %>%
  filter(pc_hold_in_booking == "PC Hold")
data1 <- group_by(data1, county) %>% mutate(pct = round(total/sum(total)*100, 1))
data1 <- as.data.frame(data1)
data1 <- data1 %>% mutate(pct = round(pct, digits = 0)) %>%
  mutate(pct = paste0(pct, "%"))

PRES_gg_pchold_hu_prop <- ggplot(data1, aes(x = county, y = total, fill = high_utilizer_10_pct )) +
  geom_col(colour = "white", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("gray",jri_orange),
                    na.value = "white",
                    labels = c("Non-HU      ","HU", " ")) +
  geom_text(aes(label = pct, fontface = 'bold'),
            position = position_fill(vjust = 0.5),
            vjust = 0.7,
            size = 10, family = "Franklin Gothic Book",
            color = case_when(data1$high_utilizer_10_pct  == "No" ~ "black",
                              TRUE ~ "white")
            ) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.85, size = 28, color = "black"),
        axis.text.y = element_text(size = 28, color = "black"),
        legend.position = "right",
        legend.justification = c(1, 0.5),
        legend.title=element_blank(),
        legend.text = element_text(family = "Franklin Gothic Book", size = 28, color = "black"))
  # geom_hline(aes(yintercept=0.20), size = 1)
PRES_gg_pchold_hu_prop

ggsave(PRES_gg_pchold_hu_prop, file=paste0(sp_data_path, "/Data/r_data/PRES_gg_pchold_prop.png", sep = ""),
       width = 15, height = 6, dpi = 100)

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Proportion of HU entrances that are PC holds

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Select variables
df_hu_pc_holds <- bookings_entrances %>%
  filter(county != "Coos" & county != "Strafford") %>%
  select(county,
         fy,
         id,
         booking_id,
         high_utilizer_1_pct,
         high_utilizer_5_pct,
         high_utilizer_10_pct,
         pc_hold_in_booking) %>%
  distinct() %>%
  filter(!is.na(pc_hold_in_booking))

# Get entrances of high utilizers
df_hu_pc_holds_fy_4_times <- df_hu_pc_holds %>% filter(high_utilizer_10_pct == "Yes") %>% group_by(fy, pc_hold_in_booking) %>% summarise(total = n())
df_hu_pc_holds_fy_1_pct   <- df_hu_pc_holds %>% filter(high_utilizer_1_pct   == "Yes") %>% group_by(fy, pc_hold_in_booking) %>% summarise(total = n())
df_hu_pc_holds_fy_5_pct   <- df_hu_pc_holds %>% filter(high_utilizer_5_pct   == "Yes") %>% group_by(fy, pc_hold_in_booking) %>% summarise(total = n())
df_hu_pc_holds_fy_10_pct  <- df_hu_pc_holds %>% filter(high_utilizer_10_pct  == "Yes") %>% group_by(fy, pc_hold_in_booking) %>% summarise(total = n())

# gg plots of proportion of HU entrances that are PC holds by FY
gg_hu_pc_holds_fy_4_times <- fnc_pct_grouped_bar_chart(df_hu_pc_holds_fy_4_times, "gray", jri_dark_blue)
gg_hu_pc_holds_fy_1_pct   <- fnc_pct_grouped_bar_chart(df_hu_pc_holds_fy_1_pct,   "gray", jri_light_blue)
gg_hu_pc_holds_fy_5_pct   <- fnc_pct_grouped_bar_chart(df_hu_pc_holds_fy_5_pct,   "gray", jri_green)
gg_hu_pc_holds_fy_10_pct  <- fnc_pct_grouped_bar_chart(df_hu_pc_holds_fy_10_pct,  "gray", jri_orange)

################################################################################

# Histogram showing LOS for high utilizers for BOOKINGS not entrances

################################################################################

df_los_no_pc_hold <- booking_no_pc_hold %>%
  select(fy,
         county,
         id,
         los,
         los_category,
         high_utilizer_1_pct,
         high_utilizer_5_pct,
         high_utilizer_10_pct) %>%
  distinct() %>%
  filter(!is.na(los))

df_los_10_pct <- df_los_no_pc_hold %>%
  filter(high_utilizer_10_pct == "Yes") %>%
  group_by(los_category) %>%
  summarise(total = n()) %>%
  mutate(hu = "HU (Top 10%)")

gg_hu_los_category_count <- ggplot(df_los_10_pct, aes(los_category, total)) +
  geom_bar(aes(fill = hu), position = "dodge", stat="identity", width = 0.75) +
  scale_fill_manual(values=c(jri_orange),
                    labels = c("HU (Top 10%)")) +
  scale_y_continuous(labels = label_number(big.mark = ","),
                     limits = c(0,2000)) +
  xlab("Length of Stay (Days)") + ylab("Number of Entrances\n") +
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
pct_hu_los_between_0_10_days <- df_los_no_pc_hold %>% group_by(los_category) %>%
  filter(high_utilizer_10_pct == "Yes") %>%
  summarise(total = n()) %>%
  mutate(pct = round(total/sum(total)*100, 1)) %>%
  mutate(los_category = as.character(los_category))
pct_hu_los_between_0_10_days = pct_hu_los_between_0_10_days[-c(8:12),]
pct_hu_los_between_0_10_days <- sum(pct_hu_los_between_0_10_days$pct)

# get % of bookings that are 0-1 days
pct_hu_los_between_0_1_days <- df_los_no_pc_hold %>% group_by(los_category) %>%
  filter(high_utilizer_10_pct == "Yes") %>%
  summarise(total = n()) %>%
  mutate(pct = round(total/sum(total)*100, 1)) %>%
  mutate(los_category = as.character(los_category))
pct_hu_los_between_0_1_days = pct_hu_los_between_0_1_days[-c(4:12),]
pct_hu_los_between_0_1_days <- sum(pct_hu_los_between_0_1_days$pct)

################################################################################

# Save graphs and tables

################################################################################

ggsave(gg_hu_pc_holds_fy_4_times, file=paste0(sp_data_path, "/Data/r_data/gg_hu_pc_holds_fy_4_times.png", sep = ""),
       width = 4.5, height = 4.2, dpi = 100)
ggsave(gg_hu_pc_holds_fy_1_pct, file=paste0(sp_data_path, "/Data/r_data/gg_hu_pc_holds_fy_1_pct.png", sep = ""),
       width = 4.5, height = 4.2, dpi = 100)
ggsave(gg_hu_pc_holds_fy_5_pct, file=paste0(sp_data_path, "/Data/r_data/gg_hu_pc_holds_fy_5_pct.png", sep = ""),
       width = 4.5, height = 4.2, dpi = 100)
ggsave(gg_hu_pc_holds_fy_10_pct, file=paste0(sp_data_path, "/Data/r_data/gg_hu_pc_holds_fy_10_pct.png", sep = ""),
       width = 4.5, height = 4.2, dpi = 100)

##########

# Save data

##########

save(table_hu_4_times_summary, file=paste0(sp_data_path, "/Data/r_data/table_hu_4_times_summary.Rda", sep = ""))
save(table_hu_1_pct_summary, file=paste0(sp_data_path, "/Data/r_data/table_hu_1_pct_summary.Rda", sep = ""))
save(table_hu_5_pct_summary, file=paste0(sp_data_path, "/Data/r_data/table_hu_5_pct_summary.Rda", sep = ""))
save(table_hu_10_pct_summary, file=paste0(sp_data_path, "/Data/r_data/table_hu_10_pct_summary.Rda", sep = ""))
save(PRES_hu_summary, file=paste0(sp_data_path, "/Data/r_data/PRES_hu_summary.Rda", sep = ""))
save(PRES_hu_summary1, file=paste0(sp_data_path, "/Data/r_data/PRES_hu_summary1.Rda", sep = ""))
