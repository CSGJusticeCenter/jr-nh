############################################
# Project: JRI New Hampshire
# File: los.R
# Last updated: October 19, 2022
# Author: Mari Roberts

# Tables, graphs, and numbers for los
############################################

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Reactable table for LOS for entrances (including Coos bookings and Strafford)
# Includes HU's and non-HU's

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Count freq of los for entrances (including Coos bookings and Strafford)
df_los_entrances <- bookings_entrances %>%
  select(fy,
         county,
         id,
         booking_id,
         los,
         los_category,
         high_utilizer_4_times,
         high_utilizer_1_pct,
         high_utilizer_5_pct,
         high_utilizer_10_pct) %>%
  distinct() %>%
  filter(!is.na(los))
# dim(df_los_entrances); length(unique(df_los_entrances$booking_id)) # 51349, 51349 will be less than total entrances because of NAs in LOS's

# Average lOS for all entrances
avg_los_entrances <- df_los_entrances %>%
  group_by() %>%
  dplyr::summarize(mean = mean(los, na.rm=TRUE))
avg_los_entrances <- as.numeric(avg_los_entrances)

# Df showing summary for LOS for entrances
df_los_entrances_summary <- df_los_entrances %>%
  group_by() %>%
  summarise(
    total  = n(),
    min    = min(los, na.rm = T),
    median = median(los, na.rm = T),
    mean   = mean(c(los, na.rm = T)),
    max    = max(los, na.rm = T)
  ) %>%
  mutate(mean = round(mean, 1)) # 28.3
df_los_entrances_summary <- df_los_entrances_summary %>% mutate(type = "All (HU's and non-HU's)") %>% select(type, everything())

row_los_entrances_summary <- reactable(df_los_entrances_summary,
                                       pagination = FALSE,
                                       style = list(fontFamily = "Franklin Gothic Book", fontSize = "1.0rem"),
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


################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Reactable table for LOS for entrances (including Coos bookings and Strafford)
# For HU's only

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Average lOS for entrances for 10% HU's
avg_los_entrances <- df_los_entrances %>%
  filter(high_utilizer_10_pct == "Yes") %>%
  group_by() %>%
  dplyr::summarize(mean = mean(los, na.rm=TRUE))
avg_los_entrances <- as.numeric(avg_los_entrances)

# overall LOS for entrances for 10% HU's
df_los_entrances_summary <- df_los_entrances %>%
  filter(high_utilizer_10_pct == "Yes") %>%
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

row_los_entrances_summary <- reactable(df_los_entrances_summary,
                                       pagination = FALSE,
                                       style = list(fontFamily = "Franklin Gothic Book", fontSize = "1.0rem"),
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
row_los_entrances_summary

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# Ggplot showing histogram of LOS

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

df_los_1_pct <- df_los_entrances %>%
  filter(high_utilizer_1_pct == "Yes") %>% group_by(los_category) %>% summarise(total = n()) %>%
  mutate(hu = "Top 1%")
df_los_5_pct <- df_los_entrances %>%
  filter(high_utilizer_5_pct == "Yes") %>% group_by(los_category) %>% summarise(total = n()) %>%
  mutate(hu = "Top 5%")
df_los_10_pct <- df_los_entrances %>%
  filter(high_utilizer_10_pct == "Yes") %>% group_by(los_category) %>% summarise(total = n()) %>%
  mutate(hu = "Top 10%")
df_los_pct <- rbind(df_los_1_pct, df_los_5_pct, df_los_10_pct)
df_los_pct$hu <- factor(df_los_pct$hu, levels = c("Top 1%", "Top 5%", "Top 10%"))

# Grouped ggplot with all HU categories
gg_los_category_by_hu <- ggplot(df_los_pct, aes(los_category, total)) +
  geom_bar(aes(fill = hu), position = "dodge", stat="identity", width = 0.75) +
  scale_fill_manual(values=c(jri_light_blue, jri_green, jri_orange),
                    labels = c("Top 1%      ","Top 5%      ", "Top 10%")) +
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

# redo LOS categories just for this graph
data1 <- df_los_entrances %>%
  mutate(los_category =
           case_when(los == 0 ~ "0",
                     los == 1 ~ "1",
                     los == 2 ~ "2",
                     los == 3 ~ "3",
                     los == 4 ~ "4",
                     los == 5 ~ "5",
                     los == 6 ~ "6",
                     los == 7 ~ "7",
                     los == 8 ~ "8",
                     los == 9 ~ "9",
                     los >= 10  & los <= 15  ~ "10-15",
                     los >= 16  & los <= 20  ~ "16-20",
                     los >= 21  & los <= 25  ~ "21-25",
                     los >= 26  & los <= 30  ~ "26-30",
                     los >= 31  & los <= 50  ~ "31-50",
                     los >= 51  & los <= 100 ~ "51-100",
                     los >= 101 & los <= 180 ~ "101-180",
                     los >  180              ~ "181-994")) %>% # 944 is the most LOS for 10pct HU's
  mutate(los_category = ordered(los_category,
                                levels = c("0",
                                           "1",
                                           "2",
                                           "3",
                                           "4",
                                           "5",
                                           "6",
                                           "7",
                                           "8",
                                           "9",
                                           "10-15",
                                           "16-20",
                                           "21-25",
                                           "26-30",
                                           "31-50",
                                           "51-100",
                                           "101-180",
                                           "181-994"))) %>%
  filter(high_utilizer_10_pct == "Yes") %>%
  group_by(los_category) %>%
  summarise(total = n()) %>%
  mutate(hu = "Top 10%") %>%
  filter(!is.na(los_category))

# Average los for entrances
avg_los_entrances_10_pct <- df_los_entrances %>%
  filter(high_utilizer_10_pct == "Yes") %>%
  group_by() %>%
  dplyr::summarize(mean = mean(los, na.rm=TRUE))
avg_los_entrances_10_pct <- as.numeric(avg_los_entrances_10_pct)

# Median los for entrances
median_los_entrances_10_pct <- df_los_entrances %>%
  filter(high_utilizer_10_pct == "Yes") %>%
  group_by() %>%
  dplyr::summarize(median = median(los, na.rm=TRUE))
median_los_entrances_10_pct <- as.numeric(median_los_entrances_10_pct)

# Maximum los for entrances
maximum_los_entrances_10_pct <- df_los_entrances %>%
  filter(high_utilizer_10_pct == "Yes") %>%
  group_by() %>%
  dplyr::summarize(maximum = max(los, na.rm=TRUE))
maximum_los_entrances_10_pct <- as.numeric(maximum_los_entrances_10_pct)

# NOTE: Manually adds median and mean to discrete scale. Make sure this is accurate each time run.
PRES_gg_los_10_pct <- ggplot(data1, aes(x = los_category, y = total, fill = hu)) +

  geom_bar(aes(x = los_category, y = total, fill = hu), stat="identity", width = 0.75) +
  labs(x = "Length of Stay (Days)", y = "Number of People\n") +

  # Median
  geom_vline(xintercept = 3, linetype = "dashed", colour = "darkgray",size = 1) +
  geom_richtext(aes(x=3, y=3400, label="Median = 2"),
                color = "white",
                size = 7.5,
                fontface = 'bold',
                fill = "darkgray",
                label.r = unit(0, "lines"),
                label.padding = unit(c(.5, .5, .5, .5), "lines"),
                nudge_x = 1,
                #angle = 90
                ) +

  # Mean
  geom_vline(xintercept = 12, linetype = "dashed", colour = "darkgray",size = 1) +
  geom_richtext(aes(x=12, y=3400, label="Average = 22"),
                color = "white",
                size = 7.5,
                fontface = 'bold',
                fill = "darkgray",
                label.r = unit(0, "lines"),
                label.padding = unit(c(.5, .5, .5, .5), "lines"),
                nudge_x = 1.1,
                #angle = 90
                ) +
  scale_fill_manual(values=c(jri_orange)) +
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3, big.mark = ","),
                     expand = c(.1,0),
                     breaks = seq(0, 5000, by = 1000),
                     limits = c(0,5000)) +

  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,size = 22, color = "black",
                                   hjust=1),
        axis.text.y = element_text(size = 22, color = "black"),
        axis.title = element_text(color = "black"),
        axis.title.y = element_text(size = 22, color = "black"),
        axis.title.x = element_text(size = 22, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "top",
        legend.justification = c(0, 0),
        legend.title=element_blank(),
        legend.text = element_text(family = "Franklin Gothic Book", size = 22, color = "black")
  )

# Get % of bookings that are 0-10 days
pct_los_between_0_10_days <- df_los_entrances %>%
  filter(high_utilizer_10_pct == "Yes") %>%
  group_by(los_category) %>%
  summarise(total = n()) %>%
  mutate(pct = round(total/sum(total)*100, 1)) %>%
  mutate(los_category = as.character(los_category))
pct_los_between_0_10_days = pct_los_between_0_10_days[-c(8:12),]
pct_los_between_0_10_days <- sum(pct_los_between_0_10_days$pct)

# Get % of bookings that are 0-1 days
pct_los_between_0_1_days <- df_los_entrances %>%
  filter(high_utilizer_10_pct == "Yes") %>%
  group_by(los_category) %>%
  summarise(total = n()) %>%
  mutate(pct = round(total/sum(total)*100, 1)) %>%
  mutate(los_category = as.character(los_category))
pct_los_between_0_1_days = pct_los_between_0_1_days[-c(3:12),]
pct_los_between_0_1_days <- sum(pct_los_between_0_1_days$pct)
