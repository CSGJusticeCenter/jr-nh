############################################
# Project: JRI New Hampshire
# File:  high_utilizers_4_times.R
# Last updated: October 12, 2022
# Author: Mari Roberts

# High Utilizers Based on Jail Bookings by State
# Explore HU's defined as those booked 4 or more times in a FY
# Also Explore HU's defined as 1%, 5%, and 10%

# Tables, graphs, and numbers for high utilizers analysis page
############################################

################################################################################

# Min med mean max df for bookings and entrances of HU's by county
# All  - county, total entrances, avg entrances/FY,
# HU's - total hu entrances, avg HU entrances/FY, mean, min, max, proportion of entrances that are HU entrances

################################################################################

# df for table
df_hu_4_times_summary <- fnc_hus_descriptive_summary(bookings_entrances, "high_utilizer_4_times", "Yes", "Coos (bookings only)")

# # reactable table
# table_hu_4_times_summary <- fnc_reactable_summary(df_hu_4_times_summary, "HU",
#                                                   total1  = "Entrances",
#                                                   total2_name = "test",
#                                                   freq1_name = "Proportion",
#                                                   mean1_name = "AVG",
#                                                   max1_name = "MAX")

################################################################################

# Propotion of HU entrances out of all entrances over time

################################################################################

df_hu_4_times <- bookings_entrances %>%
  select(fy,
         county,
         id,
         booking_id,
         num_bookings,
         high_utilizer_4_times,
         month_year_text,
         month_year
         ) %>%
  distinct()

# table(df_hu_4_times$high_utilizer_4_times) about 22.3% of entrances are HU's

df1 <- df_hu_4_times %>%
  group_by(fy, high_utilizer_4_times) %>%
  summarise(total = n())

df2 <- group_by(df1, fy) %>%
  mutate(pct = round(total/sum(total)*100, 1))
df2 <- as.data.frame(df2)
df2 <- df2 %>% mutate(pct = round(pct, 1))

gg_hu_4_times_fy <- ggplot(df2, aes(x = fy, y = total, fill = high_utilizer_4_times)) +
  geom_col(colour = NA, position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("gray", jri_orange), labels = c("Non-HU      ","HU")) +
  geom_text(data=subset(df2, pct > 3), aes(label = paste0(pct, "%"), fontface = 'bold'), position = position_fill(vjust = 0.5),
            size = 7.5, family = "Franklin Gothic Book",
            color = ifelse(df2$high_utilizer_4_times == "No", "black", "white")) +
  theme_axes +
  theme(legend.position = "top",
        legend.justification = c(0, 0),
        legend.title=element_blank(),
        axis.title.y = element_blank())

################################################################################

# Area chart showing the prop of HU entrances over time
# Doesn't look very good

################################################################################

# plot1 <- ggplot(df1) +
#   geom_area(aes(fy, total, fill = high_utilizer_4_times), color = "white") +
#   scale_fill_manual(values=c("gray",jri_orange), labels = c("Non-HU      ","HU")) +
#   scale_x_continuous(
#     limits = c(2018.8, 2021.8),
#     expand = c(0, 0),
#     breaks = c(2019, 2020, 2021),
#     labels = c("2019", "2020", "2021")
#   ) +
#   scale_y_continuous(
#     labels=comma,
#     limits = c(0, 22000),
#     breaks = seq(0, 22000, by = 5000),
#     expand = c(.05, 0)
#   ) +
#   theme_axes +
#   theme(legend.position = "top",
#         legend.justification = c(0, 0),
#         legend.title=element_blank(),
#         axis.title.y = element_blank())
# plot1


################################################################################

# Stacked bar chart showing the prop of HU entrances over time

################################################################################

df3 <- merge(df1, df2, by = c("fy", "high_utilizer_4_times", "total"))

gg_hu_entrance_prop_amount_fy <- ggplot(df3, aes(x = fy, y = total, fill = high_utilizer_4_times)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("gray",jri_orange), labels = c("Non-HU      ","HU")) +
  scale_y_continuous(
    labels=comma,
    limits = c(0, 22000),
    breaks = seq(0, 22000, by = 5000),
    expand = c(.05, 0)
  ) +
  theme_axes +
  theme(legend.position = "top",
        legend.justification = c(0, 0),
        legend.title=element_blank(),
        axis.title.y = element_blank())


################################################################################

# Calculate change in HU's over time

################################################################################

df_hu_entrances_fy <- df1 %>% filter(high_utilizer_4_times == "Yes")

# save changes between years to call in sentences in incarceration patterns rmd
v1 <- df_hu_entrances_fy %>% ungroup() %>% filter(fy == 2019) %>% select(total); v1 <- as.numeric(v1)
v2 <- df_hu_entrances_fy %>% ungroup() %>% filter(fy == 2020) %>% select(total); v1 <- as.numeric(v1)
v3 <- df_hu_entrances_fy %>% ungroup() %>% filter(fy == 2021) %>% select(total); v1 <- as.numeric(v1)
change_19_20_hu_entrances <- (v2 - v1)/v1
change_19_20_hu_entrances <- round(change_19_20_hu_entrances*100, 1)
change_20_21_hu_entrances <- (v3 - v2)/v2
change_20_21_hu_entrances <- round(change_20_21_hu_entrances*100, 1)
change_19_21_hu_entrances <- (v3 - v1)/v1
change_19_21_hu_entrances <- round(change_19_21_hu_entrances*100, 1)

################################################################################

# reactable table of number of HU entrances by FY and county and the change from 2019 to 2021

################################################################################

# data table of number of entrances by FY and county
df_hu_entrances_county <- bookings_entrances %>%
  filter(high_utilizer_4_times == "Yes") %>%
  select(id, booking_id, fy, county) %>%
  distinct() %>%
  group_by(fy, county) %>%
  dplyr::summarise(total = n()) %>%
  spread(fy, total) %>%
  mutate(`2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`),
         `2021` = as.numeric(`2021`)) %>%
  mutate(total = `2019` + `2020` + `2021`) %>%
  adorn_totals("row") %>%
  mutate(change_19_21 = (`2021`-`2019`)/`2019`) %>%
  mutate(county = case_when(county == "Coos" ~ "Coos (bookings only)", TRUE ~ county))

# reactable table of number of HU entrances by FY and county and the change from 2019 to 2021
table_hu_entrances_fy_county <- fnc_reactable_county_fy(df_hu_entrances_county)
# save_reactable(iris_table, file = paste0(sp_data_path, "/Data/r_data/table_hu_entrances_fy_county.png", sep = ""))

################################################################################

# Line graph showing HU entrances

################################################################################

df1 <- df_hu_4_times %>% filter(high_utilizer_4_times == "Yes")
temp <- fnc_covid_time_highchart(df1, "", "", jri_orange)

################################################################################

# Proportion of HU entrances that are PC holds

################################################################################

# select variables
df_hu_pc_holds <- bookings_entrances %>%
  filter(county != "Coos" & county != "Strafford") %>%
  select(county, fy, booking_id, high_utilizer_4_times, high_utilizer_1_pct, high_utilizer_5_pct, high_utilizer_10_pct, pc_hold_in_booking) %>%
  distinct() %>%
  filter(!is.na(pc_hold_in_booking))

# get entrances of high utilizers
df_hu_pc_holds_fy_4_times <- df_hu_pc_holds %>% filter(high_utilizer_4_times == "Yes") %>% group_by(fy, pc_hold_in_booking) %>% summarise(total = n())
df_hu_pc_holds_fy_1_pct   <- df_hu_pc_holds %>% filter(high_utilizer_1_pct   == "Yes") %>% group_by(fy, pc_hold_in_booking) %>% summarise(total = n())
df_hu_pc_holds_fy_5_pct   <- df_hu_pc_holds %>% filter(high_utilizer_5_pct   == "Yes") %>% group_by(fy, pc_hold_in_booking) %>% summarise(total = n())
df_hu_pc_holds_fy_10_pct  <- df_hu_pc_holds %>% filter(high_utilizer_10_pct  == "Yes") %>% group_by(fy, pc_hold_in_booking) %>% summarise(total = n())

# gg plots of proportion of entrances that are PC holds by FY
gg_hu_pc_holds_fy_4_times <- fnc_pct_grouped_bar_chart(df_hu_pc_holds_fy_4_times, "gray", jri_dark_blue)
gg_hu_pc_holds_fy_1_pct   <- fnc_pct_grouped_bar_chart(df_hu_pc_holds_fy_1_pct,   "gray", jri_light_blue)
gg_hu_pc_holds_fy_5_pct   <- fnc_pct_grouped_bar_chart(df_hu_pc_holds_fy_5_pct,   "gray", jri_green)
gg_hu_pc_holds_fy_10_pct  <- fnc_pct_grouped_bar_chart(df_hu_pc_holds_fy_10_pct,  "gray", jri_orange)

################################################################################

# Histogram showing LOS for high utilizers

################################################################################

df_los_4_times <- df_los_no_pc_hold %>%
  filter(high_utilizer_4_times == "Yes") %>%
  group_by(los_category) %>%
  summarise(total = n()) %>%
  mutate(hu = "HU (entered 3 or more times)")

gg_hu_los_category_count <- ggplot(df_los_4_times, aes(los_category, total)) +
  geom_bar(aes(fill = hu), position = "dodge", stat="identity", width = 0.75) +
  scale_fill_manual(values=c(jri_orange),
                    labels = c("HU (3 or more entrances per FY)")) +
  scale_y_continuous(labels = label_number(big.mark = ","),
                     limits = c(0,6000)) +
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
  filter(high_utilizer_4_times == "Yes") %>%
  summarise(total = n()) %>%
  mutate(pct = round(total/sum(total)*100, 1)) %>%
  mutate(los_category = as.character(los_category))
pct_hu_los_between_0_10_days = pct_hu_los_between_0_10_days[-c(6:10),]
pct_hu_los_between_0_10_days <- sum(pct_hu_los_between_0_10_days$pct)

# get % of bookings that are 0-1 days
pct_hu_los_between_0_1_days <- df_los_no_pc_hold %>% group_by(los_category) %>%
  filter(high_utilizer_4_times == "Yes") %>%
  summarise(total = n()) %>%
  mutate(pct = round(total/sum(total)*100, 1)) %>%
  mutate(los_category = as.character(los_category))
pct_hu_los_between_0_1_days = pct_hu_los_between_0_1_days[-c(3:10),]
pct_hu_los_between_0_1_days <- sum(pct_hu_los_between_0_1_days$pct)

################################################################################

# Histogram showing LOS for NON high utilizers

################################################################################

df_los_4_times_new <- df_los_no_pc_hold %>%
  filter(high_utilizer_4_times == "No") %>%
  group_by(los_category) %>%
  summarise(total = n()) %>%
  mutate(hu = "Non-HU")

gg_non_hu_los_category_count <- ggplot(df_los_4_times_new, aes(los_category, total)) +
  geom_bar(aes(fill = hu), position = "dodge", stat="identity", width = 0.75) +
  scale_fill_manual(values=c(jri_green),
                    labels = c("Non-HU")) +
  scale_y_continuous(labels = label_number(big.mark = ","),
                     limits = c(0,6000)) +
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
#
# # get % of bookings that are 0-10 days
# pct_non_hu_los_between_0_10_days <- df_los_no_pc_hold %>% group_by(los_category) %>%
#   filter(high_utilizer_4_times == "No") %>%
#   summarise(total = n()) %>%
#   mutate(pct = round(total/sum(total)*100, 1)) %>%
#   mutate(los_category = as.character(los_category))
# pct_non_hu_los_between_0_10_days = pct_non_hu_los_between_0_10_days[-c(6:10),]
# pct_non_hu_los_between_0_10_days <- sum(pct_non_hu_los_between_0_10_days$pct)
#
# # get % of bookings that are 0-1 days
# pct_non_hu_los_between_0_1_days <- df_los_no_pc_hold %>% group_by(los_category) %>%
#   filter(high_utilizer_4_times == "No") %>%
#   summarise(total = n()) %>%
#   mutate(pct = round(total/sum(total)*100, 1)) %>%
#   mutate(los_category = as.character(los_category))
# pct_non_hu_los_between_0_1_days = pct_non_hu_los_between_0_1_days[-c(3:10),]
# pct_non_hu_los_between_0_1_days <- sum(pct_non_hu_los_between_0_1_days$pct)
#

# combine into grouped barplot
df_los_4_times_new <- df_los_no_pc_hold %>%
  group_by(los_category, high_utilizer_4_times) %>%
  summarise(total = n())

ggplot(df_los_4_times_new, aes(fill=high_utilizer_4_times, y=total, x=los_category)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c(jri_green),
                    labels = c("Non-HU")) +
  scale_y_continuous(labels = label_number(big.mark = ","),
                     limits = c(0,6000)) +
  xlab("Length of Stay (Days)") + ylab("Number of Entrances\n") +
  theme_minimal(base_family = "Franklin Gothic Book") +
  theme_axes +
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

################################################################################

# Save graphs and tables

################################################################################

ggsave(gg_hu_4_times_fy, file=paste0(sp_data_path, "/Data/r_data/gg_hu_4_times_fy.png", sep = ""),
       width = 4.5, height = 4.2, dpi = 100)
ggsave(gg_hu_entrance_prop_amount_fy, file=paste0(sp_data_path, "/Data/r_data/gg_hu_entrance_prop_amount_fy.png", sep = ""),
       width = 5.5, height = 5.2, dpi = 100)

ggsave(gg_hu_pc_holds_fy_4_times, file=paste0(sp_data_path, "/Data/r_data/gg_hu_pc_holds_fy_4_times.png", sep = ""),
       width = 4.5, height = 4.2, dpi = 100)
ggsave(gg_hu_pc_holds_fy_1_pct, file=paste0(sp_data_path, "/Data/r_data/gg_hu_pc_holds_fy_1_pct.png", sep = ""),
       width = 4.5, height = 4.2, dpi = 100)
ggsave(gg_hu_pc_holds_fy_5_pct, file=paste0(sp_data_path, "/Data/r_data/gg_hu_pc_holds_fy_5_pct.png", sep = ""),
       width = 4.5, height = 4.2, dpi = 100)
ggsave(gg_hu_pc_holds_fy_10_pct, file=paste0(sp_data_path, "/Data/r_data/gg_hu_pc_holds_fy_10_pct.png", sep = ""),
       width = 4.5, height = 4.2, dpi = 100)

ggsave(gg_los_category_by_hu, file=paste0(sp_data_path, "/Data/r_data/gg_los_category_by_hu.png", sep = ""),
       width = 7, height = 5.2, dpi = 100)

