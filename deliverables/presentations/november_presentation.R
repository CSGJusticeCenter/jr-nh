############################################
# Project: JRI New Hampshire
# File: november_presentation.R
# Last updated: October 4, 2022
# Author: Mari Roberts

# Create powerpoint slides with editable graphs
############################################

source("data_cleaning/12_dataframes.R")
source("data_cleaning/13_incarceration_patterns.R")


##########
# ggplot bar chart showing the number of bookings by FY - in incarceration_patterns.R
##########

df_bookings_long <- df_bookings %>% select(-total)
df_bookings_long <- gather(df_bookings_long, year, total, `2019`:`2021`, factor_key=TRUE)
df_bookings_long <- df_bookings_long %>% mutate(year = as.numeric(year)) %>%
  mutate(year = case_when(year == 1 ~ "2019", year == 2 ~ "2020", year == 3 ~ "2021"))

theme_set(theme_no_axes)
nh_bookings_barchart_gg <-
  ggplot(data=df_bookings_long, aes(x=year, y=total)) +
  geom_bar(stat="identity", width = 0.74, fill = jri_dark_blue) +
  xlab("") + ylab("Number of Bookings") +
  geom_text(aes(label = comma(total)), color = "black", vjust = -1, size = 7.5, family = "Franklin Gothic Book") +
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3, big.mark = ","),
                     expand = c(0,0),
                     limits = c(0,24000))

# df_bookings_w_hus <- df_hu_bookings_table %>% select(fy, num_bookings_1_pct, num_bookings_3_pct, num_bookings_5_pct)
# df_bookings_w_hus <- gather(df_bookings_w_hus, variable_name , total, num_bookings_1_pct :num_bookings_5_pct , factor_key=TRUE)
# df_bookings_w_hus <- df_bookings_w_hus %>% filter(fy != "Total") %>% droplevels() %>%
#   select(variable_name, year = fy, everything())
# df_bookings_w_hus <- rbind(df_bookings_w_hus, df_bookings_long)
#
# theme_set(theme_axes)
# ggplot(data=df_bookings_w_hus, aes(x=year, y=total, group=variable_name)) +
#   geom_line()+
#   geom_point() +
#   scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3, big.mark = ","),
#                      expand = c(0,0),
#                      limits = c(0,24000))

df_nh_bookings_with_pc_holds <- df_nh_pc_holds_county %>%
  select(county, total_pc_holds = total, freq)
df_nh_bookings_county1 <- df_nh_bookings_county %>%
  select(county, total_bookings = total)
df_nh_bookings_with_pc_holds <- df_nh_bookings_with_pc_holds %>%
  left_join(df_nh_bookings_county1, by = "county") %>%
  select(county, total_bookings, everything()) %>%
  arrange(county)
df_nh_bookings_with_pc_holds <- df_nh_bookings_with_pc_holds %>%
  adorn_totals("row") %>%
  mutate(freq = case_when(county == "Total" ~ (filter(df, county=='Total')$total_pc_holds)/(filter(df, county=='Total')$total_bookings),
                          TRUE ~ freq)) %>%
  full_join(df_nh_bookings_county1, by = c("county", "total_bookings")) %>%
  arrange(county) %>%
  mutate(total_bookings = case_when(county == "Total" ~ sum(total_bookings) - filter(df, county=='Total')$total_bookings,
                          TRUE ~ total_bookings))

nh_bookings_with_pc_holds <-
  reactable(df_nh_bookings_with_pc_holds,
            pagination = FALSE,
            style = list(fontFamily = "Franklin Gothic Book"),
            theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
            compact = TRUE,
            fullWidth = FALSE,
            rowStyle = function(index) {
              if (index %in% c(10)) {
                list(`border-top` = "thin solid",
                     fontWeight = "bold")
              }
            },
            defaultColDef = reactable::colDef(
              format = colFormat(separators = TRUE), align = "left"),
            columns = list(
             county          = colDef(minWidth = 120, name = "County", style = list(fontWeight = "bold")),
             total_bookings  = colDef(minWidth = 80, name = "Total Bookings", align = "center"),
             total_pc_holds  = colDef(minWidth = 80, name = "# PC Holds", align = "center"),
             freq            = colDef(style = list(fontWeight = "bold"), name = "% PC Holds", format = colFormat(percent = TRUE, digits = 1), minWidth = 80, align = "center"))) %>%
  add_source("Coos and Strafford bookings were not included when calculating the proportion of bookings that are PC holds.", font_style = "italic", font_size = 14)

