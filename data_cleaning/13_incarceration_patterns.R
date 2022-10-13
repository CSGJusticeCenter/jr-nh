############################################
# Project: JRI New Hampshire
# File: incarceration_patterns.R
# Last updated: October 12, 2022
# Author: Mari Roberts

# Tables, graphs, and numbers for incarceration patterns page
# Focuses on bookings, entrances, booking types, and los

# Notes:
# Entrances = Bookings and PC holds
# Bookings  = Booked for a criminal charge
# A PC hold is technically not a booking
############################################

# save booking dates
all_booking_dates <- bookings_entrances %>%
  select(county, id, booking_id, booking_date, month_year_text, month_year, fy) %>%
  distinct()

################################################################################

# How many individual people were booked into New Hampshire jails annually?

# Use booking_no_pc_hold because it is bookings that do not include PC holds
# Does not include Strafford

################################################################################

####################

# by state

####################

# table of total number of people booked (no duplicates for counting by FY)
people_booked_total <- booking_no_pc_hold %>%
  dplyr::ungroup() %>%
  dplyr::select(id, county) %>%
  dplyr::distinct() %>%
  dplyr::group_by() %>%
  dplyr::summarise(total = n()) %>%
  dplyr::mutate(label = formatC(total, format="d", big.mark=","))

# count number of people booked total
amt_people_booked <- people_booked_total$total
amt_people_booked <- format(round(as.numeric(amt_people_booked), 0), nsmall=0, big.mark=",")

# table of total number of people booked by FY (some duplicates because it's by FY)
# transpose and format
df_people_booked_pre <- booking_no_pc_hold %>%
  dplyr::ungroup() %>%
  dplyr::select(id, fy, county) %>%
  dplyr::distinct() %>%
  dplyr::group_by(fy) %>%
  dplyr::summarise(total = n()) %>%
  dplyr::mutate(label = formatC(total, format="d", big.mark=","))

df_people_booked <- df_people_booked_pre %>%
  select(-label) %>%
  t %>% as.data.frame() %>%
  row_to_names(1) %>%
  mutate(`2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`),
         `2021` = as.numeric(`2021`)) %>%
  mutate(total = `2019` + `2020` + `2021`)
df_people_booked <- tibble::rownames_to_column(df_people_booked, "variable_name")
df_people_booked <- df_people_booked %>%
  mutate(variable_name = ifelse(variable_name == "total", "# People Booked into Jail", ""))

# count number of people booked for all three years but some double counting because it's by FY
amt_people_booked_fy_total <- df_people_booked$total
amt_people_booked_fy_total <- format(round(as.numeric(amt_people_booked_fy_total), 0), nsmall=0, big.mark=",")

##########

# create reactable table

##########

# one row table minimal showing number of people booked by FY and total
table_people_booked <-
  reactable(df_people_booked,
            pagination = FALSE,
            theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
            defaultColDef = reactable::colDef(format = colFormat(separators = TRUE), align = "center"),
            compact = TRUE,
            fullWidth = FALSE,
            columns = list(
              `variable_name` = colDef(minWidth = 225, name = ""),
              `2019`  = colDef(minWidth = 80, name = "2019"),
              `2020`  = colDef(minWidth = 80, name = "2020"),
              `2021`  = colDef(minWidth = 80, name = "2021",
                               style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
              `total` = colDef(minWidth = 80, name = "Total")))

##########

# highcharter bar chart of number of people booked by FY

##########

hc_people_booked <- df_people_booked_pre %>%
  hchart('column', hcaes(x = fy, y = total, color = jri_light_blue)) %>%
  hc_xAxis(title = list(text = "Fiscal Year", style = list(color =  "#000000", fontWeight = "bold"))) %>%
  hc_yAxis(title = list(text = "Number of People Booked", style = list(color =  "#000000", fontWeight = "bold"))) %>%
  hc_setup() %>%
  hc_add_theme(hc_theme_jc) %>%
  hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, format = "{point.label}")))

# save changes between years to call in sentences in incarceration patterns rmd
v1 <- as.numeric(filter(df_people_booked_pre, fy==2019) %>% select(total))
v2 <- as.numeric(filter(df_people_booked_pre, fy==2020) %>% select(total))
v3 <- as.numeric(filter(df_people_booked_pre, fy==2021) %>% select(total))
change_19_20_people_booked <- (v2 - v1)/v1
change_19_20_people_booked <- round(change_19_20_people_booked*100, 1)
change_20_21_people_booked <- (v3 - v2)/v2
change_20_21_people_booked <- round(change_20_21_people_booked*100, 1)
change_19_21_people_booked <- (v3 - v1)/v1
change_19_21_people_booked <- round(change_19_21_people_booked*100, 1)

####################

# by county

####################

# df of total number of people booked by FY
df_people_booked_county <- booking_no_pc_hold %>%
  dplyr::ungroup() %>%
  dplyr::select(id, fy, county) %>%
  dplyr::distinct() %>%
  dplyr::group_by(fy, county) %>%
  dplyr::summarise(total = n()) %>%
  spread(fy, total) %>%
  mutate(`2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`),
         `2021` = as.numeric(`2021`)) %>%
  mutate(total = `2019` + `2020` + `2021`)

##########

# Reactable table

##########

# table showing the number of people booked by FY by county
table_people_booked_fy_county <- reactable(df_people_booked_county,
                                     pagination = FALSE,
                                     theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
                                     defaultColDef = reactable::colDef(
                                       format = colFormat(separators = TRUE), align = "left",
                                       footer = function(values, name) {
                                         if (name %in% c("2019", "2020", "2021", "total")) {
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
                                     columns = list(
                                       `county` = colDef(footer = "Total",
                                                         minWidth = 120, name = "County"),
                                       `2019`  = colDef(minWidth = 80, name = "2019", align = "center"),
                                       `2020`  = colDef(minWidth = 80, name = "2020", align = "center"),
                                       `2021`  = colDef(minWidth = 80, name = "2021", align = "center",
                                                        style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                                       `total` = colDef(minWidth = 80, name = "Total", align = "center")))

##########

# ggplot bar chart showing the number of people booked by FY

##########

# data for ggplot showing the number of people booked by FY
df_people_booked_long <- df_people_booked %>% select(-total)
df_people_booked_long <- gather(df_people_booked, fy, total, `2019`:`2021`, factor_key=TRUE)
df_people_booked_long <- df_people_booked_long %>% mutate(fy = as.numeric(fy)) %>%
  mutate(fy = case_when(fy == 1 ~ "2019", fy == 2 ~ "2020", fy == 3 ~ "2021"))

# gg plot showing the number of people booked by FY
gg_people_booked <-
  ggplot(data=df_people_booked_long, aes(x=fy, y=total)) +
  geom_bar(stat="identity", width = 0.74, fill = jri_orange) +
  xlab("") + ylab("People Booked") +
  geom_text(aes(label = comma(total)), color = "black", vjust = -1, size = 7.5, family = "Franklin Gothic Book") +
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3, big.mark = ","),
                     expand = c(0,0),
                     limits = c(0,24000)) +
  theme_no_axes

################################################################################

# How bookings does NH have annually?

# Use booking_no_pc_hold because it is bookings that do not include PC holds
# Does not include Strafford

################################################################################

####################

# by state

####################

##########

# ggplot bar chart showing the number of bookings by FY

##########

# select variables
df_bookings_events <- booking_no_pc_hold %>%
  select(id, booking_id, fy, county) %>%
  distinct()

# calculate number of booking events per fy
df_bookings_events <- df_bookings_events %>%
  distinct() %>%
  group_by(fy) %>%
  dplyr::summarise(total = n()) %>%
  mutate(label = formatC(total, format="d", big.mark=","))

# transpose and format and save additional df
df_bookings <- df_bookings_events %>%
  select(-label) %>%
  t %>% as.data.frame() %>%
  row_to_names(1) %>%
  mutate(`2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`),
         `2021` = as.numeric(`2021`)) %>%
  mutate(total = `2019` + `2020` + `2021`)

# create table showing number of bookings by fiscal year
df_bookings <- tibble::rownames_to_column(df_bookings, "variable_name")
df_bookings <- df_bookings %>% mutate(variable_name = ifelse(variable_name == "total", "# of Bookings", ""))

# count number of bookings for three years
amt_bookings <- df_bookings$total
amt_bookings <- format(round(as.numeric(amt_bookings), 0), nsmall=0, big.mark=",")

##########

# Reactable table

##########

# one row minimal table showing the number of bookings by FY and total
table_bookings_fy <-
   reactable(df_bookings,
             pagination = FALSE,
             theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
             defaultColDef = reactable::colDef(format = colFormat(separators = TRUE), align = "center"),
             compact = TRUE,
             fullWidth = FALSE,
             columns = list(
               `variable_name` = colDef(minWidth = 225, name = ""),
               `2019`  = colDef(minWidth = 80, name = "2019"),
               `2020`  = colDef(minWidth = 80, name = "2020"),
               `2021`  = colDef(minWidth = 80, name = "2021",
                                style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
               `total` = colDef(minWidth = 80, name = "Total")))

##########

# # ggplot showing the number of bookings by FY

##########

# data for ggplot showing the number of bookings by FY
df_bookings_long <- df_bookings %>% select(-total)
df_bookings_long <- gather(df_bookings_long, fy, total, `2019`:`2021`, factor_key=TRUE) %>%
  mutate(fy = as.numeric(fy)) %>%
  mutate(fy = case_when(fy == 1 ~ "2019", fy == 2 ~ "2020", fy == 3 ~ "2021"))

# ggplot showing the number of bookings by FY
gg_bookings <-
  ggplot(data=df_bookings_long, aes(x=fy, y=total)) +
  geom_bar(stat="identity", width = 0.74, fill = jri_green) +
  xlab("") + ylab("Number of Bookings") +
  geom_text(aes(label = comma(total)), color = "black", vjust = -1, size = 7.5, family = "Franklin Gothic Book") +
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3, big.mark = ","),
                     expand = c(0,0),
                     limits = c(0,24000)) +
  theme_no_axes

##########

# Highchart bar chart showing the number of bookings by FY

##########

# highchart bar chart showing the number of bookings by FY
hc_bookings <- df_bookings_events %>%
  hchart('column', hcaes(x = fy, y = total, color = jri_light_blue)) %>%
  hc_xAxis(title = list(text = "Fiscal Year", style = list(color =  "#000000", fontWeight = "bold"))) %>%
  hc_yAxis(title = list(text = "Number of Bookings", style = list(color =  "#000000", fontWeight = "bold"))) %>%
  hc_setup() %>%
  hc_add_theme(hc_theme_jc) %>%
  hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, format = "{point.label}")))

# save changes between years to call in sentences in incarceration patterns rmd
v1 <- as.numeric(filter(df_bookings_events, fy==2019) %>% select(total))
v2 <- as.numeric(filter(df_bookings_events, fy==2020) %>% select(total))
v3 <- as.numeric(filter(df_bookings_events, fy==2021) %>% select(total))
change_19_20_bookings <- (v2 - v1)/v1
change_19_20_bookings <- round(change_19_20_bookings*100, 1)
change_20_21_bookings <- (v3 - v2)/v2
change_20_21_bookings <- round(change_20_21_bookings*100, 1)
change_19_21_bookings <- (v3 - v1)/v1
change_19_21_bookings <- round(change_19_21_bookings*100, 1)

################################################################################

# How many entrances does NH have annually?

# Use bookings_entrances which includes Coos and Strafford.
# Including Coos so we can get an actual picture of the number, even if its undershot

################################################################################

# total number of people booked (no duplicates for counting by FY)
df_people_entered_total <- bookings_entrances %>%
  ungroup() %>%
  dplyr::select(id, county) %>%
  dplyr::distinct() %>%
  dplyr::group_by() %>%
  dplyr::summarise(total = n()) %>%
  dplyr::mutate(label = formatC(total, format="d", big.mark=","))

##########

# Reactable table number of entrances per FY (including Coos)

##########

# select variables
df_entrances_events <- bookings_entrances %>%
  select(id, booking_id, fy, county) %>%
  distinct()

# calculate number of booking events per fy
df_entrances_events <- df_entrances_events %>%
  distinct() %>%
  group_by(fy) %>%
  dplyr::summarise(total = n()) %>%
  mutate(label = formatC(total, format="d", big.mark=","))

# transposed version
df_entrances <- df_entrances_events %>%
  select(-label) %>%
  t %>% as.data.frame() %>%
  row_to_names(1) %>%
  mutate(`2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`),
         `2021` = as.numeric(`2021`)) %>%
  mutate(total = `2019` + `2020` + `2021`)

# create df showing number of entrances by fiscal year
df_entrances <- tibble::rownames_to_column(df_entrances, "variable_name")
df_entrances <- df_entrances %>% mutate(variable_name = ifelse(variable_name == "total", "# of entrances", ""))

# one row minimal table showing the number of entrances (including Coos) each FY and total
table_entrances_fy <-
  reactable(df_entrances,
            pagination = FALSE,
            theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
            defaultColDef = reactable::colDef(format = colFormat(separators = TRUE), align = "center"),
            compact = TRUE,
            fullWidth = FALSE,
            columns = list(
              `variable_name` = colDef(minWidth = 225, name = ""),
              `2019`  = colDef(minWidth = 80, name = "2019"),
              `2020`  = colDef(minWidth = 80, name = "2020"),
              `2021`  = colDef(minWidth = 80, name = "2021",
                               style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
              `total` = colDef(minWidth = 80, name = "Total")))

# count number of entrances for three years
amt_entrances <- df_entrances$total
amt_entrances <- format(round(as.numeric(amt_entrances), 0), nsmall=0, big.mark=",")

##########

# ggplot bar chart showing the number of entrances by FY

##########

# data for ggplot showing the number of entrances by FY
df_entrances_long <- df_entrances %>% select(-total)
df_entrances_long <- gather(df_entrances_long, fy, total, `2019`:`2021`, factor_key=TRUE) %>%
  mutate(fy = as.numeric(fy)) %>%
  mutate(fy = case_when(fy == 1 ~ "2019", fy == 2 ~ "2020", fy == 3 ~ "2021"))

# gg plot showing the number of entrances by FY
gg_entrances <-
  ggplot(data=df_entrances_long, aes(x=fy, y=total)) +
  geom_bar(stat="identity", width = 0.74, fill = jri_dark_blue) +
  xlab("") + ylab("Number of Entrances") +
  geom_text(aes(label = comma(total)), color = "black", vjust = -1, size = 7.5, family = "Franklin Gothic Book") +
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3, big.mark = ","),
                     expand = c(0,0),
                     limits = c(0,24000)) +
  theme_no_axes

# save changes between years to call in sentences in incarceration patterns rmd
v1 <- as.numeric(filter(df_entrances_events, fy==2019) %>% select(total))
v2 <- as.numeric(filter(df_entrances_events, fy==2020) %>% select(total))
v3 <- as.numeric(filter(df_entrances_events, fy==2021) %>% select(total))
change_19_20_entrances <- (v2 - v1)/v1
change_19_20_entrances <- round(change_19_20_entrances*100, 1)
change_20_21_entrances <- (v3 - v2)/v2
change_20_21_entrances <- round(change_20_21_entrances*100, 1)
change_19_21_entrances <- (v3 - v1)/v1
change_19_21_entrances <- round(change_19_21_entrances*100, 1)

##########
# by county
##########

# data table of number of entrances by FY and county
df_entrances_county <- bookings_entrances %>%
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

# data table of number of bookings by FY and county
df_bookings_county <- booking_no_pc_hold %>%
  select(id, booking_id, fy, county) %>%
  distinct() %>%
  group_by(fy, county) %>%
  dplyr::summarise(total = n()) %>%
  spread(fy, total) %>%
  mutate(`2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`),
         `2021` = as.numeric(`2021`)) %>%
  mutate(total = `2019` + `2020` + `2021`)

# data table of number of bookings for Coos, since we dont have the number of entrances
df_bookings_coos <- booking_no_pc_hold %>%
  select(id, booking_id, fy, county) %>%
  distinct() %>%
  group_by(fy, county) %>%
  dplyr::summarise(total = n()) %>%
  spread(fy, total) %>%
  mutate(`2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`),
         `2021` = as.numeric(`2021`)) %>%
  mutate(total = `2019` + `2020` + `2021`) %>%
  filter(county == "Coos")

##########

# reactable of number of bookings by FY and county

##########

table_bookings_fy_county <- fnc_reactable_county_fy(df_bookings_county)

##########

# reactable of number of entrances by FY and county

##########

# shows number of entrances by FY and county and the change from 2019 to 2021
table_entrances_fy_county <- fnc_reactable_county_fy(df_entrances_county)

################################################################################

# Common Booking Types ????????????????????????? which data frame?

################################################################################

# custom functions to find the number of booking types by fiscal year
df_booking <- fnc_variable_table(bookings_entrances_19, bookings_entrances_20, bookings_entrances_21, "booking_type_standard")
df_booking <- fnc_variable_table_desc(df_booking)
df_booking <- df_booking %>% filter(variable_name != "Total") %>%
  select(booking_type = variable_name, everything())

##########

# Reactable table

##########

# create reactable table of number/freq of booking types by fiscal year and for all 3 years
table_booking_types <- fnc_reactable_fy(df_booking,
                                           metric_label = "Booking Type",
                                           label_width = 275,
                                           note = "")

################################################################################

# LOS

# What was the average length of incarceration in jail?
#
# Statistics: First, break up into PC vs. criminal charge admissions
# Then, calculate the minimum, mean, median, and maximum jails stays
#
# Graphic: show distribution (density plot?), with vertical lines for average
# (we can use median is the data are skewed, or mean if they’re somewhat normally distributed)
#
# I wonder if we could potentially have a global distribution density plot,
# but then have 9 lines showing the average for each county as a visual to compare across counties.
# This is a low priority though.

################################################################################

# count freq of los for non-PC Holds
# remove strafford because they don't have pc data
# keep coos because all the data we were given was non-PC Holds

df_los_no_pc_hold <- booking_no_pc_hold %>%
  select(fy,
         county,
         booking_id,
         los,
         high_utilizer_4_times,
         high_utilizer_1_pct, high_utilizer_5_pct, high_utilizer_10_pct) %>%
  distinct() %>%
  filter(!is.na(los))
dim(df_los_no_pc_hold); length(unique(df_los_no_pc_hold$booking_id)) # 31769, 31769

# average lOS for non-PC bookings
avg_los_no_pc_hold <- df_los_no_pc_hold %>%
  group_by() %>%
  dplyr::summarize(mean = mean(los, na.rm=TRUE))
avg_los_no_pc_hold <- as.numeric(avg_los_no_pc_hold)

# overall LOS for all non-PC Holds
df_los_no_pc_hold_summary <- fnc_summary(df_los_no_pc_hold, "los")
df_los_no_pc_hold_summary <- df_los_no_pc_hold_summary %>% mutate(type = "All (HU's and non-HU's)") %>% select(type, everything())

##########

# Reactable table for LOS for non-PC holds

##########

# reactable table for LOS summary statistics by HU type
table_los_no_pc_hold_summary <- reactable(df_los_no_pc_hold_summary,
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

# count freq of los for entrances (including Coos bookings and Strafford)
df_los_entrances_with_coos <- bookings_entrances %>%
  select(fy,
         county,
         booking_id,
         los,
         high_utilizer_4_times,
         high_utilizer_1_pct, high_utilizer_5_pct, high_utilizer_10_pct) %>%
  distinct() %>%
  filter(!is.na(los))
dim(df_los_entrances_with_coos); length(unique(df_los_entrances_with_coos$booking_id)) # 51349, 51349 will be less because of NAs in LOS's

# average lOS for non-PC bookings
avg_los_entrances_with_coos <- df_los_entrances_with_coos %>%
  group_by() %>%
  dplyr::summarize(mean = mean(los, na.rm=TRUE))
avg_los_entrances_with_coos <- as.numeric(avg_los_entrances_with_coos)

# overall LOS for all non-PC Holds
df_los_entrances_with_coos_summary <- fnc_summary(df_los_entrances_with_coos, "los")
df_los_entrances_with_coos_summary <- df_los_entrances_with_coos_summary %>% mutate(type = "All (HU's and non-HU's)") %>% select(type, everything())

##########

# Reactable table for LOS for entrances (including Coos bookings and Strafford)

##########

# reactable table for LOS summary statistics by HU type
table_los_entrances_with_coos_summary <- reactable(df_los_entrances_with_coos_summary,
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


################################################################################

# Save to SP

################################################################################

# people booked
save(amt_people_booked,                     file=paste0(sp_data_path, "/Data/r_data/amt_people_booked.Rda",                      sep = ""))
save(amt_people_booked_fy_total,            file=paste0(sp_data_path, "/Data/r_data/amt_people_booked_fy_total.Rda",             sep = ""))
save(table_people_booked,                   file=paste0(sp_data_path, "/Data/r_data/table_people_booked.Rda",                    sep = ""))
save(table_people_booked_fy_county,         file=paste0(sp_data_path, "/Data/r_data/table_people_booked_fy_county.Rda",          sep = ""))
save(hc_people_booked,                      file=paste0(sp_data_path, "/Data/r_data/hc_people_booked.Rda",                       sep = ""))
save(gg_people_booked,                      file=paste0(sp_data_path, "/Data/r_data/gg_people_booked.Rda",                       sep = ""))
save(change_19_20_people_booked,            file=paste0(sp_data_path, "/Data/r_data/change_19_20_people_booked.Rda",             sep = ""))
save(change_20_21_people_booked,            file=paste0(sp_data_path, "/Data/r_data/change_20_21_people_booked.Rda",             sep = ""))
save(change_19_21_people_booked,            file=paste0(sp_data_path, "/Data/r_data/change_19_21_people_booked.Rda",             sep = ""))

# bookings
save(amt_bookings,                          file=paste0(sp_data_path, "/Data/r_data/amt_bookings.Rda",                          sep = ""))
save(table_bookings_fy,                     file=paste0(sp_data_path, "/Data/r_data/table_bookings_fy.Rda",                     sep = ""))
save(table_bookings_fy_county,              file=paste0(sp_data_path, "/Data/r_data/table_bookings_fy_county.Rda",              sep = ""))
save(table_entrances_fy_county,             file=paste0(sp_data_path, "/Data/r_data/table_entrances_fy_county.Rda",             sep = ""))
save(hc_bookings,                           file=paste0(sp_data_path, "/Data/r_data/hc_bookings.Rda",                           sep = ""))
save(change_19_20_bookings,                 file=paste0(sp_data_path, "/Data/r_data/change_19_20_bookings.Rda",                 sep = ""))
save(change_20_21_bookings,                 file=paste0(sp_data_path, "/Data/r_data/change_20_21_bookings.Rda",                 sep = ""))
save(change_19_21_bookings,                 file=paste0(sp_data_path, "/Data/r_data/change_19_21_bookings.Rda",                 sep = ""))
save(gg_bookings,                           file=paste0(sp_data_path, "/Data/r_data/gg_bookings.Rda",                           sep = ""))
save(gg_entrances,                          file=paste0(sp_data_path, "/Data/r_data/gg_entrances.Rda",                           sep = ""))

# booking types
save(table_booking_types,                   file=paste0(sp_data_path, "/Data/r_data/table_booking_types.Rda",                   sep = ""))

# length of stay
save(avg_los_no_pc_hold,                    file=paste0(sp_data_path, "/Data/r_data/avg_los_no_pc_hold.Rda",                    sep = ""))
save(table_los_no_pc_hold_summary,          file=paste0(sp_data_path, "/Data/r_data/table_los_no_pc_hold_summary.Rda",          sep = ""))
save(avg_los_entrances_with_coos,           file=paste0(sp_data_path, "/Data/r_data/avg_los_no_pc_hold.Rda",                    sep = ""))
save(table_los_entrances_with_coos_summary, file=paste0(sp_data_path, "/Data/r_data/table_los_entrances_with_coos_summary.Rda", sep = ""))
