############################################
# Project: JRI New Hampshire
# File: incarceration_patterns.R
# Last updated: October 11, 2022
# Author: Mari Roberts

# Tables, graphs, and numbers for incarceration patterns page
############################################

# save booking dates
all_booking_dates <- nh_booking %>%
  select(county, id, booking_id, booking_date, month_year_text, month_year, fy) %>%
  distinct()

################################################################################

# How many individual people were booked into New Hampshire jails annually?

################################################################################

##########
# by state
##########

# table of total number of people booked (no duplicates for counting by FY)
df_people_booked_total <- nh_booking_no_pc_hold %>%
  dplyr::ungroup() %>%
  dplyr::select(id, county) %>%
  dplyr::distinct() %>%
  dplyr::group_by() %>%
  dplyr::summarise(total = n()) %>%
  dplyr::mutate(label = formatC(total, format="d", big.mark=","))

# count number of people booked total
amt_nh_people_booked <- df_people_booked_total$total
amt_nh_people_booked <- format(round(as.numeric(amt_nh_people_booked), 0), nsmall=0, big.mark=",")

# table of total number of people booked by FY (some duplicates because it's by FY)
df_people_booked_pre <- nh_booking_no_pc_hold %>%
  dplyr::ungroup() %>%
  dplyr::select(id, fy, county) %>%
  dplyr::distinct() %>%
  dplyr::group_by(fy) %>%
  dplyr::summarise(total = n()) %>%
  dplyr::mutate(label = formatC(total, format="d", big.mark=","))

# transpose and format
df_people_booked <- df_people_booked_pre %>%
  select(-label) %>%
  t %>% as.data.frame() %>%
  row_to_names(1) %>%
  mutate(`2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`),
         `2021` = as.numeric(`2021`)) %>%
  mutate(total = `2019` + `2020` + `2021`)

# create reactable table
df_people_booked <- tibble::rownames_to_column(df_people_booked, "variable_name")
df_people_booked <- df_people_booked %>% mutate(variable_name = ifelse(variable_name == "total", "# People Booked into Jail", ""))
table_nh_people_booked <-
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

# count number of people booked for all three years but some double counting because it's by FY
amt_nh_people_booked_fy_total <- df_people_booked$total
amt_nh_people_booked_fy_total <- format(round(as.numeric(amt_nh_people_booked_fy_total), 0), nsmall=0, big.mark=",")

# get counties included
nh_counties <- fnc_counties_in_data(nh_booking)

# highcharter bar chart of number of people booked by FY
hc_nh_people_booked <- df_people_booked_pre %>%
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
change_19_20_nh_people_booked <- (v2 - v1)/v1
change_19_20_nh_people_booked <- round(change_19_20_nh_people_booked*100, 1)
change_20_21_nh_people_booked <- (v3 - v2)/v2
change_20_21_nh_people_booked <- round(change_20_21_nh_people_booked*100, 1)
change_19_21_nh_people_booked <- (v3 - v1)/v1
change_19_21_nh_people_booked <- round(change_19_21_nh_people_booked*100, 1)

##########
# by county
##########

# table of total number of people booked by FY
df_nh_people_booked_county <- nh_booking_no_pc_hold %>%
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

table_nh_people_booked_fy_county <- reactable(df_nh_people_booked_county,
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
                                                         minWidth = 200, name = "County"),
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
gg_nh_people_booked <-
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

################################################################################

##########
# by state
##########

##########
# ggplot bar chart showing the number of bookings by FY
##########

# select variables
df_bookings_events <- nh_booking_no_pc_hold %>%
  select(id, booking_id, fy, county) %>%
  distinct()

# calculate number of booking events per fy
df_bookings_events <- df_bookings_events %>%
  distinct() %>%
  group_by(fy) %>%
  dplyr::summarise(total = n()) %>%
  mutate(label = formatC(total, format="d", big.mark=","))

# transposed version
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
table_nh_bookings_fy <-
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

# count number of bookings for three years
amt_nh_bookings <- df_bookings$total
amt_nh_bookings <- format(round(as.numeric(amt_nh_bookings), 0), nsmall=0, big.mark=",")

# get counties included
nh_counties <- fnc_counties_in_data(nh_booking)

# data for ggplot showing the number of bookings by FY
df_bookings_long <- df_bookings %>% select(-total)
df_bookings_long <- gather(df_bookings_long, fy, total, `2019`:`2021`, factor_key=TRUE) %>%
  mutate(fy = as.numeric(fy)) %>%
  mutate(fy = case_when(fy == 1 ~ "2019", fy == 2 ~ "2020", fy == 3 ~ "2021"))

# gg plot showing the number of bookings by FY
gg_nh_bookings <-
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
hc_nh_bookings <- df_bookings_events %>%
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
change_19_20_nh_bookings <- (v2 - v1)/v1
change_19_20_nh_bookings <- round(change_19_20_nh_bookings*100, 1)
change_20_21_nh_bookings <- (v3 - v2)/v2
change_20_21_nh_bookings <- round(change_20_21_nh_bookings*100, 1)
change_19_21_nh_bookings <- (v3 - v1)/v1
change_19_21_nh_bookings <- round(change_19_21_nh_bookings*100, 1)

################################################################################

# How many entrances does NH have annually?

################################################################################

##########
# ggplot bar chart showing the number of entrances by FY
##########

# select variables
df_entrances_events <- nh_booking %>%
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

# create table showing number of entrances by fiscal year
df_entrances <- tibble::rownames_to_column(df_entrances, "variable_name")
df_entrances <- df_entrances %>% mutate(variable_name = ifelse(variable_name == "total", "# of entrances", ""))
table_nh_entrances_fy <-
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
amt_nh_entrances <- df_entrances$total
amt_nh_entrances <- format(round(as.numeric(amt_nh_entrances), 0), nsmall=0, big.mark=",")

##########
# ggplot bar chart showing the number of entrances by FY
##########

# data for ggplot showing the number of entrances by FY
df_entrances_long <- df_entrances %>% select(-total)
df_entrances_long <- gather(df_entrances_long, fy, total, `2019`:`2021`, factor_key=TRUE) %>%
  mutate(fy = as.numeric(fy)) %>%
  mutate(fy = case_when(fy == 1 ~ "2019", fy == 2 ~ "2020", fy == 3 ~ "2021"))

# gg plot showing the number of entrances by FY
gg_nh_entrances <-
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
change_19_20_nh_entrances <- (v2 - v1)/v1
change_19_20_nh_entrances <- round(change_19_20_nh_entrances*100, 1)
change_20_21_nh_entrances <- (v3 - v2)/v2
change_20_21_nh_entrances <- round(change_20_21_nh_entrances*100, 1)
change_19_21_nh_entrances <- (v3 - v1)/v1
change_19_21_nh_entrances <- round(change_19_21_nh_entrances*100, 1)

##########
# by county
##########

# data table of number of bookings by FY and county
df_nh_entrances_county <- nh_booking %>%
  select(id, booking_id, fy, county) %>%
  distinct() %>%
  group_by(fy, county) %>%
  dplyr::summarise(total = n()) %>%
  spread(fy, total) %>%
  mutate(`2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`),
         `2021` = as.numeric(`2021`)) %>%
  mutate(total = `2019` + `2020` + `2021`)

# data table of number of bookings by FY and county
df_nh_bookings_county <- nh_booking_no_pc_hold %>%
  select(id, booking_id, fy, county) %>%
  distinct() %>%
  group_by(fy, county) %>%
  dplyr::summarise(total = n()) %>%
  spread(fy, total) %>%
  mutate(`2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`),
         `2021` = as.numeric(`2021`)) %>%
  mutate(total = `2019` + `2020` + `2021`)

# reactable of number of bookings by FY and county
table_nh_bookings_fy_county <-
  reactable(df_nh_bookings_county,
            pagination = FALSE,
            style = list(fontFamily = "Franklin Gothic Book"),
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
                                minWidth = 140, name = "County"),
              `2019`  = colDef(minWidth = 80, name = "2019", align = "center"),
              `2020`  = colDef(minWidth = 80, name = "2020", align = "center"),
              `2021`  = colDef(minWidth = 80, name = "2021", align = "center",
                               style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
              `total` = colDef(minWidth = 80, name = "Total", align = "center")))

# reactable of number of entrances by FY and county
table_nh_entrances_fy_county <-
  reactable(df_nh_entrances_county,
            pagination = FALSE,
            style = list(fontFamily = "Franklin Gothic Book"),
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
                                minWidth = 140, name = "County"),
              `2019`  = colDef(minWidth = 80, name = "2019", align = "center"),
              `2020`  = colDef(minWidth = 80, name = "2020", align = "center"),
              `2021`  = colDef(minWidth = 80, name = "2021", align = "center",
                               style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
              `total` = colDef(minWidth = 80, name = "Total", align = "center")))

################################################################################

# Common Booking Types

################################################################################

# custom functions to find the number of booking types by fiscal year
df_booking <- fnc_variable_table(nh_booking_19, nh_booking_20, nh_booking_21, "booking_type_standard")
df_booking <- fnc_variable_table_desc(df_booking)
df_booking <- df_booking %>% filter(variable_name != "Total") %>%
  select(booking_type = variable_name, everything())

# create reactable table of number/freq of booking types by fiscal year and for all 3 years
table_nh_booking_types <- reactable(df_booking,
                              pagination = FALSE,
                              theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
                              defaultColDef = reactable::colDef(
                                format = colFormat(separators = TRUE), align = "center",
                                footer = function(values, name) {
                                  if (name %in% c("count_19", "count_20", "count_21", "total")) {
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
                              columnGroups = list(
                                colGroup(name = "2019", columns = c("count_19", "pct_19")),
                                colGroup(name = "2020", columns = c("count_20", "pct_20")),
                                colGroup(name = "2021", columns = c("count_21", "pct_21")),
                                colGroup(name = "3 Years", columns = c("total", "freq"))
                              ),
                              columns = list(
                                booking_type = colDef(footer = "Total",
                                                      name = "Booking Type",
                                                      align = "left",
                                                      minWidth = 275),
                                count_19     = colDef(minWidth = 80,
                                                      name = "Count"),
                                pct_19       = colDef(minWidth = 80,
                                                      name = "%",
                                                      format = colFormat(percent = TRUE, digits = 1)),
                                count_20     = colDef(minWidth = 80,
                                                      name = "Count"),
                                pct_20       = colDef(minWidth = 80,
                                                      name = "%",
                                                      format = colFormat(percent = TRUE, digits = 1)),
                                count_21     = colDef(minWidth = 80,
                                                      name = "Count"),
                                pct_21       = colDef(minWidth = 80,
                                                      name = "%",
                                                      style = list(position = "sticky", borderRight = "1px solid #d3d3d3"),
                                                      format = colFormat(percent = TRUE, digits = 1)),
                                total        = colDef(minWidth = 100,
                                                      name = "Count"),
                                freq         = colDef(minWidth = 90,
                                                      name = "%",
                                                      format = colFormat(percent = TRUE, digits = 1))))

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
df_los <- nh_booking %>%
  filter(county != "Strafford") %>%
  select(fy, county, booking_id,
         pc_hold_in_booking,
         los,
         high_utilizer_1_pct, high_utilizer_5_pct, high_utilizer_10_pct) %>%
  filter(pc_hold_in_booking == "Non-PC Hold") %>%
  select(-pc_hold_in_booking) %>%
  distinct() %>%
  filter(!is.na(los))
dim(df_los); length(unique(df_los$booking_id))

# add LOS category
df_los <- df_los %>%
  mutate(los_category =
           case_when(los == 0 ~ "0",
                     los == 1 ~ "1",
                     los == 2 ~ "2",
                     los == 3 ~ "3",
                     los >= 4   & los <= 10   ~ "4-10",
                     los >= 11  & los <= 30  ~ "11-30",
                     los >= 31  & los <= 50  ~ "31-50",
                     los >= 50  & los <= 100 ~ "50-100",
                     los >= 101 & los <= 180 ~ "101-180",
                     los >  180              ~ "Over 180")) %>%
  mutate(los_category = factor(los_category,
                               levels = c("0",
                                          "1",
                                          "2",
                                          "3",
                                          "4-10",
                                          "11-30",
                                          "31-50",
                                          "50-100",
                                          "101-180",
                                          "Over 180")))

# average lOS for non-PC bookings
avg_los_no_pchs <- df_los %>%
  group_by() %>%
  dplyr::summarize(mean = mean(los, na.rm=TRUE))
avg_los_no_pchs <- as.numeric(avg_los_no_pchs)

# overall LOS for all non-PC Holds
df_los_summary <- fnc_los_summary(df_los)
df_los_summary <- df_los_summary %>% mutate(type = "All (HU's and non-HU's)") %>% select(type, everything())

# reactable table for LOS summary statistics by HU type
table_los_summary <- reactable(df_los_summary,
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

# counties in study
save(nh_counties,                      file=paste0(sp_data_path, "/Data/r_data/nh_counties.Rda",                     sep = ""))

# people booked
save(amt_nh_people_booked,             file=paste0(sp_data_path, "/Data/r_data/amt_nh_people_booked.Rda",            sep = ""))
save(amt_nh_people_booked_fy_total,    file=paste0(sp_data_path, "/Data/r_data/amt_nh_people_booked_fy_total.Rda",   sep = ""))
save(table_nh_people_booked,           file=paste0(sp_data_path, "/Data/r_data/table_nh_people_booked.Rda",          sep = ""))
save(table_nh_people_booked_fy_county, file=paste0(sp_data_path, "/Data/r_data/table_nh_people_booked_fy_county.Rda",   sep = ""))
save(hc_nh_people_booked,              file=paste0(sp_data_path, "/Data/r_data/hc_nh_people_booked.Rda",             sep = ""))
save(gg_nh_people_booked,              file=paste0(sp_data_path, "/Data/r_data/gg_nh_people_booked.Rda",             sep = ""))
save(change_19_20_nh_people_booked,    file=paste0(sp_data_path, "/Data/r_data/change_19_20_nh_people_booked.Rda",   sep = ""))
save(change_20_21_nh_people_booked,    file=paste0(sp_data_path, "/Data/r_data/change_20_21_nh_people_booked.Rda",   sep = ""))
save(change_19_21_nh_people_booked,    file=paste0(sp_data_path, "/Data/r_data/change_19_21_nh_people_booked.Rda",   sep = ""))

# bookings
save(amt_nh_bookings,               file=paste0(sp_data_path, "/Data/r_data/amt_nh_bookings.Rda",                 sep = ""))
save(table_nh_bookings_fy,          file=paste0(sp_data_path, "/Data/r_data/table_nh_bookings_fy.Rda",            sep = ""))
save(table_nh_bookings_fy_county,   file=paste0(sp_data_path, "/Data/r_data/table_nh_bookings_fy_county.Rda",     sep = ""))
save(table_nh_entrances_fy_county,  file=paste0(sp_data_path, "/Data/r_data/table_nh_entrances_fy_county.Rda",    sep = ""))
save(hc_nh_bookings,                file=paste0(sp_data_path, "/Data/r_data/hc_nh_bookings.Rda",                  sep = ""))
save(change_19_20_nh_bookings,      file=paste0(sp_data_path, "/Data/r_data/change_19_20_nh_bookings.Rda",        sep = ""))
save(change_20_21_nh_bookings,      file=paste0(sp_data_path, "/Data/r_data/change_20_21_nh_bookings.Rda",        sep = ""))
save(change_19_21_nh_bookings,      file=paste0(sp_data_path, "/Data/r_data/change_19_21_nh_bookings.Rda",        sep = ""))
save(gg_nh_bookings,                file=paste0(sp_data_path, "/Data/r_data/gg_nh_bookings.Rda",                  sep = ""))

# booking types
save(table_nh_booking_types,        file=paste0(sp_data_path, "/Data/r_data/table_nh_booking_types.Rda",          sep = ""))

# length of stay
save(avg_los_no_pchs,               file=paste0(sp_data_path, "/Data/r_data/avg_los_no_pchs.Rda",                 sep = ""))
save(table_los_summary,             file=paste0(sp_data_path, "/Data/r_data/table_los_summary.Rda",               sep = ""))
