############################################
# Project: JRI New Hampshire
# File: incarceration_patterns.R
# Last updated: October 12, 2022
# Author: Mari Roberts

# Tables, graphs, and numbers for incarceration patterns page
# Focuses on bookings, booking types, and los

# Notes:
# Entrances = Bookings and PC holds
# Bookings  = Booked for a criminal charge
# A PC hold is technically not a booking
############################################

################################################################################

# PEOPLE BOOKED INTO JAIL

# Use booking_no_pc_hold because it is bookings that do not include PC holds
# Does not include Strafford

################################################################################

####################

# by state

####################

# table of total number of people booked (no duplicates for counting by FY)
# no Strafford
amt_people_booked <- booking_no_pc_hold %>%
  dplyr::ungroup() %>%
  dplyr::select(id, county) %>%
  dplyr::distinct() %>%
  dplyr::group_by() %>%
  dplyr::summarise(total = n()) %>%
  dplyr::mutate(label = formatC(total, format="d", big.mark=","))
  amt_people_booked <- amt_people_booked$total
  # amt_people_booked <- format(round(as.numeric(amt_people_booked), 0), nsmall=0, big.mark=",")

# table of total number of people booked by FY (some duplicates because it's by FY)
df_people_booked_pre <- booking_no_pc_hold %>%
  dplyr::ungroup() %>%
  dplyr::select(id, fy, county) %>%
  dplyr::distinct() %>%
  dplyr::group_by(fy) %>%
  dplyr::summarise(total = n()) %>%
  dplyr::mutate(label = formatC(total, format="d", big.mark=","))

# transpose and format, wide version
df_people_booked <- df_people_booked_pre %>%
  select(-label) %>%
  t %>% as.data.frame() %>%
  row_to_names(1) %>%
  mutate(`2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`),
         `2021` = as.numeric(`2021`)) %>%
  mutate(total = amt_people_booked)
  df_people_booked <- tibble::rownames_to_column(df_people_booked, "variable_name")
  df_people_booked <- df_people_booked %>%
  mutate(variable_name = ifelse(variable_name == "total", "# Unique People Booked into Jail", ""))

# count number of people booked for all three years / accounts for double counting by FY-accurate
# no Strafford
amt_people_booked <- format(round(as.numeric(amt_people_booked), 0), nsmall=0, big.mark=",")

##########

# create reactable table showing number of people booked by FY and total

##########

# one row table minimal showing number of people booked by FY and total
# no Strafford
table_people_booked <-
  reactable(df_people_booked,
            pagination = FALSE,
            theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
            defaultColDef = reactable::colDef(format = colFormat(separators = TRUE), align = "center"),
            compact = TRUE,
            fullWidth = FALSE,
            columns = list(
              `variable_name` = colDef(minWidth = 250, name = ""),
              `2019`  = colDef(minWidth = 80, name = "2019"),
              `2020`  = colDef(minWidth = 80, name = "2020"),
              `2021`  = colDef(minWidth = 80, name = "2021",
                               style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
              `total` = colDef(minWidth = 80, name = "Total")))

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

##########

# create reactable table showing number people booked by FY and total

##########

# number of people booked by county for all three years
amt_people_booked_county <- booking_no_pc_hold %>%
  dplyr::ungroup() %>%
  dplyr::select(id, county) %>%
  dplyr::distinct() %>%
  dplyr::group_by(county) %>%
  dplyr::summarise(total = n())

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
  left_join(amt_people_booked_county, by = "county")

df_people_booked_county <- df_people_booked_county %>%
  adorn_totals("row") %>%
  mutate(change_19_21 = (`2021`-`2019`)/`2019`) %>%
  mutate(county = case_when(county == "Coos" ~ "Coos", TRUE ~ county))

# table showing the number of people booked by FY by county
PRES_table_people_booked_fy_county <- fnc_reactable_county_fy(df_people_booked_county, row_num = 9)

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


#################################################################################################################################################
#################################################################################################################################################

# NUMBER OF BOOKINGS

# Use booking_no_pc_hold because it is bookings that do not include PC holds
# Does not include Strafford

#################################################################################################################################################
#################################################################################################################################################

####################

# by state

####################

##########

# ggplot bar chart showing the number of bookings by FY

##########

# calculate number of booking events per fy
df_bookings_events <- booking_no_pc_hold %>%
  select(id, booking_id, fy, county) %>%
  distinct() %>%
  distinct() %>%
  group_by(fy) %>%
  dplyr::summarise(total = n()) %>%
  mutate(label = formatC(total, format="d", big.mark=","))

# create table showing number of bookings by fiscal year
df_bookings <- df_bookings_events %>%
  select(-label) %>%
  t %>% as.data.frame() %>%
  row_to_names(1) %>%
  mutate(`2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`),
         `2021` = as.numeric(`2021`)) %>%
  mutate(total = `2019` + `2020` + `2021`)
  df_bookings <- tibble::rownames_to_column(df_bookings, "variable_name")
  df_bookings <- df_bookings %>%
    mutate(variable_name = ifelse(variable_name == "total", "# of Bookings", ""))

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

# PRESENTATION - ggplot showing the number of bookings by FY

##########

# data for ggplot showing the number of bookings by FY
df1 <- df_bookings %>% select(-total)
df1 <- gather(df1, fy, total, `2019`:`2021`, factor_key=TRUE) %>%
  mutate(fy = as.numeric(fy)) %>%
  mutate(fy = case_when(fy == 1 ~ "2019", fy == 2 ~ "2020", fy == 3 ~ "2021"))

# ggplot showing the number of bookings by FY
PRES_gg_bookings <-
  ggplot(data=df1, aes(x=fy, y=total)) +
  geom_bar(stat="identity", width = 0.74, fill = jri_green) +
  xlab("") + ylab("Number of Bookings") +
  geom_text(aes(label = comma(total)), color = "black", vjust = -1, size = 7.5, family = "Franklin Gothic Book") +
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3, big.mark = ","),
                     expand = c(0,0),
                     limits = c(0,24000)) +
  theme_no_axes

####################

# by County

####################

##########

# create reactable table showing number bookings by FY and total

##########

# number of bookings by county for all three years
amt_bookings_county <- booking_no_pc_hold %>%
  dplyr::ungroup() %>%
  dplyr::select(booking_id, county) %>%
  dplyr::distinct() %>%
  dplyr::group_by(county) %>%
  dplyr::summarise(total = n())

# df of total number of people booked by FY
df_bookings_county <- booking_no_pc_hold %>%
  dplyr::ungroup() %>%
  dplyr::select(booking_id, fy, county) %>%
  dplyr::distinct() %>%
  dplyr::group_by(fy, county) %>%
  dplyr::summarise(total = n()) %>%
  spread(fy, total) %>%
  mutate(`2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`),
         `2021` = as.numeric(`2021`)) %>%
  left_join(amt_bookings_county, by = "county")

df_bookings_county <- df_bookings_county %>%
  adorn_totals("row") %>%
  mutate(change_19_21 = (`2021`-`2019`)/`2019`) %>%
  mutate(county = case_when(county == "Coos" ~ "Coos", TRUE ~ county))

# table showing the number of people booked by FY by county
PRES_table_bookings_fy_county <- fnc_reactable_county_fy(df_bookings_county, row_num = 9)



#################################################################################################################################################
#################################################################################################################################################

# Common Booking Types

#################################################################################################################################################
#################################################################################################################################################

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
