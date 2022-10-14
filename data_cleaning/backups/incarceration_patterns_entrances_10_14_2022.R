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
