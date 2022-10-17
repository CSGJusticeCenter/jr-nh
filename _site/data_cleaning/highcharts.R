

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
