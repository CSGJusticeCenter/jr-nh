############################################
# Project: JRI New Hampshire
# File: incarceration_patterns.R
# Last updated: August 22, 2022
# Author: Mari Roberts

# Tables, graphs, and numbers for incarcerarion patterns page
############################################

# detach plyr to remove issues with dplyr
detach(package:plyr)

# save booking dates
all_booking_dates <- nh_booking %>% select(id, booking_id, booking_date, month_year_text, month_year) %>% distinct()

##################
# How many individual people were booked into New Hampshire jails annually?
##################

###
# by state
###

df_people_booked_pre <- nh_booking %>%
  select(id, fy, county) %>%
  distinct() %>%
  group_by(fy) %>%
  dplyr::summarise(total = n()) %>%
  mutate(label = formatC(total, format="d", big.mark=","))

df_people_booked <- df_people_booked_pre %>%
  select(-label) %>%
  t %>% as.data.frame() %>%
  row_to_names(1) %>%
  mutate(`2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`),
         `2021` = as.numeric(`2021`)) %>%
  mutate(total = `2019` + `2020` + `2021`)

df_people_booked <- tibble::rownames_to_column(df_people_booked, "variable_name")
df_people_booked <- df_people_booked %>% mutate(variable_name = ifelse(variable_name == "total", "# People Booked into Jail", ""))
nh_people_booked <- reactable(df_people_booked,
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
nh_people_booked_amt <- df_people_booked$total
nh_people_booked_amt <- format(round(as.numeric(nh_people_booked_amt), 0), nsmall=0, big.mark=",")

# get counties included
nh_counties <- fnc_counties_in_data(nh_booking)

showtext_auto()
nh_people_booked_barchart <- df_people_booked_pre %>%
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
nh_people_booked_change_19_20 <- (v2 - v1)/v1
nh_people_booked_change_19_20 <- round(nh_people_booked_change_19_20*100, 1)
nh_people_booked_change_20_21 <- (v3 - v2)/v2
nh_people_booked_change_20_21 <- round(nh_people_booked_change_20_21*100, 1)
nh_people_booked_change_19_21 <- (v3 - v1)/v1
nh_people_booked_change_19_21 <- round(nh_people_booked_change_19_21*100, 1)

###
# by county
###

nh_people_booked_county <- nh_booking %>%
  select(id, fy, county) %>%
  distinct() %>%
  group_by(fy, county) %>%
  dplyr::summarise(total = n())

nh_people_booked_county <- nh_people_booked_county %>% spread(fy, total)
nh_people_booked_county <- nh_people_booked_county %>%
  mutate(`2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`),
         `2021` = as.numeric(`2021`)) %>%
  mutate(total = `2019` + `2020` + `2021`)

nh_people_booked_county <- reactable(nh_people_booked_county,
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

##################
# How bookings does NH have per fiscal year?
##################

###
# by state
###

# larger df with pc hold variable
df_bookings_events_all <- nh_booking %>%
  select(id, booking_id, fy, county, pc_hold) %>%
  distinct()
dim(df_bookings_events_all) # 39761

# smaller df without pc hold variable
df_bookings_events_distinct <- df_bookings_events_all %>%
  select(-pc_hold) %>%
  distinct()
dim(df_bookings_events_distinct) # 39348

# calculate number of booking events per year
df_bookings_events <- df_bookings_events_distinct %>%
  group_by(fy) %>%
  dplyr::summarise(total = n()) %>%
  mutate(label = formatC(total, format="d", big.mark=","))
df_bookings <- df_bookings_events %>%
  select(-label) %>%
  t %>% as.data.frame() %>%
  row_to_names(1) %>%
  mutate(`2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`),
         `2021` = as.numeric(`2021`)) %>%
  mutate(total = `2019` + `2020` + `2021`)

# create dataframe showing number of bookings by fiscal year
df_bookings <- tibble::rownames_to_column(df_bookings, "variable_name")
df_bookings <- df_bookings %>% mutate(variable_name = ifelse(variable_name == "total", "# of Bookings", ""))
nh_bookings <- reactable(df_bookings,
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
nh_bookings_amt <- df_bookings$total
nh_bookings_amt <- format(round(as.numeric(nh_bookings_amt), 0), nsmall=0, big.mark=",")

# get counties included
nh_counties <- fnc_counties_in_data(nh_booking)

# bar chart showing the number of bookings by FY
showtext_auto()
nh_bookings_barchart <- df_bookings_pre %>%
  hchart('column', hcaes(x = fy, y = total, color = jri_light_blue)) %>%
  hc_xAxis(title = list(text = "Fiscal Year", style = list(color =  "#000000", fontWeight = "bold"))) %>%
  hc_yAxis(title = list(text = "Number of Bookings", style = list(color =  "#000000", fontWeight = "bold"))) %>%
  hc_setup() %>%
  hc_add_theme(hc_theme_jc) %>%
  hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, format = "{point.label}")))

# save changes between years to call in sentences in incarceration patterns rmd
v1 <- as.numeric(filter(df_bookings_pre, fy==2019) %>% select(total))
v2 <- as.numeric(filter(df_bookings_pre, fy==2020) %>% select(total))
v3 <- as.numeric(filter(df_bookings_pre, fy==2021) %>% select(total))
nh_bookings_change_19_20 <- (v2 - v1)/v1
nh_bookings_change_19_20 <- round(nh_bookings_change_19_20*100, 1)
nh_bookings_change_20_21 <- (v3 - v2)/v2
nh_bookings_change_20_21 <- round(nh_bookings_change_20_21*100, 1)
nh_bookings_change_19_21 <- (v3 - v1)/v1
nh_bookings_change_19_21 <- round(nh_bookings_change_19_21*100, 1)

###
# by county
###

nh_bookings_county <- nh_booking %>%
  select(id, booking_id, fy, county) %>%
  distinct() %>%
  group_by(fy, county) %>%
  dplyr::summarise(total = n())

nh_bookings_county <- nh_bookings_county %>% spread(fy, total)
nh_bookings_county <- nh_bookings_county %>%
  mutate(`2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`),
         `2021` = as.numeric(`2021`)) %>%
  mutate(total = `2019` + `2020` + `2021`)

nh_bookings_county <- reactable(nh_bookings_county,
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

############################################################################################################
# Booking Types

# What are the most common booking types?
# try to find a way to explain how protective custody holds are labeled as numerous
# things in the booking type, charge description, etc.
############################################################################################################

# sep by fy year
nh_booking_19 <- nh_booking %>% select(county, id, fy, booking_id, booking_date, booking_type) %>% distinct() %>% filter(fy == 2019)
nh_booking_20 <- nh_booking %>% select(county, id, fy, booking_id, booking_date, booking_type) %>% distinct() %>% filter(fy == 2020)
nh_booking_21 <- nh_booking %>% select(county, id, fy, booking_id, booking_date, booking_type) %>% distinct() %>% filter(fy == 2021)

# custom functions to find the number of booking types by fiscal year
df_booking <- fnc_variable_table(nh_booking_19, nh_booking_20, nh_booking_21, "booking_type")
df_booking <- fnc_variable_table_desc(df_booking)
df_booking <- df_booking %>% filter(variable_name != "Total") %>%
  select(booking_type = variable_name, everything())

# create reactable table of number/freq of booking types by fiscal year and for all 3 years
nh_booking_types <- reactable(df_booking,
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


############################################################################################################
# PC HOLDS
############################################################################################################

###########
# Highchart pc holds over time
# remove Coos because hthey delete the entire booking if it's a PC hold
###########

# detach(package:plyr)
df_pch <- df_bookings_events_all %>%
  filter(county != "Coos") %>%
  dplyr::group_by(booking_id) %>%
  mutate(all_hold_types=paste(sort(unique(pc_hold)), collapse="&")) %>%
  mutate(pc_hold_in_booking = ifelse(all_hold_types == 'Non-PC Hold&PC Hold' | all_hold_types == 'PC Hold', "PC Hold Booking", "Non-PC Hold Booking")) %>%
  select(booking_id, pc_hold_in_booking, county, fy) %>%
  distinct()
dim(df_pch)

df_pch <- merge(df_pch, all_booking_dates, by = "booking_id", all.x = TRUE)

# get counties included
pch_counties <- fnc_counties_in_data(df_pch)

# filter to PC holds
df1 <- df_pch %>% filter(pc_hold_in_booking == "PC Hold Booking")

# generate high chart using custom function
nh_pch_time_highchart <- fnc_covid_time_highchart(df1, yaxis_label = "Number of PC Holds", title = NULL)

###########
# Table pc holds by FY
###########

# filter by year
pch_19 <- df_pch %>% select(county, id, booking_id, fy, pc_hold_in_booking) %>% distinct() %>% filter(fy == 2019)
pch_20 <- df_pch %>% select(county, id, booking_id, fy, pc_hold_in_booking) %>% distinct() %>% filter(fy == 2020)
pch_21 <- df_pch %>% select(county, id, booking_id, fy, pc_hold_in_booking) %>% distinct() %>% filter(fy == 2021)

# generate table showing PC holds from 2019-2021
pch_df <- fnc_variable_table(pch_19, pch_20, pch_21, "pc_hold_in_booking")
pch_df <- pch_df %>% dplyr::rename(pc_hold_in_booking = variable_name)
pch_df[is.na(pch_df)] = 0
pch_df <- pch_df %>% filter(pc_hold_in_booking != "Total")

# % of bookings that are PC holds
nh_pch_pct_amt <- pch_df %>% filter(pc_hold_in_booking == "Yes")
nh_pch_pct_amt <- nh_pch_pct_amt$freq*100
nh_pch_pct_amt <- round(nh_pch_pct_amt, 1)

# create reactable table for pc holds by fiscal year
nh_pch_table <- fnc_reactable_fy(pch_df, metric_label = " ", label_width = 150, reactable_counties = pch_counties, note = "Coos removes bookings that are PC holds so Coos's administrative data (671 bookings) is not included in this table.")

############################################################################################################
# Save to SP
############################################################################################################

save(nh_counties,                   file=paste0(sp_data_path, "/Data/r_data/nh_counties.Rda",                   sep = ""))

save(nh_people_booked_amt,          file=paste0(sp_data_path, "/Data/r_data/nh_people_booked_amt.Rda",          sep = ""))
save(nh_people_booked,              file=paste0(sp_data_path, "/Data/r_data/nh_people_booked.Rda",              sep = ""))
save(nh_people_booked_county,       file=paste0(sp_data_path, "/Data/r_data/nh_people_booked_county.Rda",       sep = ""))
save(nh_people_booked_barchart,     file=paste0(sp_data_path, "/Data/r_data/nh_people_booked_barchart.Rda",     sep = ""))
save(nh_people_booked_change_19_20, file=paste0(sp_data_path, "/Data/r_data/nh_people_booked_change_19_20.Rda", sep = ""))
save(nh_people_booked_change_20_21, file=paste0(sp_data_path, "/Data/r_data/nh_people_booked_change_20_21.Rda", sep = ""))
save(nh_people_booked_change_19_21, file=paste0(sp_data_path, "/Data/r_data/nh_people_booked_change_19_21.Rda", sep = ""))

save(nh_bookings_amt,               file=paste0(sp_data_path, "/Data/r_data/nh_bookings_amt.Rda",               sep = ""))
save(nh_bookings,                   file=paste0(sp_data_path, "/Data/r_data/nh_bookings.Rda",                   sep = ""))
save(nh_bookings_county,            file=paste0(sp_data_path, "/Data/r_data/nh_bookings_county.Rda",            sep = ""))
save(nh_bookings_barchart,          file=paste0(sp_data_path, "/Data/r_data/nh_bookings_barchart.Rda",          sep = ""))
save(nh_bookings_change_19_20,      file=paste0(sp_data_path, "/Data/r_data/nh_bookings_change_19_20.Rda",      sep = ""))
save(nh_bookings_change_20_21,      file=paste0(sp_data_path, "/Data/r_data/nh_bookings_change_20_21.Rda",      sep = ""))
save(nh_bookings_change_19_21,      file=paste0(sp_data_path, "/Data/r_data/nh_bookings_change_19_21.Rda",      sep = ""))

save(nh_booking_types,              file=paste0(sp_data_path, "/Data/r_data/nh_booking_types.Rda",              sep = ""))
save(nh_pch_time_highchart,         file=paste0(sp_data_path, "/Data/r_data/nh_pch_time_highchart.Rda",         sep = ""))
save(nh_pch_table,                  file=paste0(sp_data_path, "/Data/r_data/nh_pch_table.Rda",                  sep = ""))
save(nh_pch_pct_amt,                file=paste0(sp_data_path, "/Data/r_data/nh_pch_pct_amt.Rda",                sep = ""))
save(pch_counties,                  file=paste0(sp_data_path, "/Data/r_data/pch_counties.Rda",                  sep = ""))
