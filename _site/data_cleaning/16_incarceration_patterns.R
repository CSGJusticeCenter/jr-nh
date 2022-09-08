############################################
# Project: JRI New Hampshire
# File: incarceration_patterns.R
# Last updated: August 22, 2022
# Author: Mari Roberts

# Generate tables for state overview

# Uses tables created in info_by_county.R
# nh_adm_all
# nh_sentence, nh_sentence_19, nh_sentence_20, nh_sentence_21
# nh_booking, nh_booking_19, nh_booking_20, nh_booking_21
############################################

##################
# How many people were booked into New Hampshire jails annually?
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
            `variable_name` = colDef(minWidth = 185, name = ""),
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
  hc_yAxis(title = list(text = "Number of PC Holds", style = list(color =  "#000000", fontWeight = "bold"))) %>%
  hc_setup() %>%
  hc_add_theme(hc_theme_jc) %>%
  hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, format = "{point.label}")))

# save changes between years to call in sentences in incarceration patterns rmd
v1 <- as.numeric(filter(df_people_booked_pre, fy==2019) %>% select(total))
v2 <- as.numeric(filter(df_people_booked_pre, fy==2020) %>% select(total))
v3 <- as.numeric(filter(df_people_booked_pre, fy==2021) %>% select(total))
nh_booked_change_19_20 <- (v2 - v1)/v1
nh_booked_change_19_20 <- round(nh_booked_change_19_20*100, 1)
nh_booked_change_20_21 <- (v3 - v2)/v2
nh_booked_change_20_21 <- round(nh_booked_change_20_21*100, 1)
nh_booked_change_19_21 <- (v3 - v1)/v1
nh_booked_change_19_21 <- round(nh_booked_change_19_21*100, 1)

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


# “Local” version – both graphic and stats, but for single counties
# create reactable table of number/freq of booking types by fiscal year and for all 3 years

# Graphic: line plot or other, with state-level, annual admissions per year FY2019-2021
# Accompanying statistic: average annual admissions per year (total admission/3)

############################################################################################################
# Booking Types

# What are the most common booking types?
# try to find a way to explain how protective custody holds are labeled as numerous
# things in the booking type, charge description, etc.
############################################################################################################

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
###########

# remove Coos
df_pch <- nh_booking %>% filter(county != "Coos")

# get counties included
pch_counties <- fnc_counties_in_data(df_pch)

# filter to PC holds
df1 <- df_pch %>% filter(pc_hold == "PC Hold")

# generate high chart using custom function
nh_pch_time_highchart <- fnc_covid_time_highchart(df1, yaxis_label = "Number of PC Holds", title = NULL)

###########
# Table pc holds by FY
###########

# filter by year
pch_19 <- df_pch %>% filter(fy == 2019)
pch_20 <- df_pch %>% filter(fy == 2020)
pch_21 <- df_pch %>% filter(fy == 2021)

# generate table showing PC holds from 2019-2021
pch_df <- fnc_variable_table(pch_19, pch_20, pch_21, "pc_hold")
pch_df <- pch_df %>% dplyr::rename(pc_hold = variable_name)
pch_df[is.na(pch_df)] = 0
pch_df <- pch_df %>% filter(pc_hold != "Total")

# % of bookings that are PC holds
nh_pch_pct_amt <- pch_df %>% filter(pc_hold == "PC Hold")
nh_pch_pct_amt <- nh_pch_pct_amt$freq*100
nh_pch_pct_amt <- round(nh_pch_pct_amt, 1)

# create reactable table for pc holds by fiscal year
nh_pch_table <- fnc_reactable_fy(pch_df, metric_label = " ", label_width = 150, reactable_counties = pch_counties, note = "Coos removes bookings that are PC holds so Coos's administrative data is not included in this table.")

############################################################################################################
# Save to SP
############################################################################################################

save(nh_counties,               file=paste0(sp_data_path, "/Data/r_data/nh_counties.Rda",               sep = ""))
save(nh_people_booked_amt,      file=paste0(sp_data_path, "/Data/r_data/nh_people_booked_amt.Rda",      sep = ""))

save(nh_people_booked,          file=paste0(sp_data_path, "/Data/r_data/nh_people_booked.Rda",          sep = ""))
save(nh_people_booked_county,   file=paste0(sp_data_path, "/Data/r_data/nh_people_booked_county.Rda",   sep = ""))
save(nh_people_booked_barchart, file=paste0(sp_data_path, "/Data/r_data/nh_people_booked_barchart.Rda", sep = ""))

save(nh_booked_change_19_20,    file=paste0(sp_data_path, "/Data/r_data/nh_booked_change_19_20.Rda",    sep = ""))
save(nh_booked_change_20_21,    file=paste0(sp_data_path, "/Data/r_data/nh_booked_change_20_21.Rda",    sep = ""))
save(nh_booked_change_19_21,    file=paste0(sp_data_path, "/Data/r_data/nh_booked_change_19_21.Rda",    sep = ""))

save(nh_booking_types,          file=paste0(sp_data_path, "/Data/r_data/nh_booking_types.Rda",          sep = ""))
save(nh_pch_time_highchart,     file=paste0(sp_data_path, "/Data/r_data/nh_pch_time_highchart.Rda",     sep = ""))
save(nh_pch_table,              file=paste0(sp_data_path, "/Data/r_data/nh_pch_table.Rda",              sep = ""))
save(nh_pch_pct_amt,            file=paste0(sp_data_path, "/Data/r_data/nh_pch_pct_amt.Rda",            sep = ""))
save(pch_counties,              file=paste0(sp_data_path, "/Data/r_data/pch_counties.Rda",              sep = ""))
