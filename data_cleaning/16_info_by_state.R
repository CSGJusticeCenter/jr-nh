############################################
# Project: JRI New Hampshire
# File: info_by_state.R
# Last updated: August 22, 2022
# Author: Mari Roberts

# Generate tables for state overview

# Uses tables created in info_by_county.R
# nh_adm_all
# nh_sentence, nh_sentence_19, nh_sentence_20, nh_sentence_21
# nh_booking, nh_booking_19, nh_booking_20, nh_booking_21
############################################


############################################################
# Incarceration Patterns
############################################################

##################
# How many people were booked into New Hampshire jails annually?
##################




# “Local” version – both graphic and stats, but for single counties


# Graphic: line plot or other, with state-level, annual admissions per year FY2019-2021
# Accompanying statistic: average annual admissions per year (total admission/3)

##################
# What are the most common booking types?
# try to find a way to explain how protective custody holds are labeled as numerous
# things in the booking type, charge description, etc.
##################

# custom functions to find the number of booking types by fiscal year
df_booking <- fnc_variable_table(nh_booking_19, nh_booking_20, nh_booking_21, "booking_type")
df_booking <- fnc_variable_table_desc(df_booking)
df_booking <- df_booking %>% filter(variable_name != "Total") %>%
  select(booking_type = variable_name, everything())

# create reactable table of number/freq of booking types by fiscal year and for all 3 years
nh_booking_by_fy <- reactable(df_booking,
          # rowStyle = function(index) {
          #   if (index %in% c(40)) {
          #     list(`border-bottom` = "thin solid",
          #          `border-color` = "#d3d3d3")}
          # },
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





############################################################
# PC HOLDS
############################################################

df_pch <- nh_booking %>%
  filter(county != "Coos") %>%
  mutate(pc_hold = as.numeric(pc_hold)) %>%
  mutate(pc_hold = ifelse(pc_hold == 2, "PC Hold", "Non-PC Hold"))

# get counties included
pch_counties <- df_pch %>%
  mutate(county = as.character(county))
pch_counties <- unique(pch_counties$county)

###########
# Highchart pc holds over time
###########

nh_pch_time_highchart <- fnc_pch_time_highchart(df_pch)

###########
# Table pc holds by FY
###########

# filter by year
pch_19 <- df_pch %>% filter(fy == 2019)
pch_20 <- df_pch %>% filter(fy == 2020)
pch_21 <- df_pch %>% filter(fy == 2021)

# generate table showing PC holds from 2019-2021
df <- fnc_variable_table(pch_19, pch_20, pch_21, "pc_hold")
df <- df %>% dplyr::rename(pc_hold = variable_name)
df[is.na(df)] = 0

# % of bookings that are PC holds
nh_pch_pct_amt <- df %>% filter(pc_hold == "PC Hold")
nh_pch_pct_amt <- nh_pch_pct_amt$freq*100
nh_pch_pct_amt <- round(nh_pch_pct_amt, 1)

# gt table
nh_pch_table <- gt(df) %>%

  # bold headers and year column
  tab_style(style = cell_text(weight = 'bold'), locations = cells_body(columns = c(pc_hold))) %>%
  tab_style(locations = cells_column_labels(columns = everything()),
            style = list(cell_text(weight = "bold"))) %>%

  fmt_number(columns = c(count_19, count_20, count_21, total), decimals = 0, sep_mark = ",") %>%
  fmt_percent(columns = c(pct_19, pct_20, pct_21, freq), decimals = 1) %>%

  fnc_table_settings() %>%
  fnc_pc_holds_headers()

######
# Save to SP
######

save(nh_booking_by_fy,      file=paste0(sp_data_path, "/Data/r_data/nh_booking_by_fy.Rda",      sep = ""))
save(nh_pch_time_highchart, file=paste0(sp_data_path, "/Data/r_data/nh_pch_time_highchart.Rda", sep = ""))
save(nh_pch_table,          file=paste0(sp_data_path, "/Data/r_data/nh_pch_table.Rda",          sep = ""))
save(nh_pch_pct_amt,        file=paste0(sp_data_path, "/Data/r_data/nh_pch_pct_amt.Rda",        sep = ""))
save(pch_counties,          file=paste0(sp_data_path, "/Data/r_data/pch_counties.Rda",          sep = ""))
