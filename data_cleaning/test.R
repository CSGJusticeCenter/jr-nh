###########

# Table pc holds by FY
# 7700

###########

nh_pch <- nh_booking %>%
  filter(county != "Coos" & county != "Strafford") %>%
  select(county, id, booking_id, pc_hold_in_booking) %>%
  distinct() %>%
  droplevels()

# remove Coos and Strafford
all_booking_dates_no_coos_strafford <- all_booking_dates %>% filter(county != "Coos" & county != "Strafford") %>% droplevels()
df_pch <- merge(nh_pch, all_booking_dates_no_coos_strafford, by = c("id", "booking_id", "county"), all.x = TRUE)

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
nh_pch_pct_amt <- pch_df %>% filter(pc_hold_in_booking == "PC Hold Booking")
nh_pch_pct_amt <- nh_pch_pct_amt$freq*100
nh_pch_pct_amt <- round(nh_pch_pct_amt, 1)

# create reactable table for pc holds by fiscal year
nh_pch_table <- fnc_reactable_fy(pch_df, metric_label = " ", label_width = 150, reactable_counties = pch_counties, note = "Coos removes bookings that are PC holds so Coos's administrative data (671 bookings) is not included in this table. Strafford did not provide data on charges or booking types so they are also excluded (12,233 bookings).")
nh_pch_table
# 7,770

#######################################################

#######################################################

df19 <- nh_booking %>% filter(fy == 2019) %>% select(county, id, booking_id, booking_type_standard, pc_hold_in_booking) %>% distinct()
df20 <- nh_booking %>% filter(fy == 2020) %>% select(county, id, booking_id, booking_type_standard, pc_hold_in_booking) %>% distinct()
df21 <- nh_booking %>% filter(fy == 2021) %>% select(county, id, booking_id, booking_type_standard, pc_hold_in_booking) %>% distinct()

temp19 <- df19 %>% select("booking_type_standard", booking_id) %>% distinct()

# custom functions to find the number of booking types by fiscal year
df_booking <- fnc_variable_table(df19, df20, df21, "booking_type_standard")
df_booking <- fnc_variable_table_desc(df_booking)
df_booking <- df_booking %>% filter(variable_name != "Total") %>%
  select(booking_type = variable_name, everything())

# create reactable table of number/freq of booking types by fiscal year and for all 3 years
# 7,654
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
nh_booking_types
