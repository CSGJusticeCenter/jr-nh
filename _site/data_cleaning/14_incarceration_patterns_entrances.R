############################################
# Project: JRI New Hampshire
# File: incarceration_patterns.R
# Last updated: October 12, 2022
# Author: Mari Roberts

# Tables, graphs, and numbers for incarceration patterns page
# Focuses on entrances and los

# Notes:
# Entrances = entrances and PC holds
# entrances  = entered for a criminal charge
# A PC hold is technically not a entrance
############################################

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# PEOPLE ENTERED INTO JAIL
# Includes PC holds

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

####################

# by state

####################

# Total number of people entered (no duplicates for counting by FY)
amt_people_entered <- bookings_entrances %>%
  dplyr::ungroup() %>%
  dplyr::select(id, county) %>%
  dplyr::distinct() %>%
  dplyr::group_by() %>%
  dplyr::summarise(total = n()) %>%
  dplyr::mutate(label = formatC(total, format="d", big.mark=","))
  amt_people_entered <- amt_people_entered$total

# Df of total number of people entered by FY (some duplicates because it's by FY)
df_people_entered_pre <- bookings_entrances %>%
  dplyr::ungroup() %>%
  dplyr::select(id, fy, county) %>%
  dplyr::distinct() %>%
  dplyr::group_by(fy) %>%
  dplyr::summarise(total = n()) %>%
  dplyr::mutate(label = formatC(total, format="d", big.mark=","))

# Transpose and format to wide version
df_people_entered <- df_people_entered_pre %>%
  select(-label) %>%
  t %>% as.data.frame() %>%
  row_to_names(1) %>%
  mutate(`2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`),
         `2021` = as.numeric(`2021`)) %>%
  mutate(total = amt_people_entered)
  df_people_entered <- tibble::rownames_to_column(df_people_entered, "variable_name")
  df_people_entered <- df_people_entered %>%
  mutate(variable_name = ifelse(variable_name == "total", "# Unique People entered into Jail", ""))

# count number of people entered for all three years / accounts for double counting by FY-accurate
amt_people_entered <- format(round(as.numeric(amt_people_entered), 0), nsmall=0, big.mark=",")

##########

# create reactable table showing number of people entered by FY and total

##########

# one row table showing number of people entered by FY and total
row_people_entered <-
  reactable(df_people_entered,
            pagination = FALSE,
            style = list(fontFamily = "Franklin Gothic Book", fontSize = "1.0rem"),
            theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"),
                                   headerStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
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
v1 <- as.numeric(filter(df_people_entered_pre, fy==2019) %>% select(total))
v2 <- as.numeric(filter(df_people_entered_pre, fy==2020) %>% select(total))
v3 <- as.numeric(filter(df_people_entered_pre, fy==2021) %>% select(total))
change_19_20_people_entered <- (v2 - v1)/v1
change_19_20_people_entered <- round(change_19_20_people_entered*100, 1)
change_20_21_people_entered <- (v3 - v2)/v2
change_20_21_people_entered <- round(change_20_21_people_entered*100, 1)
change_19_21_people_entered <- (v3 - v1)/v1
change_19_21_people_entered <- round(change_19_21_people_entered*100, 1)

####################

# by county

####################

# Number of people entered by county for all three years
amt_people_entered_county <- bookings_entrances %>%
  dplyr::ungroup() %>%
  dplyr::select(id, county) %>%
  dplyr::distinct() %>%
  dplyr::group_by(county) %>%
  dplyr::summarise(total = n())

# Df of total number of people entered by FY
df_people_entered_county <- bookings_entrances %>%
  dplyr::ungroup() %>%
  dplyr::select(id, fy, county) %>%
  dplyr::distinct() %>%
  dplyr::group_by(fy, county) %>%
  dplyr::summarise(total = n()) %>%
  spread(fy, total) %>%
  mutate(`2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`),
         `2021` = as.numeric(`2021`)) %>%
  left_join(amt_people_entered_county, by = "county") %>%
  adorn_totals("row") %>%
  mutate(change_19_21 = (`2021`-`2019`)/`2019`) %>%
  mutate(county = case_when(county == "Coos" ~ "Coos (bookings only)",
                            county == "Total" ~ "State",
                            TRUE ~ county))

# Table showing the number of people entered by FY by county
table_people_entered_fy_county <- fnc_reactable_county_fy(df_people_entered_county, row_num = 10)

##########

# ggplot bar chart showing the number of people entered by FY

##########

# Data for ggplot showing the number of people entered by FY
df_people_entered_long <- df_people_entered %>% select(-total)
df_people_entered_long <- gather(df_people_entered, fy, total, `2019`:`2021`, factor_key=TRUE)
df_people_entered_long <- df_people_entered_long %>% mutate(fy = as.numeric(fy)) %>%
  mutate(fy = case_when(fy == 1 ~ "2019", fy == 2 ~ "2020", fy == 3 ~ "2021"))

# gg plot showing the number of people entered by FY
gg_people_entered <-
  ggplot(data=df_people_entered_long, aes(x=fy, y=total)) +
  geom_bar(stat="identity", width = 0.74, fill = jri_orange) +
  xlab("") + ylab("People entered") +
  geom_text(aes(label = comma(total)), color = "black", vjust = -1, size = 7.5, family = "Franklin Gothic Book") +
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3, big.mark = ","),
                     expand = c(0,0),
                     limits = c(0,24000)) +
  theme_no_axes

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

# NUMBER OF ENTRANCES

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

####################

# by state

####################

# Calculate number of entrances events per fy
df_entrances_events <- bookings_entrances %>%
  select(id, booking_id, fy, county) %>%
  distinct() %>%
  distinct() %>%
  group_by(fy) %>%
  dplyr::summarise(total = n()) %>%
  mutate(label = formatC(total, format="d", big.mark=","))

# Df showing number of entrances by FY
df_entrances <- df_entrances_events %>%
  select(-label) %>%
  t %>% as.data.frame() %>%
  row_to_names(1) %>%
  mutate(`2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`),
         `2021` = as.numeric(`2021`)) %>%
  mutate(total = `2019` + `2020` + `2021`)
  df_entrances <- tibble::rownames_to_column(df_entrances, "variable_name")
  df_entrances <- df_entrances %>%
  mutate(variable_name = ifelse(variable_name == "total", "# of entrances", ""))

# Count number of entrances for three years
amt_entrances <- df_entrances$total
amt_entrances <- format(round(as.numeric(amt_entrances), 0), nsmall=0, big.mark=",")

##########

# Reactable table

##########

# One row table showing the number of entrances by FY and total
row_entrances_fy <-
  reactable(df_entrances,
            pagination = FALSE,
            style = list(fontFamily = "Franklin Gothic Book", fontSize = "1.0rem"),
            theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"),
                                   headerStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
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

# data for ggplot showing the number of entrances by FY
data1 <- df_entrances %>% select(-total)
data1 <- gather(data1, fy, total, `2019`:`2021`, factor_key=TRUE) %>%
  mutate(fy = as.numeric(fy)) %>%
  mutate(fy = case_when(fy == 1 ~ "2019", fy == 2 ~ "2020", fy == 3 ~ "2021"))

# ggplot showing the number of entrances by FY
PRES_gg_entrances <-
  ggplot(data=data1, aes(x=fy, y=total)) +
  geom_bar(stat="identity", width = 0.74, fill = jri_green) +
  xlab("") + ylab("Number of Entrances") +
  geom_text(aes(label = comma(total)), color = "black", vjust = -1, size = 7.5, family = "Franklin Gothic Book") +
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3, big.mark = ","),
                     expand = c(0,0),
                     limits = c(0,24000)) +
  theme_no_axes

####################

# by County

####################

# Number of entrances by county for all three years
amt_entrances_county <- bookings_entrances %>%
  dplyr::ungroup() %>%
  dplyr::select(booking_id, county) %>%
  dplyr::distinct() %>%
  dplyr::group_by(county) %>%
  dplyr::summarise(total = n())

# Df of total number of entrances by FY
df_entrances_county <- bookings_entrances %>%
  dplyr::ungroup() %>%
  dplyr::select(booking_id, fy, county) %>%
  dplyr::distinct() %>%
  dplyr::group_by(fy, county) %>%
  dplyr::summarise(total = n()) %>%
  spread(fy, total) %>%
  mutate(`2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`),
         `2021` = as.numeric(`2021`)) %>%
  # mutate(total = `2019` + `2020` + `2021`) %>% # this total isn't accurate because it's double counting people
  left_join(amt_entrances_county, by = "county")

df_entrances_county <- df_entrances_county %>%
  adorn_totals("row") %>%
  mutate(change_19_21 = (`2021`-`2019`)/`2019`) %>%
  mutate(county = case_when(county == "Coos" ~ "Coos (bookings only)",
                            county == "Total" ~ "State",
                            TRUE ~ county))

# Table showing the number of people entered by FY by county
PRES_table_entrances_fy_county <- fnc_reactable_county_fy(df_entrances_county, row_num = 10)

# # Number of entrances by county for all three years
# data1 <- bookings_entrances %>%
#   dplyr::ungroup() %>%
#   dplyr::select(fy, booking_id, county) %>%
#   dplyr::distinct() %>%
#   dplyr::group_by(fy, county) %>%
#   dplyr::summarise(total = n())
#
# ggplot(data = data1, aes(x=fy, y=total, group=county, color=county)) +
#   geom_line() +
#   # geom_text(aes(label = comma(total)), color = "black", position = position_dodge(0.9), vjust = -0.5,
#   #           size = 7.5, family = "Franklin Gothic Book") +
#   scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3, big.mark = ","),
#                      expand = c(0,0)
#                      #limits = c(0,14000)
#                      ) +
#   #scale_fill_manual(values=c(jri_light_blue,jri_orange), labels = c("Non-PC","PC")) +
#   theme_axes +
#   theme(#legend.position = c(0.75,0.85),
#         legend.title=element_blank(),
#         axis.title.y = element_blank())

#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################

# COMBINE ENTRANCES AND NUMBER OF PEOPLE IN ONE TABLE

#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################

# Average number of entrances/fy by county
df_avg_entrances_county <- bookings_entrances %>%
  ungroup() %>%
  select(county, id, num_bookings) %>%
  distinct() %>%
  group_by(county) %>%
  dplyr::summarize(avg_entrances = mean(num_bookings, na.rm=TRUE)) %>%
  mutate(county = case_when(county == "Coos" ~ "Coos (bookings only)", TRUE ~ county))

# Average number of entrances/fy by state
df_avg_entrances_total <- bookings_entrances %>%
  ungroup() %>%
  select(id, num_bookings) %>%
  distinct() %>%
  group_by() %>%
  dplyr::summarize(avg_entrances = mean(num_bookings, na.rm=TRUE)) %>%
  mutate(county = "State")

# Add county and state info together
df_avg_entrances_county <- rbind(df_avg_entrances_county, df_avg_entrances_total)

# Rename variables and add data together
df_entrances_county <- df_entrances_county %>% rename_with(~paste0("entrances_", .), -c("county"))
df_entrances_people_county <- df_people_entered_county %>% rename_with(~paste0("people_entered_", .), -c("county"))
df_entrances_table <- merge(df_entrances_county, df_entrances_people_county, by = "county", all.x = TRUE, all.y = TRUE)
df_entrances_table <- merge(df_entrances_table, df_avg_entrances_county, by = "county", all.x = TRUE, all.y = TRUE)

# Remove variables to condense table
data1 <- df_entrances_table %>% select(county,
                                       people_entered_total,
                                       people_entered_change_19_21,
                                       entrances_total,
                                       entrances_change_19_21,
                                       avg_entrances
                                       ) %>%
  mutate(avg_entrances = round(avg_entrances, 1)) %>%
  arrange(county %in% "State")

# Table showing the number of entrances and number of people by county
PRES_table_entrances_people_county <-
  reactable(data1,
            pagination = FALSE,
            style = list(fontFamily = "Franklin Gothic Book", fontSize = "1.0rem"),
            rowStyle = function(index) {
              if (index %in% c(10)) {
                list(`border-top` = "thin solid",
                     fontWeight = "bold")
              }
            },
            # columnGroups = list(
            #   colGroup(name = "Number of People",    columns = c("people_entered_total", "people_entered_change_19_21")),
            #   colGroup(name = "Number of Entrances", columns = c("entrances_total", "entrances_change_19_21", "avg_entrances"))
            # ),
            theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"),
                                   headerStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
            defaultColDef = reactable::colDef(
              format = colFormat(separators = TRUE), align = "center"
            ),
            compact = TRUE,
            fullWidth = FALSE,
            columns = list(
              `county`                    = colDef(align = "left", minWidth = 180, name = "County", style = list(fontWeight = "bold")),
              people_entered_total        = colDef(minWidth = 90,  name = "Total People"),
              people_entered_change_19_21 = colDef(show = F, minWidth = 90,  name = "Change Entrances\n2019-2021", format = colFormat(percent = TRUE, digits = 1),
                                                   style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
              entrances_total             = colDef(minWidth = 90,  name = " Total Entrances"),
              entrances_change_19_21      = colDef(show = F, minWidth = 100,  name = "Entrances Change from 19-21", format = colFormat(percent = TRUE, digits = 1)),
              avg_entrances             = colDef(minWidth = 120,  name = "Average Number of Entrances Per Person")
              ))

################################################################################

# Number of Entrances Per Person

################################################################################

min(bookings_entrances$num_bookings)
max(bookings_entrances$num_bookings)

data1 <- bookings_entrances %>% ungroup() %>% select(id, num_bookings) %>% distinct() %>%
  mutate(num_bookings_category = case_when(num_bookings == 1 ~ "1",
                                           num_bookings == 2 ~ "2",
                                           num_bookings == 3 ~ "3",
                                           num_bookings == 4 ~ "4",
                                           num_bookings == 5 ~ "5",
                                           num_bookings  > 5 ~ "6+")) %>%
  mutate(num_bookings_category = factor(num_bookings_category,
                                        levels = c("1",
                                                   "2",
                                                   "3",
                                                   "4",
                                                   "5",
                                                   "6+")))

# Histogram showing the frequency of the number of entrances per person
PRES_gg_num_entrances <- ggplot(data1, aes(x = num_bookings_category)) +
  geom_bar(width = 0.74, fill = jri_dark_blue) +
  scale_y_continuous(labels = label_number(big.mark = ","),
                     expand = c(0,0),
                     limits = c(0,26500)) +
  theme_axes +
  xlab("\nNumber of Entrances Per Person") + ylab("Count\n")

# different version (goes up to 20+ instead of 6+)

# subset data and create categories for number of entrances
data1 <- bookings_entrances %>%
  ungroup() %>%
  select(id, num_bookings) %>%
  distinct() %>%
  mutate(num_bookings_category = case_when(num_bookings == 1 ~ "1",
                                           num_bookings == 2 ~ "2",
                                           num_bookings == 3 ~ "3",
                                           num_bookings == 4 ~ "4",
                                           num_bookings == 5 ~ "5",
                                           num_bookings == 6 ~ "6",
                                           num_bookings == 7 ~ "7",
                                           num_bookings == 8 ~ "8",
                                           num_bookings == 9 ~ "9",
                                           num_bookings == 10 ~ "10",
                                           num_bookings == 11 ~ "11",
                                           num_bookings == 12 ~ "12",
                                           num_bookings == 13 ~ "13",
                                           num_bookings == 14 ~ "14",
                                           num_bookings == 15 ~ "15",
                                           num_bookings == 16 ~ "16",
                                           num_bookings == 17 ~ "17",
                                           num_bookings == 18 ~ "18",
                                           num_bookings == 19 ~ "19",
                                           num_bookings >= 20 ~ "20+")) %>%
  mutate(num_bookings_category = factor(num_bookings_category,
                                        levels = c("1",
                                                   "2",
                                                   "3",
                                                   "4",
                                                   "5",
                                                   "6",
                                                   "7",
                                                   "8",
                                                   "9",
                                                   "10",
                                                   "11",
                                                   "12",
                                                   "13",
                                                   "14",
                                                   "15",
                                                   "16",
                                                   "17",
                                                   "18",
                                                   "19",
                                                   "20+")))

# Histogram showing the frequency of the number of entrances per person
PRES_gg_num_entrances <- ggplot(data1, aes(x = num_bookings_category)) +
  geom_bar(width = 0.74, fill = jri_green) +
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3, big.mark = ","),
                     expand = c(0,0),
                     limits = c(0,26500)) +
  theme_axes +
  xlab("\nNumber of Entrances") + ylab("Number of People\n")

##########

# Save data

##########

save(amt_people_entered,              file=paste0(sp_data_path, "/Data/r_data/amt_people_entered.Rda",              sep = ""))
save(row_people_entered,              file=paste0(sp_data_path, "/Data/r_data/row_people_entered.Rda",              sep = ""))
save(amt_entrances,                   file=paste0(sp_data_path, "/Data/r_data/amt_entrances.Rda",                   sep = ""))
save(row_entrances_fy,                file=paste0(sp_data_path, "/Data/r_data/row_entrances_fy.Rda",                sep = ""))
save(PRES_gg_entrances,               file=paste0(sp_data_path, "/Data/r_data/PRES_gg_entrances.Rda",               sep = ""))
save(PRES_table_entrances_fy_county,  file=paste0(sp_data_path, "/Data/r_data/PRES_table_entrances_fy_county.Rda",  sep = ""))
save(PRES_table_entrances_people_county, file=paste0(sp_data_path, "/Data/r_data/PRES_table_entrances_people_county.Rda",  sep = ""))
save(PRES_gg_num_entrances,  file=paste0(sp_data_path, "/Data/r_data/PRES_gg_num_entrances.Rda",  sep = ""))
