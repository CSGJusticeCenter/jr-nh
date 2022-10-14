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

################################################################################

# PEOPLE ENTERED INTO JAIL
# Includes PC holds

################################################################################

####################

# by state

####################

# table of total number of people entered (no duplicates for counting by FY)
amt_people_entered <- bookings_entrances %>%
  dplyr::ungroup() %>%
  dplyr::select(id, county) %>%
  dplyr::distinct() %>%
  dplyr::group_by() %>%
  dplyr::summarise(total = n()) %>%
  dplyr::mutate(label = formatC(total, format="d", big.mark=","))
  amt_people_entered <- amt_people_entered$total

# table of total number of people entered by FY (some duplicates because it's by FY)
df_people_entered_pre <- bookings_entrances %>%
  dplyr::ungroup() %>%
  dplyr::select(id, fy, county) %>%
  dplyr::distinct() %>%
  dplyr::group_by(fy) %>%
  dplyr::summarise(total = n()) %>%
  dplyr::mutate(label = formatC(total, format="d", big.mark=","))

# transpose and format, wide version
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

# one row table minimal showing number of people entered by FY and total
table_people_entered <-
  reactable(df_people_entered,
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

##########

# create reactable table showing number people entered by FY and total

##########

# number of people entered by county for all three years
amt_people_entered_county <- bookings_entrances %>%
  dplyr::ungroup() %>%
  dplyr::select(id, county) %>%
  dplyr::distinct() %>%
  dplyr::group_by(county) %>%
  dplyr::summarise(total = n())

# df of total number of people entered by FY
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

# table showing the number of people entered by FY by county
PRES_table_people_entered_fy_county <- fnc_reactable_county_fy(df_people_entered_county, row_num = 10)

##########

# ggplot bar chart showing the number of people entered by FY

##########

# data for ggplot showing the number of people entered by FY
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


#################################################################################################################################################
#################################################################################################################################################

# NUMBER OF ENTRANCES

#################################################################################################################################################
#################################################################################################################################################

####################

# by state

####################

##########

# ggplot bar chart showing the number of entrances by FY

##########

# calculate number of entrances events per fy
df_entrances_events <- bookings_entrances %>%
  select(id, booking_id, fy, county) %>%
  distinct() %>%
  distinct() %>%
  group_by(fy) %>%
  dplyr::summarise(total = n()) %>%
  mutate(label = formatC(total, format="d", big.mark=","))

# create table showing number of entrances by fiscal year
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

# count number of entrances for three years
amt_entrances <- df_entrances$total
amt_entrances <- format(round(as.numeric(amt_entrances), 0), nsmall=0, big.mark=",")

##########

# Reactable table

##########

# one row minimal table showing the number of entrances by FY and total
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

##########

# PRESENTATION - ggplot showing the number of entrances by FY

##########

# data for ggplot showing the number of entrances by FY
df1 <- df_entrances %>% select(-total)
df1 <- gather(df1, fy, total, `2019`:`2021`, factor_key=TRUE) %>%
  mutate(fy = as.numeric(fy)) %>%
  mutate(fy = case_when(fy == 1 ~ "2019", fy == 2 ~ "2020", fy == 3 ~ "2021"))

# ggplot showing the number of entrances by FY
PRES_gg_entrances <-
  ggplot(data=df1, aes(x=fy, y=total)) +
  geom_bar(stat="identity", width = 0.74, fill = jri_green) +
  xlab("") + ylab("Number of entrances") +
  geom_text(aes(label = comma(total)), color = "black", vjust = -1, size = 7.5, family = "Franklin Gothic Book") +
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3, big.mark = ","),
                     expand = c(0,0),
                     limits = c(0,24000)) +
  theme_no_axes

####################

# by County

####################

##########

# create reactable table showing number entrances by FY and total

##########

# number of entrances by county for all three years
amt_entrances_county <- bookings_entrances %>%
  dplyr::ungroup() %>%
  dplyr::select(booking_id, county) %>%
  dplyr::distinct() %>%
  dplyr::group_by(county) %>%
  dplyr::summarise(total = n())

# df of total number of people entered by FY
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

# table showing the number of people entered by FY by county
PRES_table_entrances_fy_county <- fnc_reactable_county_fy(df_entrances_county, row_num = 10)

#################################################################################################################################################
#################################################################################################################################################

# COMBINE ENTRANCES AND NUMBER OF PEOPLE IN ONE TABLE

#################################################################################################################################################
#################################################################################################################################################

# average number of entrances/fy by county
df_avg_entrances_county <- bookings_entrances %>%
  select(county, booking_id, num_bookings, fy) %>%
  distinct() %>%
  group_by(county) %>%
  dplyr::summarize(avg_entrances_fy = mean(num_bookings, na.rm=TRUE)) %>%
  mutate(county = case_when(county == "Coos" ~ "Coos (bookings only)", TRUE ~ county))
df_avg_entrances_total <- bookings_entrances %>%
  select(booking_id, num_bookings, fy) %>%
  distinct() %>%
  group_by() %>%
  dplyr::summarize(avg_entrances_fy = mean(num_bookings, na.rm=TRUE)) %>%
  mutate(county = "State")
df_avg_entrances_county <- rbind(df_avg_entrances_county, df_avg_entrances_total)

# rename variables and add data together
df_entrances_county <- df_entrances_county %>% rename_with(~paste0("entrances_", .), -c("county"))
df_entrances_people_county <- df_people_entered_county %>% rename_with(~paste0("people_entered_", .), -c("county"))
df_entrances_table <- merge(df_entrances_county, df_entrances_people_county, by = "county", all.x = TRUE, all.y = TRUE)
df_entrances_table <- merge(df_entrances_table, df_avg_entrances_county, by = "county", all.x = TRUE, all.y = TRUE)

# remove variables to condense table
data1 <- df_entrances_table %>% select(county,
                                       people_entered_total,
                                       people_entered_change_19_21,
                                       entrances_total,
                                       entrances_change_19_21,
                                       avg_entrances_fy
                                       ) %>%
  mutate(avg_entrances_fy = round(avg_entrances_fy, 1)) %>%
  arrange(county %in% "State")

PRES_table_entrances_people_county <-
  reactable(data1,
            pagination = FALSE,
            style = list(fontFamily = "Franklin Gothic Book"),
            rowStyle = function(index) {
              if (index %in% c(10)) {
                list(`border-top` = "thin solid",
                     fontWeight = "bold")
              }
            },
            columnGroups = list(
              colGroup(name = "Number of People",    columns = c("people_entered_total", "people_entered_change_19_21")),
              colGroup(name = "Number of Entrances", columns = c("entrances_total", "entrances_change_19_21"))
            ),
            theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
            defaultColDef = reactable::colDef(
              format = colFormat(separators = TRUE), align = "center"
            ),
            compact = TRUE,
            fullWidth = FALSE,
            columns = list(
              `county`                    = colDef( align = "left", minWidth = 180, name = "County", style = list(fontWeight = "bold")),
              people_entered_total        = colDef(minWidth = 100,  name = "Total People"),
              people_entered_change_19_21 = colDef(minWidth = 120,  name = "Change from 2019-2021", format = colFormat(percent = TRUE, digits = 1),
                                                   style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
              entrances_total             = colDef(minWidth = 100,  name = "Total Entrances"),
              entrances_change_19_21      = colDef(minWidth = 120,  name = "Change from 2019-2021", format = colFormat(percent = TRUE, digits = 1)),
              avg_entrances_fy             = colDef(minWidth = 180,  name = "Avgerage Number of Entrances Per Person/Year")

              ))
