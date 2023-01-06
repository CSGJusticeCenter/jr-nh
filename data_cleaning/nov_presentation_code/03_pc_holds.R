############################################
# Project: JRI New Hampshire
# File: pc_holds.R
# Last updated: October 19, 2022
# Author: Mari Roberts

# Tables, graphs, and numbers for incarceration patterns page, pc hold section
############################################

################################################################################

# How protective custody holds are recorded across counties

################################################################################

# Select PC hold recordings to show how each county records pc holds (charge_desc, booking_type, sentence_status, release_type).
df_county_pc_hold_recordings <- adm_all %>%
  dplyr::filter(pc_hold == "PC Hold") %>%
  dplyr::group_by(county, charge_desc, booking_type, sentence_status, release_type) %>%
  dplyr::summarise(total = n()) %>%
  dplyr::mutate(total = formattable::comma(total, digits = 0))

################################################################################

# Highchart pc holds over time
# Remove Coos and Strafford because they don't have data on PC holds. Coos deletes the entire PC hold entry.

################################################################################

# filter to PC holds
data1 <- df_pch %>% filter(pc_hold_in_booking == "PC Hold")

# generate high chart using custom function
hc_pch_time <- fnc_covid_time_highchart(data1, yaxis_label = "Number of PC Holds", title = NULL, jri_light_blue)

################################################################################

# Table pc holds by FY

################################################################################

# Filter pc data by year
data_19 <- df_pch %>% select(county, id, booking_id, fy, pc_hold_in_booking) %>% distinct() %>% filter(fy == 2019)
data_20 <- df_pch %>% select(county, id, booking_id, fy, pc_hold_in_booking) %>% distinct() %>% filter(fy == 2020)
data_21 <- df_pch %>% select(county, id, booking_id, fy, pc_hold_in_booking) %>% distinct() %>% filter(fy == 2021)

# Df showing PC holds from 2019-2021
data1 <- fnc_variable_table(data_19, data_20, data_21, "pc_hold_in_booking")
data1 <- data1 %>% dplyr::rename(pc_hold_in_booking = variable_name)
data1[is.na(data1)] = 0
data1 <- data1 %>% filter(pc_hold_in_booking != "Total")

# % of entrances (bookings) that are PC holds
amt_pch_pct <- data1 %>% filter(pc_hold_in_booking == "PC Hold")
amt_pch_pct <- amt_pch_pct$freq*100
amt_pch_pct <- round(amt_pch_pct, 1)

# Create reactable table for pc holds by FY
table_pch <- fnc_reactable_fy(data1, metric_label = " ", label_width = 150, note = "Coos and Strafford are included in the NA line of the table above. Coos removes entrances that are PC holds so Coos's administrative data is not used to calculate the proportion of entrances that are PC holds or Non-PC Holds. Strafford's data cannot differentiate between bookings and PC holds so their administrative data is also excluded in these calculations.")

################################################################################

# Table pc holds by FY by county

################################################################################

# Select variables and count number of pc holds vs non-pc holds by county by FY
df_pc_holds_fy_county <- df_pch %>%
  select(id, booking_id, fy, county, pc_hold_in_booking) %>%
  distinct() %>%
  group_by(fy, county, pc_hold_in_booking) %>%
  dplyr::summarise(total = n())

# Reshape table for viewing
df_pc_holds_fy_county <- df_pc_holds_fy_county %>% spread(pc_hold_in_booking, total) %>% clean_names()
df_pc_holds_fy_county <- dcast(setDT(df_pc_holds_fy_county), county~fy, value.var=c('non_pc_hold', 'pc_hold'))

# Calculate % of bookings that pc holds by county by FY
df_pc_holds_fy_county <- df_pc_holds_fy_county %>%
  mutate(pc_hold_pct_2019 = pc_hold_2019/(non_pc_hold_2019 + pc_hold_2019),
         pc_hold_pct_2020 = pc_hold_2020/(non_pc_hold_2020 + pc_hold_2020),
         pc_hold_pct_2021 = pc_hold_2021/(non_pc_hold_2021 + pc_hold_2021),
         pc_hold_total = pc_hold_2019 + pc_hold_2020 + pc_hold_2021,
         non_pc_hold_total = non_pc_hold_2019 + non_pc_hold_2020 + non_pc_hold_2021) %>%
  mutate(freq = pc_hold_total/(non_pc_hold_total + pc_hold_total)) %>%
  select(county,
         count_19 = pc_hold_2019,
         pct_19   = pc_hold_pct_2019,
         count_20 = pc_hold_2020,
         pct_20   = pc_hold_pct_2020,
         count_21 = pc_hold_2021,
         pct_21   = pc_hold_pct_2021,
         total    = pc_hold_total,
         freq)

# Remove Coos and Strafford from table
data1 <- df_pc_holds_fy_county %>% filter(county != "Coos" & county != "Strafford") %>% droplevels()

# Table showing the number and proportion of PC holds each FY by county
table_pc_holds_fy_county <- fnc_reactable_fy(data1, metric_label = " ", label_width = 150, note = "Coos removes entrances that are PC holds so Coos's administrative data is not included in this table. Strafford's data cannot differentiate between bookings and PC holds so their administrative data is also excluded.")

################################################################################

# Reactable table with number of entrances and proportion that are PC holds
# Includes Coos and Strafford in the table but not the calculation

################################################################################

##########

# Number of people entering for a pc hold by county

##########

# Select variables and count number of pc holds people had by county by FY
df_pc_people_fy_county <- df_pch %>%
  ungroup() %>%
  filter(pc_hold_in_booking == "PC Hold" & county != "Strafford" & county != "Coos") %>%
  select(id, county) %>%
  distinct() %>%
  group_by(county) %>%
  summarise(total_people = n()) %>%
  adorn_totals("row") %>%
  mutate(county = case_when(county == "Total" ~ "State", TRUE ~ county)) %>%
  arrange(county %in% "State")

##########

# Total and freq of pc hold entrances by FY and county

##########

# Data for total booking/entrances by FY and county
df_entrances_county1 <- bookings_entrances %>%
  select(id, booking_id, fy, county) %>%
  distinct() %>%
  group_by(fy, county) %>%
  dplyr::summarise(total = n()) %>%
  spread(fy, total) %>%
  mutate(`2019` = as.numeric(`2019`),
         `2020` = as.numeric(`2020`),
         `2021` = as.numeric(`2021`)) %>%
  mutate(total = `2019` + `2020` + `2021`) %>%
  select(county, total_entrances = total)

# Data for total booking/entrances for Coos and Strafford
# Separate out so they aren't included in calculations
df_entrances_Coos_Strafford <- df_entrances_county1 %>%
  filter(county == "Coos" | county == "Strafford") %>%
  mutate(total_pc_holds = NA,
         freq = NA)

# Remaining data with other counties
df_entrances_county1 <- df_entrances_county1 %>%
  ungroup() %>%
  filter(county != "Coos" & county != "Strafford") %>%
  droplevels()

# Arrange data and get totals per row
df_entrances_with_pc_holds <- df_pc_holds_fy_county %>%
  select(county, total_pc_holds = total, freq) %>%
  left_join(df_entrances_county1, by = "county") %>%
  select(county, total_entrances, everything()) %>%
  arrange(county) %>%
  adorn_totals("row")

# Combine Coos and Strafford numbers
df_entrances_with_pc_holds <- df_entrances_with_pc_holds %>%
  mutate(freq = case_when(county == "Total" ~ (filter(df_entrances_with_pc_holds, county=='Total')$total_pc_holds)/(filter(df_entrances_with_pc_holds, county=='Total')$total_entrances),
                          TRUE ~ freq)) %>%
  full_join(df_entrances_county1, by = c("county", "total_entrances")) %>%
  arrange(county) %>%
  mutate(total_entrances = case_when(county == "Total" ~ sum(total_entrances) - filter(df_entrances_with_pc_holds, county=='Total')$total_entrances,
                                    TRUE ~ total_entrances))

df_entrances_with_pc_holds <- rbind(df_entrances_with_pc_holds, df_entrances_Coos_Strafford)

# Add totals and arrange table data
# Add total number of people df
df_entrances_with_pc_holds <- df_entrances_with_pc_holds %>%
  mutate(total_entrances = case_when(county == "Total" ~ sum(total_entrances, na.rm = TRUE),
                                     TRUE ~ total_entrances)) %>%
  filter((county != "Coos" | county != "Strafford") & !is.na(total_entrances)) %>%
  arrange(county) %>%
  mutate(county = case_when(county == "Coos" ~ "Coos (bookings only)",
                            county == "Total" ~ "State", TRUE ~ county)) %>%
  left_join(df_pc_people_fy_county, by = "county") %>%
  arrange(county %in% "State") %>%
  select(county, total_entrances, total_pc_holds, total_people, freq)

# Reactable table showing the total number of entrances and prop of PC holds
# Includes Coos and Strafford in the table but not the calculation
PRES_table_entrances_with_pc_holds <-
  reactable(df_entrances_with_pc_holds,
            pagination = FALSE,
            style = list(fontFamily = "Franklin Gothic Book", fontSize = "1.0rem"),
            theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"),
                                   headerStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
            compact = TRUE,
            fullWidth = FALSE,
            rowStyle = function(index) {
              if (index %in% c(10)) {
                list(`border-top` = "thin solid",
                     fontWeight = "bold")
              }
            },
            defaultColDef = reactable::colDef(
              format = colFormat(separators = TRUE), align = "left"),
            columns = list(
              county          = colDef(minWidth = 180, name = "County", style = list(fontWeight = "bold")),
              total_entrances = colDef(minWidth = 100, name = "Total Entrances", align = "center"),
              total_pc_holds  = colDef(minWidth = 100, name = "PC Holds (Entrances)", align = "center"),
              total_people    = colDef(minWidth = 100, name = "PC Holds (People)", align = "center"),
              freq            = colDef(minWidth = 100, style = list(fontWeight = "bold"), name = "% PC Hold Entrances", format = colFormat(percent = TRUE, digits = 1), align = "center"))) %>%
  add_source("Coos bookings and Strafford entrances were not included when calculating the proportion of entrances that are PC holds.", font_style = "italic", font_size = 14)

##########

# ggplot showing the decrease in entrances by county

##########

# Data for ggplots showing the number of entrances and proportion of PC holds by FY
data1 <- df_pch %>%
  group_by(county, pc_hold_in_booking) %>% summarise(total = n())
data1 <- group_by(data1, county) %>% mutate(pct = round(total/sum(total)*100, 1))
data1 <- as.data.frame(data1)
data1 <- data1 %>% mutate(pct = round(pct, digits = 0)) %>%
  mutate(pct = paste0(pct, "%"))

PRES_gg_pchold_prop <- ggplot(data1, aes(x = county, y = total, fill = pc_hold_in_booking)) +
  geom_col(colour = "white", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("gray",jri_red),
                    na.value = "white",
                    labels = c("Non-PC      ","PC", " ")
                    ) +
  geom_text(aes(label = pct, fontface = 'bold'),
            position = position_fill(vjust = 0.5),
            vjust = 0.8,
            size = 10, family = "Franklin Gothic Book",
            color = case_when(data1$pc_hold_in_booking == "Non-PC Hold" ~ "black",
                              data1$county == "Hillsborough" ~ "white",
                              TRUE ~ "white")) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.85, size = 28, color = "black"),
        axis.text.y = element_text(size = 28, color = "black"),
        legend.position = "right",
        legend.justification = c(1, 0.5),
        legend.title=element_blank(),
        legend.text = element_text(family = "Franklin Gothic Book", size = 28, color = "black"))+
  geom_hline(aes(yintercept=0.20), size = 1)
PRES_gg_pchold_prop

##########

# ggplots showing the number of entrances and proportion of PC holds by FY

##########

# Data for ggplots showing the number of entrances and proportion of PC holds by FY
data1 <- df_pch %>%
  filter(county != "Coos" | county != "Strafford") %>%
  group_by(fy, pc_hold_in_booking) %>% summarise(total = n()) %>% filter(!is.na(pc_hold_in_booking))

# ggplot grouped chart showing the number of entrances and proportion of PC holds by FY
gg_pch_grouped_barchart <-
  ggplot(data1, aes(fill=pc_hold_in_booking, y=total, x=fy)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = comma(total)), color = "black", position = position_dodge(0.9), vjust = -0.5,
            size = 7.5, family = "Franklin Gothic Book") +
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3, big.mark = ","),
                     expand = c(0,0),
                     limits = c(0,14000)) +
  scale_fill_manual(values=c(jri_light_blue,jri_orange), labels = c("Non-PC","PC")) +
  theme_no_axes +
  theme(legend.position = c(0.75,0.85),
        legend.title=element_blank(),
        axis.title.y = element_blank())

data1 <- group_by(data1, fy) %>% mutate(pct = round(total/sum(total)*100, 1))
data1 <- as.data.frame(data1)
data1 <- data1 %>% mutate(pct = comma(pct, digits = 1)) %>% mutate(pct = paste0(pct, "%"))

# Grouped bar chart showing the proportion of PC holds by FY
PRES_gg_pch_pct_barchart <- fnc_pct_grouped_bar_chart(data1, "gray", jri_red)

##########

# Save data

##########

save(amt_pch_pct,                        file=paste0(sp_data_path, "/Data/r_data/pc_holds_page/amt_pch_pct.Rda",                        sep = ""))
save(PRES_gg_pch_pct_barchart,           file=paste0(sp_data_path, "/Data/r_data/pc_holds_page/PRES_gg_pch_pct_barchart.Rda",           sep = ""))
save(hc_pch_time,                        file=paste0(sp_data_path, "/Data/r_data/pc_holds_page/hc_pch_time.Rda",                        sep = ""))
save(PRES_table_entrances_with_pc_holds, file=paste0(sp_data_path, "/Data/r_data/pc_holds_page/PRES_table_entrances_with_pc_holds.Rda", sep = ""))
save(table_pc_holds_fy_county,           file=paste0(sp_data_path, "/Data/r_data/pc_holds_page/table_pc_holds_fy_county.Rda",           sep = ""))

# save ggplots to images folder (only works here)
ggsave(PRES_gg_pch_pct_barchart,         file="img/PRES_gg_pch_pct_barchart.png", width = 6,  height = 5, dpi = 100)
ggsave(PRES_gg_pchold_prop,              file="img/PRES_gg_pchold_prop.png",      width = 15, height = 6, dpi = 100)
