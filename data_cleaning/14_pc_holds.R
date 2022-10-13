############################################
# Project: JRI New Hampshire
# File: pc_holds.R
# Last updated: October 11, 2022
# Author: Mari Roberts

# Tables, graphs, and numbers for incarceration patterns page, pc hold section
############################################

# pch data with Coos or Strafford but the PC hold section is NA
df_pch <- pch
dim(df_pch); length(unique(df_pch$booking_id)) # 51545, 51545
table(df_pch$county)

###########
# Highchart pc holds over time
# remove Coos and Strafford because they don't have data on PC holds. Coos deletes the entire entry.
###########

# get counties included
pch_counties <- fnc_counties_in_data(df_pch)

# filter to PC holds
temp <- df_pch %>% filter(pc_hold_in_booking == "PC Hold")

# generate high chart using custom function
hc_pch_time <- fnc_covid_time_highchart(temp, yaxis_label = "Number of PC Holds", title = NULL)

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

# % of entrances (bookings) that are PC holds
amt_pch_pct <- pch_df %>% filter(pc_hold_in_booking == "PC Hold")
amt_pch_pct <- amt_pch_pct$freq*100
amt_pch_pct <- round(amt_pch_pct, 1)

# create reactable table for pc holds by fiscal year
table_pch <- fnc_reactable_fy(pch_df, metric_label = " ", label_width = 150, note = "Coos and Strafford are included in the NA line of the table above. Coos removes entrances that are PC holds so Coos's administrative data is not used to calculate the proportion of entrances that are PC holds or Non-PC Holds. Strafford's data cannot differentiate between bookings and PC holds so their administrative data is also excluded in these calculations.")

###########
# Table pc holds by FY by county
###########

# detach(package:plyr)
# select variables
# count number of pc holds vs non-pc holds by county by fiscal year
df_pc_holds_fy_county <- df_pch %>%
  select(id, booking_id, fy, county, pc_hold_in_booking) %>%
  distinct() %>%
  group_by(fy, county, pc_hold_in_booking) %>%
  dplyr::summarise(total = n())

# reshape table for viewing
df_pc_holds_fy_county <- df_pc_holds_fy_county %>% spread(pc_hold_in_booking, total) %>% clean_names()
df_pc_holds_fy_county <- dcast(setDT(df_pc_holds_fy_county), county~fy, value.var=c('non_pc_hold', 'pc_hold'))

# calculate % of bookings that pc holds by county by fiscal year
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
         freq) #%>%
  #filter(county != "Coos" & county != "Strafford") %>% droplevels()

# format into a reactable table????????????????????????????????????????????????????????? fix total
table_pc_holds_fy_county <- fnc_reactable_fy(df_pc_holds_fy_county, metric_label = " ", label_width = 150, note = "Coos removes entrances that are PC holds so Coos's administrative data is not included in this table. Strafford's data cannot differentiate between bookings and PC holds so their administrative data is also excluded.")

###########
# How protective custody holds are recorded across counties
###########

# select PC hold recordings to show how each county records pc holds (charge_desc, booking_type, sentence_status, release_type)
# df_county_pc_hold_recordings <- adm_all %>% filter(pc_hold == "PC Hold") %>%
#   select(county, charge_desc, booking_type, sentence_status, release_type) %>% distinct()
df_county_pc_hold_recordings <- adm_all %>%
  dplyr::filter(pc_hold == "PC Hold") %>%
  dplyr::group_by(county, charge_desc, booking_type, sentence_status, release_type) %>%
  dplyr::summarise(total = n()) %>%
  dplyr::mutate(total = formattable::comma(total, digits = 0))

##########

# reactable table with number of entrances and proportion that are PC holds
# includes coos and strafford in the table but not the calculation

##########

# data with number of pc holds and freq
df_entrances_with_pc_holds <- df_pc_holds_fy_county %>%
  select(county, total_pc_holds = total, freq)

# data for total booking/entrances
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

# data for total booking/entrances for coos and strafford
# separate out so they aren't included in calculations
df_entrances_coos_strafford <- df_entrances_county1 %>%
  filter(county == "Coos" | county == "Strafford") %>%
  mutate(total_pc_holds = NA,
         freq = NA)

# remaining data with other counties
df_entrances_county1 <- df_entrances_county1 %>%
  ungroup() %>%
  filter(county != "Coos" & county != "Strafford") %>%
  droplevels()

# arrange data
df_entrances_with_pc_holds <- df_entrances_with_pc_holds %>%
  left_join(df_entrances_county1, by = "county") %>%
  select(county, total_entrances, everything()) %>%
  arrange(county) %>%
  adorn_totals("row")
df_entrances_with_pc_holds <- df_entrances_with_pc_holds %>%
  mutate(freq = case_when(county == "Total" ~ (filter(df_entrances_with_pc_holds, county=='Total')$total_pc_holds)/(filter(df_entrances_with_pc_holds, county=='Total')$total_entrances),
                          TRUE ~ freq)) %>%
  full_join(df_entrances_county1, by = c("county", "total_entrances")) %>%
  arrange(county) %>%
  mutate(total_entrances = case_when(county == "Total" ~ sum(total_entrances) - filter(df_entrances_with_pc_holds, county=='Total')$total_entrances,
                                    TRUE ~ total_entrances))

# combine coos and strafford numbers
df_entrances_with_pc_holds <- rbind(df_entrances_with_pc_holds, df_entrances_coos_strafford)

# add totals and arrange table data
df_entrances_with_pc_holds <- df_entrances_with_pc_holds %>%
  mutate(total_entrances = case_when(county == "Total" ~ sum(total_entrances, na.rm = TRUE),
                                     TRUE ~ total_entrances)) %>%
  filter((county != "Coos" | county != "Strafford") & !is.na(total_entrances)) %>%
  arrange(county) %>%
  mutate(county = case_when(county == "Coos" ~ "Coos (bookings only)", TRUE ~ county))

# reactable table showing the total number of entrances and prop of PC holds
# includes coos and strafford in the table but not the calculation
table_entrances_with_pc_holds <-
  reactable(df_entrances_with_pc_holds,
            pagination = FALSE,
            style = list(fontFamily = "Franklin Gothic Book"),
            theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
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
              county          = colDef(minWidth = 150, name = "County", style = list(fontWeight = "bold")),
              total_entrances  = colDef(minWidth = 100, name = "# Entrance", align = "center"),
              total_pc_holds  = colDef(minWidth = 100, name = "# PC Holds", align = "center"),
              freq            = colDef(minWidth = 100, style = list(fontWeight = "bold"), name = "% PC Holds", format = colFormat(percent = TRUE, digits = 1), align = "center"))) %>%
  add_source("Coos bookings and Strafford entrances were not included when calculating the proportion of entrances that are PC holds.", font_style = "italic", font_size = 14)

##########

# ggplots showing the number of entrances and proportion of PC holds by FY

##########

# data for ggplots showing the number of entrances and proportion of PC holds by FY
temp <- df_pch %>%
  filter(county != "Coos" | county != "Strafford") %>%
  group_by(fy, pc_hold_in_booking) %>% summarise(total = n()) %>% filter(!is.na(pc_hold_in_booking))

# ggplot grouped chart showing the number of entrances and proportion of PC holds by FY
gg_pch_grouped_barchart <-
  ggplot(temp, aes(fill=pc_hold_in_booking, y=total, x=fy)) +
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

temp2 <- group_by(temp, fy) %>% mutate(pct = round(total/sum(total)*100, 1))
temp2 <- as.data.frame(temp2)
temp2 <- temp2 %>% mutate(pct = comma(pct, digits = 1)) %>% mutate(pct = paste0(pct, "%"))

gg_pch_pct_barchart <- fnc_pct_grouped_bar_chart(temp2, "gray", jri_red)

################################################################################

# Save to SP

################################################################################

# protective custody holds
save(hc_pch_time,                   file=paste0(sp_data_path, "/Data/r_data/hc_pch_time.Rda",                   sep = ""))
save(table_pch,                     file=paste0(sp_data_path, "/Data/r_data/table_pch.Rda",                     sep = ""))
save(amt_pch_pct,                   file=paste0(sp_data_path, "/Data/r_data/amt_pch_pct.Rda",                   sep = ""))
save(pch_counties,                  file=paste0(sp_data_path, "/Data/r_data/pch_counties.Rda",                     sep = ""))
save(table_pc_holds_fy_county,      file=paste0(sp_data_path, "/Data/r_data/table_pc_holds_fy_county.Rda",      sep = ""))
save(df_county_pc_hold_recordings,  file=paste0(sp_data_path, "/Data/r_data/df_county_pc_hold_recordings.Rda",     sep = ""))
save(gg_pch_grouped_barchart,       file=paste0(sp_data_path, "/Data/r_data/gg_pch_grouped_barchart.Rda",       sep = ""))
save(gg_pch_pct_barchart,           file=paste0(sp_data_path, "/Data/r_data/gg_pch_pct_barchart.Rda",           sep = ""))
save(table_entrances_with_pc_holds, file=paste0(sp_data_path, "/Data/r_data/table_entrances_with_pc_holds.Rda", sep = ""))
