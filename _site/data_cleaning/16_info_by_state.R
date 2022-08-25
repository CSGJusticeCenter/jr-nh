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

##############################
# PC HOLDS
##############################

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
df <- fnc_pc_hold_table(pch_19, pch_20, pch_21)

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

save(nh_pch_time_highchart, file=paste0(sp_data_path, "/Data/r_data/nh_pch_time_highchart.Rda", sep = ""))
save(nh_pch_table,          file=paste0(sp_data_path, "/Data/r_data/nh_pch_table.Rda",          sep = ""))
save(nh_pch_pct_amt,        file=paste0(sp_data_path, "/Data/r_data/nh_pch_pct_amt.Rda",        sep = ""))
save(pch_counties,          file=paste0(sp_data_path, "/Data/r_data/pch_counties.Rda",          sep = ""))

