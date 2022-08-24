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

df <- nh_booking %>%
  mutate(pc_hold = as.numeric(pc_hold)) %>%
  mutate(pc_hold = ifelse(pc_hold == 2, 1, 0))

fnc_pch_time_highchart(df)



# filter to PC holds change this to proportions
df1 <- df %>% filter(pc_hold == 1)

  df1 <- df1 %>%
    dplyr::group_by(month_year, month_year_text, county) %>%
    dplyr::summarise(total = n())
  df1 <- df1 %>%
    mutate(tooltip = paste0("<b>", month_year_text, "</b><br>","Total: ", total, "<br>"))

  chart <- df1 %>%
    hchart('line', hcaes(x = month_year, y = total, group = county)) %>%
    hc_setup() %>%
    hc_xAxis(title = list(text = "Month and Year", style = list(color =  "#000000", fontWeight = "bold"))) %>%
    hc_yAxis(title = list(text = "Number of PC Holds", style = list(color =  "#000000", fontWeight = "bold"))) %>%
    hc_title(text = "Number of PC Holds from 2019-2021")




