############################################
# Project: JRI New Hampshire
# File: info_by_state_plots.R
# Last updated: August 22, 2022
# Author: Mari Roberts

# Generate tables for state overview
############################################


adm_table <- gt(df) %>%

  # spanner for 2021 Survey
  tab_spanner(label = "Survey 2021", columns = c(previous_2018, previous_2019, previous_2020)) %>%
  tab_style(style = cell_text(size = px(12)),
            locations = cells_column_labels(columns = c(previous_2018, previous_2019, previous_2020))) %>%

  # spanner for 2022 survey
  tab_spanner(label = "Survey 2022", columns = c(current_2018, current_2019, current_2020, current_2021)) %>%
  tab_style(style = cell_text(size = px(12)),
            locations = cells_column_labels(columns = c(current_2018, current_2019, current_2020, current_2021))) %>%

  # table title and subtitle
  tab_header(title = "Prison Admissions", subtitle = "2021 Data and Changes in Data from 2018 to 2020") %>%
  tab_style(style = cell_text(color = "black", weight = "bold", align = "left"),
            locations = cells_title("title")) %>%
  tab_style(style = cell_text(color = "#696969", weight = "normal", align = "left"),
            locations = cells_title("subtitle")) %>%

  # border lines around 2021 survey
  tab_style(style = list(cell_borders(side = c("left"), color = "gray", weight = px(1))),
            locations = cells_body(columns = c(previous_2018))) %>%
  tab_style(style = list(cell_borders(side = c("left"), color = "gray", weight = px(1))),
            locations = cells_body(columns = c(current_2018))) %>%

  # hide data check columns
  cols_hide(columns = c(check_2018_21_22, check_2019_21_22, check_2020_21_22)) %>%

  # appearance settings
  fnc_table_settings() %>%

  # specifications for column widths and labels
  fnc_headers() %>%

  # change color to green if there were updates to the data from 2018 - 2020, and new data for 2021
  tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
            locations = cells_body(columns = c(current_2018), rows = previous_2018 != current_2018 & (rows = current_2018 != "Left Blank"))) %>%
  tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
            locations = cells_body(columns = c(current_2019), rows = previous_2019 != current_2019 & (rows = current_2019 != "Left Blank"))) %>%
  tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
            locations = cells_body(columns = c(current_2020), rows = previous_2020 != current_2020 & (rows = current_2020 != "Left Blank"))) %>%
  tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
            locations = cells_body(columns = current_2021, rows = current_2021 != "Left Blank")) %>%

  # change color to yellow if a field was left blank
  tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
            locations = cells_body(columns = current_2018, rows = current_2018 == "Left Blank")) %>%
  tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
            locations = cells_body(columns = current_2019, rows = current_2019 == "Left Blank")) %>%
  tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
            locations = cells_body(columns = current_2020, rows = current_2020 == "Left Blank")) %>%
  tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
            locations = cells_body(columns = current_2021, rows = current_2021 == "Left Blank")) %>%
