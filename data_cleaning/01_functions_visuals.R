############################################
# Project: JRI New Hampshire
# File: functions_visuals.R
# Last updated: August 23, 2022
# Author: Mari Roberts

# Custom table and plot based functions
############################################

###########
# GT tables
###########

# custom function to format table for for all gt tables (spacing, font size, colors, etc.)
fnc_table_settings <- function(gt_object){
  gt_object %>%
    tab_options(#table.width = px(760),
      table.align = "left",
      heading.align = "left",

      # remove row at top
      table.border.top.style = "hidden",
      # table.border.bottom.style = "transparent",
      heading.border.bottom.style = "hidden",
      table.border.bottom.color = "gray",

      # need to set this to transparent so that cells_borders of the cells can display properly
      table_body.border.bottom.style = "transparent",
      table_body.border.top.style = "transparent",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "gray",

      # font sizes
      heading.title.font.size = px(14),
      heading.subtitle.font.size = px(12),
      column_labels.font.size = px(12),
      table.font.size = px(12),
      source_notes.font.size = px(12),
      footnotes.font.size = px(12),

      # row group label and border options
      row_group.font.size = px(12),
      row_group.border.top.style = "transparent",
      row_group.border.bottom.style = "hidden",
      stub.border.style = "dashed"
    )
}

# custom function to format table headers for pc holds
fnc_pc_holds_headers <- function(gt_object){
  gt_object %>%
  cols_width(
    "pc_hold"  ~ px(100),
    "count_19" ~ px(70),
    "pct_19"   ~ px(70),
    "count_20" ~ px(70),
    "pct_20"   ~ px(70),
    "count_21" ~ px(70),
    "pct_21"   ~ px(70),
    "total"    ~ px(70),
    "freq"     ~ px(70)) %>%
    cols_label(
      pc_hold  = " ",
      count_19 = "Count",
      pct_19   = "Freq",
      count_20 = "Count",
      pct_20   = "Freq",
      count_21 = "Count",
      pct_21	 = "Freq",
      total    = "3 Yr Total",
      freq     = "Freq")
}

###########
# ggplots
###########

# booking heat map
fnc_booking_heatmap <- function(df){
  ggplot(df, aes(year, month)) +
    geom_tile(aes(fill = N), colour = "white") +
    #scale_fill_gradient(low = "#d4e9f8", high = "#00475d") +
    scale_fill_gradient(low = "#eeed90", high = "#315c15") +
    guides(fill=guide_legend(title="Total Bookings")) +
    labs(title = "Number of Bookings by Month and FY",
         x = "Year", y = "Month") +
    theme_bw() + theme_minimal()
}

fnc_covid_time_highchart <- function(df, yaxis_label, title){

  counties <- df %>%
    mutate(county = as.character(county))
  counties <- unique(counties$county)

  df1 <- df %>%
    dplyr::group_by(month_year, month_year_text) %>%
    dplyr::summarise(total = n())
  df1 <- df1 %>%
    mutate(tooltip = paste0("<b>", month_year_text, "</b><br>","Total: ", total, "<br>"),
           month_year_text = as.factor(month_year_text))

  chart <- df1 %>%
    hchart('line', hcaes(x = month_year_text, y = total), color = jri_dark_blue) %>%
    hc_setup() %>%
    hc_xAxis(
      title = list(text = "Month and Year", style = list(color =  "#000000", fontWeight = "bold")),
      plotLines = list(list(label = list(text = "Start of COVID-19 Pandemic"), color = jri_red, width = 2, value = 20, zIndex = 1))
    ) %>%
    hc_yAxis(title = list(text = yaxis_label, style = list(color =  "#000000", fontWeight = "bold"))) %>%
    hc_title(text = title) %>%
    hc_caption(
      text = (paste(" ", counties))
    )

  return(chart)

}

###########
# Highcharter
###########

# custom highcharts theme for plots
hc_theme_jc <- hc_theme(colors = c("#D25E2D", "#EDB799", "#C7E8F5", "#236ca7", "#D6C246", "#dcdcdc"),
                        chart = list(style = list(#fontFamily = "Franklin Gothic Book",
                                                  color = "#666666")),
                        title = list(align = "left", style = list(#fontFamily = "Franklin Gothic Book",
                                                                  fontSize = "24px")),
                        subtitle = list(align = "left", style = list(#fontFamily = "Franklin Gothic Book",
                                                                     fontSize = "16px")),
                        legend = list(align = "left", verticalAlign = "top"),
                        xAxis = list(gridLineColor = "transparent", lineColor = "transparent", minorGridLineColor = "transparent", tickColor = "transparent"),
                        yAxis = list(labels = list(enabled = FALSE), gridLineColor = "transparent", lineColor = "transparent", minorGridLineColor = "transparent", tickColor = "transparent"),
                        plotOptions = list(line = list(marker = list(enabled = FALSE)),
                                           spline = list(marker = list(enabled = FALSE)),
                                           area = list(marker = list(enabled = FALSE)),
                                           areaspline = list(marker = list(enabled = FALSE)),
                                           arearange = list(marker = list(enabled = FALSE)),
                                           bubble = list(maxSize = "10%")))

# # set up highcharts download buttons
# hc_setup <- function(x) {
#   hc_add_dependency(x, name = "modules/exporting.js") %>%
#     hc_add_dependency(name = "modules/offline-exporting.js") %>%
#     hc_exporting(
#       enabled = FALSE, # change to TRUE to add drop down download options
#       buttons = list(contextButton = list(menuItems = list("printChart", "downloadPNG", "downloadSVG", "downloadPDF")))) %>%
#     hc_add_theme(hc_theme_jc) %>%
#     hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
#     hc_plotOptions(series = list(animation = FALSE))
# }

# set up highcharts download buttons
hc_setup <- function(x) {
  highcharter::hc_add_dependency(x, name = "plugins/series-label.js") %>%
    highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
    highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
    highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
    highcharter::hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    highcharter::hc_exporting(enabled = TRUE)
}

###########
# Kable tables
###########

# kable freq tables
fnc_freq_table <- function(df, title){
  last_row <- nrow(df)
  kable(df, format.args = list(big.mark = ","), align=rep('c'),
        col.names=c(title,"# Bookings","% Bookings", "# Bookings","% Bookings", "# Bookings","% Bookings")) %>%
    kable_styling(bootstrap_options = c("condensed", "responsive"),
                  row_label_position = "l") %>%
    add_header_above(c(" " = 1, "FY 2019" = 2, "FY 2020" = 2, "FY 2021" = 2)) %>%
    row_spec(last_row, bold = TRUE)
}
