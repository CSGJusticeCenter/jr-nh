############################################
# Project: JRI New Hampshire
# File: functions_visuals.R
# Last updated: October 24, 2022
# Author: Mari Roberts

# Custom table and plot based functions
############################################

###########
# GT tables
###########

# Format table for for all gt tables (spacing, font size, colors, etc.)
fnc_table_settings <- function(gt_object){
  gt_object %>%
    tab_options(
      #table.width = px(760),
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

# Format table headers with fys as columns
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

# ggplot theme without axes
theme_no_axes <- theme_minimal(base_family = "Franklin Gothic Book") +
  theme(
    plot.title = element_text(
      family = "Franklin Gothic Book",
      face = "bold",
      size = 24, # 18,
      color = "black",
      margin = margin(0, 0, 15, 0)
    ),
    plot.subtitle = element_text(
      family = "Arial",
      size = 22, #15,
      color = "black",
      margin = margin(-10, 0, 15, 0)
    ),
    #axis.text = element_text(size = 22),
    axis.title = element_text(color = "black"),
    axis.title.y = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.text.x = element_text(size = 22, color = "black"),

    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    legend.position = "top",
    legend.justification = c(0, 0),
    legend.text = element_text(family = "Franklin Gothic Book", size = 22, color = "black")
  )

# ggplot theme with axes
theme_axes <- theme_minimal(base_family = "Franklin Gothic Book") +
  theme(
    plot.title = element_text(
      family = "Franklin Gothic Book",
      face = "bold",
      size = 24, # 18,
      color = "black",
      margin = margin(0, 0, 15, 0)
    ),
    plot.subtitle = element_text(
      family = "Franklin Gothic Book",
      size = 22, #15,
      color = "black",
      margin = margin(-10, 0, 15, 0)
    ),
    #axis.text = element_text(size = 22),
    axis.text.x = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title = element_text(color = "black"),
    # axis.title.y = element_blank(),
    # axis.title.x = element_blank(),
    axis.title.y = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 22, color = "black"),

    # Panel.grid.minor = element_blank(),
    # Panel.grid.major = element_blank(),
    # Panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "top",
    legend.justification = c(0, 0),
    legend.title=element_blank(),
    legend.text = element_text(family = "Franklin Gothic Book", size = 22, color = "black")
  )

# ggplot theme without axes or labels
theme_no_axes_labels <- theme_minimal(base_family = "Franklin Gothic Book") +
  theme(
    plot.title = element_text(
      family   = "Franklin Gothic Book",
      face     = "bold",
      size     = 24, # 18,
      color    = "black",
      margin   = margin(0, 0, 15, 0)),

    plot.subtitle = element_text(
      family      = "Arial",
      size        = 22, #15,
      color       = "black",
      margin      = margin(-10, 0, 15, 0)),

    #axis.text   = element_text(size = 22),
    axis.title   = element_text(color = "black"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y  = element_blank(),
    axis.text.x  = element_text(size = 22, color = "black"),

    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border     = element_blank(),

    legend.position  = "top",
    legend.justification = c(0, 0),
    legend.text      = element_text(family = "Franklin Gothic Book", size = 22, color = "black")
  )


# ggplot pie chart theme
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

# Percent grouped bar chart for PC vs non-PC hold
fnc_pct_grouped_bar_chart <- function(df, color1, color2){
  df1 <- group_by(df, fy) %>% mutate(pct = total/sum(total)*100) %>%
    mutate(pct = round(pct, 1))
  df1 <- as.data.frame(df1)
  df1 <- df1 %>% mutate(pct = paste0(pct, "%"))
  ggplot(df1, aes(x = fy, y = total, fill = pc_hold_in_booking)) +
    geom_col(colour = NA, position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values=c(color1,color2), labels = c("Non-PC      ","PC")) +
    geom_text(aes(label = pct, fontface = 'bold'), position = position_fill(vjust = 0.5),
              size = 7.5, family = "Franklin Gothic Book",
              color = ifelse(df1$pc_hold_in_booking == "Non-PC Hold", "black", "white")) +
    theme_axes +
    theme(legend.position = "top",
          legend.justification = c(0, 0),
          legend.title=element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank())
}

# Percent bar chart showing the change in the of HU's and non-HU's over time
fnc_hu_pct_grouped_bar_chart <- function(df, color1, color2, type){
  df1 <- group_by(df, fy) %>% mutate(pct = round(total/sum(total)*100, 1))
  df1 <- as.data.frame(df1)
  df1 <- df1 %>% mutate(pct = round(pct, 1))

  ggplot(df1, aes(x = fy, y = total, fill = type)) +
    geom_col(colour = NA, position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values=c(color1, color2), labels = c("Non-HU      ","HU")) +
    geom_text(data=subset(df1, pct > 3), aes(label = paste0(pct, "%"), fontface = 'bold'), position = position_fill(vjust = 0.5),
              size = 7.5, family = "Franklin Gothic Book",
              color = ifelse(df1$type == "No", "black", "white")) +
    theme_axes +
    theme(legend.position = "top",
          legend.justification = c(0, 0),
          legend.title=element_blank(),
          axis.title.y = element_blank())

}

# Get proportion of high utilizers by variable
fnc_gg_huvsnonhu_pct <- function(df, variable_name, color1, color2){
  df$variable_name <- get(variable_name, df)
  df1 <- group_by(df, fy) %>% mutate(pct = total/sum(total)*100) %>%
    mutate(pct = round(pct, 1))
  df1 <- as.data.frame(df1)
  df1 <- df1 %>% mutate(pct = paste0(pct, "%"))

  ggplot(df1, aes(x = fy, y = total, fill = variable_name)) +
    geom_col(colour = NA, position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values=c(color1, color2), labels = c("Non-HU      ","HU")) +
    geom_text(aes(label = pct, fontface = 'bold'), position = position_fill(vjust = 0.5),
              size = 7.5, family = "Franklin Gothic Book",
              color = ifelse(df1$variable_name == "Non-PC Hold", "black", "white")) +
    theme_axes +
    theme(legend.position = "top",
          legend.justification = c(0, 0),
          legend.title=element_blank(),
          axis.title.y = element_blank())
}

###########
# highcharts
###########

# highcharts theme
hc_theme_jc <- hc_theme(colors = c(jri_light_blue, jri_green, jri_orange),
                        chart = list(style = list(fontFamily = "Franklin Gothic Book", color = "#000000")),
                        title = list(align = "left", style = list(fontFamily = "Franklin Gothic Book", fontSize = "24px")),
                        subtitle = list(align = "left", style = list(fontFamily = "Franklin Gothic Book", fontSize = "16px")),
                        legend = list(align = "center", verticalAlign = "top"),
                        xAxis = list(gridLineColor = "transparent", lineColor = "transparent", minorGridLineColor = "transparent", tickColor = "transparent"),
                        #yAxis = list(labels = list(enabled = FALSE), gridLineColor = "transparent", lineColor = "transparent", minorGridLineColor = "transparent", tickColor = "transparent"),
                        plotOptions = list(line = list(marker = list(enabled = FALSE)),
                                           spline = list(marker = list(enabled = FALSE)),
                                           area = list(marker = list(enabled = FALSE)),
                                           areaspline = list(marker = list(enabled = FALSE)),
                                           arearange = list(marker = list(enabled = FALSE)),
                                           bubble = list(maxSize = "10%")))

# Set up highcharts download buttons
hc_setup <- function(x) {
  highcharter::hc_add_dependency(x, name = "plugins/series-label.js") %>%
    highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
    highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
    highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
    highcharter::hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    highcharter::hc_exporting(enabled = TRUE)
}

# Highchart showing change over time with covid line
fnc_covid_time_highchart <- function(df, yaxis_label, title, line_color){

  df1 <- df %>%
    dplyr::group_by(month_year, month_year_text) %>%
    dplyr::summarise(total = n())
  df1 <- df1 %>%
    mutate(tooltip = paste0("<b>", month_year_text, "</b><br>","Total: ", total, "<br>"),
           month_year_text = as.factor(month_year_text))

  chart <- df1 %>%
    hchart('line', hcaes(x = month_year_text, y = total), color = line_color) %>%
    hc_setup() %>%
    hc_xAxis(
      title = list(text = "Month and Year", style = list(color =  "#000000", fontWeight = "bold", fontSize = "16px")),
      plotLines = list(list(label = list(text = "COVID-19 Start"), fontSize = "26px", color = "gray", width = 2, value = 20, zIndex = 1))
    ) %>%
    hc_yAxis(title = list(text = yaxis_label, style = list(color =  "#000000", fontWeight = "bold", fontSize = "16px"))) %>%
    hc_title(text = title)
  return(chart)

}

###########
# Kable tables
###########

# Kable freq tables
fnc_freq_table <- function(df, title){
  last_row <- nrow(df)
  kable(df, format.args = list(big.mark = ","), align=rep('c'),
        col.names=c(title,"# Bookings","% Bookings", "# Bookings","% Bookings", "# Bookings","% Bookings")) %>%
    kable_styling(bootstrap_options = c("condensed", "responsive"),
                  row_label_position = "l") %>%
    add_header_above(c(" " = 1, "FY 2019" = 2, "FY 2020" = 2, "FY 2021" = 2)) %>%
    row_spec(last_row, bold = TRUE)
}


###########
# Reactable tables
###########

# Show number of entrances by type by fiscal year
fnc_reactable_fy <- function(df, metric_label, label_width, note){

  df1 <- df %>%
    dplyr::rename(new_variable_name = 1)

  fy_table <- reactable(df1,
                        style = list(fontFamily = "Franklin Gothic Book", fontSize = "1.0rem"),
                        pagination = FALSE,
                        theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"),
                                               headerStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
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
                          new_variable_name = colDef(footer = "Total",
                                                     name = metric_label,
                                                     align = "left",
                                                     minWidth = label_width,
                                                     style = list(fontWeight = "bold")),
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
                                                format = colFormat(percent = TRUE, digits = 1)))) %>%
    add_source(paste(note), font_style = "italic", font_size = 14)

  return(fy_table)
}

# Basic reactable table with fys as columns - by county
fnc_reactable_county_fy <- function(df, row_num){

  county_fy_table <-
    reactable(df,
              style = list(fontFamily = "Franklin Gothic Book", fontSize = "1.0rem"),
              pagination = FALSE,
              rowStyle = function(index) {
                if (index %in% c(row_num)) {
                  list(`border-top` = "thin solid",
                       fontWeight = "bold")
                }
              },
              theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"),
                                     headerStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
              defaultColDef = reactable::colDef(
                format = colFormat(separators = TRUE), align = "center"
              ),
              compact = TRUE,
              fullWidth = FALSE,
              columns = list(
                `county`     = colDef(align = "left", minWidth = 180, name = "County", style = list(fontWeight = "bold")),
                `2019`       = colDef(minWidth = 80,  name = "2019"),
                `2020`       = colDef(minWidth = 80,  name = "2020"),
                `2021`       = colDef(minWidth = 80,  name = "2021", style = list(position = "sticky", borderRight = "1px solid #d3d3d3")),
                total        = colDef(minWidth = 80,  name = "Total", style = list(fontWeight = "bold")),
                change_19_21 = colDef(minWidth = 120,  name = "Change from 2019-2021", format = colFormat(percent = TRUE, digits = 1), style = list(fontWeight = "bold"))
                ))

}

# Basic reactable table with fys as columns - by state
fnc_reactable_fy <- function(df, metric_label, label_width, reactable_counties, note){

  df1 <- df %>%
    dplyr::rename(new_variable_name = 1)

  fy_table <- reactable(df1,
                        pagination = FALSE,
                        style = list(fontFamily = "Franklin Gothic Book", fontSize = "1.0rem"),
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
                          new_variable_name = colDef(footer = "Total",
                                                     name = "County",
                                                     align = "left",
                                                     style = list(fontWeight = "bold"),
                                                     minWidth = 150),
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

}

# Summary table showing the min mean max of a variable
fnc_reactable_summary <- function(df, header_name, total1_name, total2_name, freq1_name, mean1_name, max1_name){

  df1 <- df %>%
    dplyr::rename(new_variable_name = 1,
                  total1 = 2,
                  total2 = 3)

  table1 <- reactable(df1,
                      pagination = FALSE,
                      style = list(fontFamily = "Franklin Gothic Book", fontSize = "1.0rem"),
                      rowStyle = function(index) {
                        if (index %in% c(10)) {
                          list(`border-top` = "thin solid",
                               fontWeight = "bold")
                        }
                      },
                      theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"),
                                             headerStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
                      defaultColDef = reactable::colDef(
                        format = colFormat(separators = TRUE), align = "center"),
                      compact = TRUE,
                      fullWidth = FALSE,
                      columns = list(
                        new_variable_name = colDef(minWidth = 190, name = header_name, align = "left",
                                                   style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),
                        total1  = colDef(minWidth = 100, name = total1_name),
                        total2  = colDef(minWidth = 100, name = total2_name),
                        freq    = colDef(minWidth = 130,  name = freq1_name, format = colFormat(percent = TRUE, digits = 1), style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),
                        min     = colDef(minWidth = 100, name = "Minimum", show = F),
                        median  = colDef(minWidth = 100, name = "Median", show = F),
                        mean    = colDef(minWidth = 130, name = mean1_name,
                                        style = list(fontWeight = "bold")),
                        max     = colDef(minWidth = 130, name = max1_name)))
}

# Summary table showing the min mean max of hu's
fnc_reactable_hus_descriptive_summary <- function(df){
  table1 <- reactable(df,
                      pagination = FALSE,
                      style = list(fontFamily = "Franklin Gothic Book", fontSize = "1.0rem"),
                      rowStyle = function(index) {
                        if (index %in% c(10)) {
                          list(`border-top` = "thin solid",
                               fontWeight = "bold")
                        }
                      },
                      theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"),
                                             headerStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
                      defaultColDef = reactable::colDef(
                        format = colFormat(separators = TRUE), align = "center"),
                      compact = TRUE,
                      fullWidth = FALSE,
                      columns = list(
                        county             = colDef(minWidth = 190, name = "County", align = "left",
                                                 style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),
                        total_entrances    = colDef(minWidth = 100, name = "Total Entrances"),
                        total_people       = colDef(minWidth = 90,  name = "Total People"),

                        total_hu_entrances = colDef(minWidth = 100, name = "HU's (Entrances)"),
                        mean_all           = colDef(minWidth = 130, name = "Avg Entrances Per Person", style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3")),
                        total_hu_people    = colDef(minWidth = 100, name = "HU's (People)" ),
                        min                = colDef(minWidth = 130, name = "Min Entrances Per Person"),
                        max                = colDef(minWidth = 130, name = "Max Entrances Per Person"),
                        median             = colDef(minWidth = 130, name = "Med Entrances Per Person"),
                        mean               = colDef(minWidth = 130, name = "Avg Entrances Per Person", format = colFormat(digits = 1), style = list(fontWeight = "bold")),
                        range              = colDef(minWidth = 130, name = "Range of Entrances Per Person"),
                        freq               = colDef(minWidth = 130, name = "Proportion of Entrances that are HU's", format = colFormat(percent = TRUE, digits = 1), style = list(fontWeight = "bold", position = "sticky", borderRight = "1px solid #d3d3d3"))
                      ))
}

# Reactable table showing the types of booking types recordings by county
# Shows how each county records things differently
fnc_reactable_booking_recordings <- function(df){
  reactable(df,
            style = list(fontFamily = "Franklin Gothic Book", fontSize = "1.0rem"),
            pagination = FALSE,
            theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"),
                                   headerStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
            defaultColDef = reactable::colDef(
              format = colFormat(separators = TRUE), align = "center"
            ),
            compact = TRUE,
            fullWidth = FALSE,
            columns = list(
              booking_type          = colDef(align = "left", minWidth = 300, name = "Booking Type (Original)"),
              booking_type_standard = colDef(align = "left", minWidth = 300, name = "Booking Type (Standardized)", style = list(fontWeight = "bold")),
              sentence_status       = colDef(align = "left", minWidth = 300, name = "Sentence Status"),
              release_type          = colDef(align = "left", minWidth = 300, name = "Release Type"),
              total                 = colDef(align = "left", minWidth = 300, name = "Number of Occurrences")
            ))
}

fnc_reactable_charges_booking_recordings <- function(df){
  reactable(df,
            style = list(fontFamily = "Franklin Gothic Book", fontSize = "1.0rem"),
            pagination = FALSE,
            theme = reactableTheme(cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"),
                                   headerStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")),
            defaultColDef = reactable::colDef(
              format = colFormat(separators = TRUE), align = "center"
            ),
            compact = TRUE,
            fullWidth = FALSE,
            columns = list(
              charge_desc           = colDef(align = "left", minWidth = 300, name = "Charge Descriptions (Original)"),
              booking_type          = colDef(align = "left", minWidth = 300, name = "Booking Type (Original)"),
              booking_type_standard = colDef(align = "left", minWidth = 300, name = "Booking Type (Standardized)", style = list(fontWeight = "bold")),
              sentence_status       = colDef(align = "left", minWidth = 300, name = "Sentence Status"),
              release_type          = colDef(align = "left", minWidth = 300, name = "Release Type"),
              total                 = colDef(align = "left", minWidth = 300, name = "Number of Occurrences")
            ))
}

