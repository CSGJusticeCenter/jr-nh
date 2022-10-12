############################################
# Project: JRI New Hampshire
# File: demographics.R
# Last updated: September 30, 2022
# Author: Mari Roberts

# Tables, graphs, and numbers for demographics page
############################################

#################
# Age
#################

# age category table
df_nh_age_category <- fnc_variable_table(nh_booking_19, nh_booking_20, nh_booking_21, "age_category")
df_nh_age_category <- df_nh_age_category %>%
  arrange(variable_name) %>%
  filter(variable_name != "Total") %>%
  select(new_variable_name = variable_name, everything()) %>%
  droplevels()

fnc_reactable_fy <- function(df, metric_label, label_width, reactable_counties, note){

  df1 <- df_nh_age_category %>%
    dplyr::rename(new_variable_name = 1)

  # create reactable table of number/freq of booking types by fiscal year and for all 3 years
  fy_table <- reactable(df1,
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
                          new_variable_name = colDef(footer = "Total",
                                                     name = "100",
                                                     align = "left",
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

temp <- fnc_reactable_fy(df_nh_age_category,
                         metric_label = "test",
                         reactable_counties = counties,
                         label_width = 150)


# create reactable table for age categories by fiscal year
nh_age_category <- fnc_reactable_fy(df_nh_age_category,
                                    metric_label = "test",
                                    reactable_counties = counties,
                                    label_width = 150,
                                    note = "test")

reactable(df_nh_age_category,
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
                        variable_name = colDef(footer = "Total",
                                                   name = "Test",
                                                   align = "left",
                                                   minWidth = 50),
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
  add_source(paste("Counties included: ", ". "), font_style = "italic", font_size = 14)

# age histogram
