############################################
# Project: JRI New Hampshire
# File: misc.R
# Last updated: September 12, 2022
# Author: Mari Roberts

# Format tables and graphics for misc website pages
############################################

############
# What variables are present in each administrative file?
# create a table with check marks and x's
############

raw_adm_data_availability <- raw_adm_data_availability.xlsx %>%
  select(-c(`Full Name`,
            `Inmate ID`,
            Address)) %>%
  select(County,
         YOB = DOB,
         everything()) %>%
  select(-YOB, -Gender, -Race)

my_color_bar <- function (color = "lightgray", fixedWidth=150,...)
{
  formatter("span", style = function(x) style(width = ))
}

yes_no <- formattable::formatter(.tag = "span", style = function(x) style(color = ifelse(x == "No" , "red", "green")), x ~ icontext(ifelse(x == "No", "glyphicon glyphicon-remove", "glyphicon glyphicon-ok"), x))
yes_no <- formatter("span", style = function(x) style(display = "inline-block",
                                                      direction = "rtl", `border-radius` = "4px", `padding-right` = "2px",
                                                      `background-color` = "white", width = "80px",
                                                      color = ifelse(x == "No" , "red", "green")), x ~ icontext(ifelse(x == "No", "glyphicon glyphicon-remove", "glyphicon glyphicon-ok"), x))

raw_adm_data_availability_table <- formattable(raw_adm_data_availability,
            align = c("l","l","l","l","l","l","l","l","l","l"),
            list(County = #formatter("span", style = x ~ style("font-weight" = "bold"), width = "200px"),
                   formatter(.tag = "span", style = function(x) style("font-weight" = "bold", "font-family" = "Franklin Gothic Book",display = "inline-block", width = "120px")),
                 YOB = yes_no,
                 Race = yes_no,
                 Gender = yes_no,
                `Booking Type` = yes_no,
                `Booking Date` = yes_no,
                `Charge Description` = yes_no,
                `Charge Code` = yes_no,
                `Release Type` = yes_no,
                `Release Date` = yes_no,
                `Sentence Status` = yes_no))

############
# save to SP
############

save(raw_adm_data_availability_table, file=paste0(sp_data_path, "/Data/r_data/raw_adm_data_availability_table.Rda", sep = ""))
