############################################
# Project: JRI New Hampshire
# File: functions.R
# Last updated: February 28, 2023
# Author: Mari Roberts

# Custom data cleaning, visualization, and structure functions
############################################

# load packages
source("code/00_library.R")

####################################

###########
# Charge cleaning functions
###########

fnc_clean_charge_data <- function(df){

  # use three separate joins as jail charge data will join on either charge code, statute title, or descriptor
  # join to charge_codes_lookup via charge_code_lookup
  df_join_one <- df %>%
    left_join(charge_codes_crime_type_lookup,
              by = c("charge_code_clean"="charge_code_lookup")) %>%
    distinct(id,
             inmate_id,
             booking_id,
             charge_desc,
             .keep_all = TRUE) %>%
    # populate for eventual rbind where all three df's need same columns
    mutate(charge_code_lookup = ifelse(!is.na(descriptor_lookup) == TRUE,
                                       charge_code_clean, NA))

  # join to charge_codes_lookup via charge_desc_clean and statute_title_lookup
  df_join_two <- df_join_one %>%
    filter(is.na(descriptor_lookup) == TRUE) %>%
    # remove these columns to avoid duplicates (e.g. .x and .y) for rbind
    dplyr::select(-c(charge_code_lookup,
                     descriptor_lookup,
                     statute_title_lookup,
                     crime_type_lookup)) %>%
    left_join(charge_codes_crime_type_lookup,
              by = c("charge_desc_clean" = "statute_title_lookup")) %>%
    distinct(id,
             inmate_id,
             booking_id,
             charge_desc_clean,
             .keep_all = TRUE) %>%

    mutate(statute_title_lookup = ifelse(!is.na(descriptor_lookup) == TRUE,
                                         charge_desc_clean, NA))

  # join to charge_codes_lookup via charge_desc_clean and descriptor_lookup
  df_join_three <- df_join_two %>%
    filter(is.na(descriptor_lookup) == TRUE) %>%
    # remove these columns to avoid duplicates (e.g. .x and .y) for rbind
    dplyr::select(-c(charge_code_lookup,
                     descriptor_lookup,
                     statute_title_lookup,
                     crime_type_lookup)) %>%
    left_join(charge_codes_crime_type_lookup,
              by = c("charge_desc_clean" = "descriptor_lookup")) %>%
    distinct(id,
             inmate_id,
             booking_id,
             charge_desc_clean,
             .keep_all = TRUE) %>%
    # populate for eventual rbind where all three df's need same columns
    mutate(descriptor_lookup = ifelse(!is.na(statute_title_lookup) == TRUE,
                                      charge_desc_clean, NA))

  # combine df's from first, second, and third attempts to join to lookup values
  df_final <- rbind(df_join_one,
                    df_join_two,
                    df_join_three) %>%
    # create booking-level flag that indicates at least one charge was a drug/alcohol charge
    # doing this before de-duping allows us to know this regardless of whether it was the
    # most serious offense for a given booking
    mutate(drug_alcohol_charge_flag = ifelse(crime_type_lookup=="Drug/Alcohol",
                                             1, 0),
           drug_alcohol_charge_flag = ifelse(is.na(drug_alcohol_charge_flag),
                                             0, drug_alcohol_charge_flag)) %>%
    group_by(id,
             inmate_id,
             booking_id) %>% # group by booking
    mutate(booking_drug_alcohol_charge_present_flag = max(drug_alcohol_charge_flag,
                                                          na.rm = TRUE)) %>%
    ungroup() %>%
    dplyr::select(-drug_alcohol_charge_flag) %>% # drop charge level flag, keep booking level flag
    # de-dup once more for records that ended up duplicating across the joins
    # use arrange to keep records with joining lookup values over identical records with missing lookup values
    arrange(id,
            inmate_id,
            booking_id,
            statute_title_lookup) %>%
    distinct(id,
             inmate_id,
             booking_id,
             charge_desc_clean,
             .keep_all = TRUE) %>%
    # as a check, at this point our total record should be identical to the cleaned jail file we started with -- it is
    # now we want to de-dup by booking (one booking may have multiple charges associated)
    # keep most serious charge by crime type (hierarchy: violent, property, drug/alcohol, public order, vop)
    mutate(crime_type_severity_order = case_when(
      crime_type_lookup=="Violent"                    ~ 1,
      crime_type_lookup=="Property"                   ~ 2,
      crime_type_lookup=="Drug/Alcohol"               ~ 3,
      crime_type_lookup=="Public Order"               ~ 4,
      crime_type_lookup=="Probation/Parole Violation" ~ 5,
      crime_type_lookup=="FTA/Bail"                   ~ 6,
      crime_type_lookup=="Temporary Hold"             ~ 7,
      is.na(crime_type_lookup) == TRUE                ~ 8,
      TRUE ~ as.numeric(NA))) %>%
    arrange(id,
            inmate_id,
            booking_id,
            crime_type_severity_order) %>%
    # de-dup by booking, keeping the record with the most serious crime type
    distinct(id,
             inmate_id,
             booking_id,
             .keep_all = TRUE) %>%
    # create missing_lookup_charge_data column to double check missingness with non-pc holds
    mutate(missing_lookup_charge_data = ifelse(is.na(statute_title_lookup) == TRUE,
                                               1, 0))
}

###########
# Create fy, age, los, recode race, and order variables
###########

# Create fy, age, los, and create age categories
# Organize variables
# Remove outliers for age
fnc_data_setup <- function(df){
  df1 <- df %>%
    mutate(fy = case_when(booking_date >= "2018-07-01" & booking_date <= "2019-06-30" ~ 2019,
                          booking_date >= "2019-07-01" & booking_date <= "2020-06-30" ~ 2020,
                          booking_date >= "2020-07-01" & booking_date <= "2021-06-30" ~ 2021),
           age = fy - yob,
           los = difftime(as.POSIXct(release_date), as.POSIXct(booking_date, tz="UTC"), units="days")) %>%
    filter(fy == 2019 | fy == 2020 | fy == 2021) %>%
    dplyr::select(id,
                  inmate_id,
                  yob,
                  race_code,
                  race = race_label,
                  gender = sex,
                  age,
                  housing,
                  charge_code,
                  charge_desc,
                  booking_date,
                  booking_type,
                  release_date,
                  release_type,
                  sentence_status,
                  los,
                  county,
                  fy) %>%
    mutate(race = ifelse(race == "Unknown", NA, race),
           age = as.numeric(age)) %>%
    filter(age >= 18) %>%
    mutate(age = ifelse(age > 100, NA, age))

  #  18–29, 30–39, and 40
  df1 <- df1 %>% mutate(age_category
                        = case_when(age <= 24 ~ "18-24 yo",
                                    age >= 25 & age <= 34 ~ "25-34 yo",
                                    age >= 35 & age <= 49 ~ "35-49 yo",
                                    age >= 50 ~ "50+ yo"))
}

# Create booking id
# Based on inmate id and booking date
fnc_booking_id <- function(df, county){
  # If id was created use id, otherwise use inmate id
  df1 <- df %>%
    mutate(id = ifelse(is.na(id), inmate_id, id))
  # Create booking date based on id and booking date
  df1$booking_id <- df1 %>% dplyr::group_indices(id, booking_date)
  # Paste county name to booking id so there aren't duplicates when we add county files together
  df1 <- df1 %>%
    mutate(id = paste(county, id, sep = "_"),
           booking_id = paste(county, "booking", booking_id, sep = "_")) %>%
    select(id, inmate_id, booking_id, everything())
}

# Get maximum los by booking id
fnc_los <- function(df){
  df_new <- df %>%
    ungroup() %>%
    dplyr::group_by(booking_id) %>%
    dplyr::summarise(los_max = max(los, na.rm=TRUE))
  df_new <- merge(df, df_new, by = "booking_id")
}

###########
# Add high utilizer variables
###########

# Create flag for high utilizer for people in the top 1%, 5%, 10% percentile of entrances.
fnc_create_high_utilizer_variables <- function(df){

  #########
  # 3 yr HUs
  #########

  # Count number of entrances by id
  df_hus_3yrs <- df %>%
    dplyr::select(id, booking_id, booking_date) %>%
    dplyr::distinct() %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(num_entrances = n())

  df_hus_3yrs <- df_hus_3yrs %>%
    dplyr::select(id, num_entrances) %>% distinct() %>%
    mutate(high_utilizer_4_times = num_entrances >= 4) %>%                                                # 4 or more times

    mutate(high_utilizer_1_pct   = quantile(df_hus_3yrs$num_entrances, probs = 0.99) < num_entrances) %>% # Top 1%

    mutate(high_utilizer_5_pct   = quantile(df_hus_3yrs$num_entrances, probs = 0.95) < num_entrances) %>% # Top 5%

    mutate(high_utilizer_10_pct  = quantile(df_hus_3yrs$num_entrances, probs = 0.90) < num_entrances) %>% # Top 10%

    mutate(high_utilizer_4_times = case_when(high_utilizer_4_times == TRUE  ~ "Yes",
                                             high_utilizer_4_times == FALSE ~ "No"),

           high_utilizer_1_pct  = case_when(high_utilizer_1_pct == TRUE     ~ "Yes",
                                            high_utilizer_1_pct == FALSE    ~ "No"),

           high_utilizer_5_pct  = case_when(high_utilizer_5_pct == TRUE     ~ "Yes",
                                            high_utilizer_5_pct == FALSE    ~ "No"),

           high_utilizer_10_pct = case_when(high_utilizer_10_pct == TRUE    ~ "Yes",
                                            high_utilizer_10_pct == FALSE   ~ "No"))

  return(df_hus_3yrs)
}

# Create exclusive HU variable - Andrew's change
fnc_hu_group_exclusive <- function(df){
  df <- df %>%
    mutate(hu_group_exclusive = case_when(high_utilizer_10_pct =="No"     ~ 4,
                                          high_utilizer_10_pct =="Yes" &
                                            high_utilizer_5_pct=="No" &
                                            high_utilizer_1_pct=="No"     ~ 3,
                                          high_utilizer_5_pct  =="Yes" &
                                            high_utilizer_1_pct=="No"     ~ 2,
                                          high_utilizer_1_pct  =="Yes"    ~ 1,
                                          TRUE ~ as.numeric(NA)),
           hu_group_exclusive = factor(hu_group_exclusive,
                                       levels = c(1,2,3,4),
                                       labels = c("Tier 1 HU", "Tier 2 HU", "Tier 3 HU", "Non-HU"))) %>%
    # Overall HU variable
    mutate(hu_group_overall = case_when(high_utilizer_10_pct == "No"  ~ 2,
                                        high_utilizer_10_pct == "Yes" ~ 1,
                                        TRUE ~ as.numeric(NA)),
           hu_group_overall = factor(hu_group_overall,
                                     levels = c(1,2),
                                     labels = c("High Utilizer", "Non-High Utilizer")))
}

###########
# Add sex labels
###########

# Add sex labels depending on sex code
fnc_sex_labels <- function(df){
  df1 <- df %>%
    mutate(gender = case_when(
      gender == "M"      ~ "Male",
      gender == "Male"   ~ "Male",

      gender == "F"      ~ "Female",
      gender == "Female" ~ "Female",

      gender == "T"      ~ "Transgender",
      gender == "TF"     ~ "Transgender",
      gender == "TRANF"  ~ "Transgender",

      gender == "Not Specified" ~ "Unknown",
      gender == "U"             ~ "Unknown",
      is.na(gender)             ~ "Unknown",
      TRUE ~ gender)) %>%
    distinct()
  df1 <- df1 %>% mutate(gender = ifelse(gender == "Unknown", NA, gender))
}

###########
# Add data labels to county data
###########

# Add labels to data for data dictionaries
fnc_add_data_labels <- function(df){

  df1 <- df %>%
    select(id,
           inmate_id,
           booking_id,
           yob,
           race_code,
           race,
           gender,
           age,
           age_category,
           housing,
           charge_code,
           charge_desc,
           booking_date,
           booking_type,
           release_date,
           release_type,
           sentence_status,
           sentence_status_standard,
           #los,
           los_max,
           county,
           fy,
           num_entrances,
           high_utilizer_4_times,
           high_utilizer_1_pct,
           high_utilizer_5_pct,
           high_utilizer_10_pct,
           pc_hold)

  # Change data types
  df1$id                       <- as.character(df1$id)
  df1$inmate_id                <- as.character(df1$inmate_id)
  df1$booking_id               <- as.character(df1$booking_id)
  df1$yob                      <- as.numeric(df1$yob)
  df1$race_code                <- as.factor(df1$race_code)
  df1$race                     <- as.character(df1$race)
  df1$gender                   <- as.factor(df1$gender)
  df1$age                      <- as.numeric(df1$age)
  df1$gender                   <- as.factor(df1$gender)
  df1$age_category             <- as.character(df1$age_category)
  df1$housing                  <- as.character(df1$housing)
  df1$charge_code              <- as.character(df1$charge_code)
  df1$charge_desc              <- as.character(df1$charge_desc)
  df1$booking_type             <- as.character(df1$booking_type)
  df1$release_type             <- as.character(df1$release_type)
  df1$sentence_status          <- as.character(df1$sentence_status)
  df1$sentence_status_standard <- as.character(df1$sentence_status_standard)
  df1$los_max                  <- as.numeric(df1$los_max)
  df1$county                   <- as.character(df1$county)
  df1$fy                       <- as.numeric(df1$fy)
  df1$num_entrances            <- as.numeric(df1$num_entrances)

  df1$high_utilizer_4_times    <- as.character(df1$high_utilizer_4_times)
  df1$high_utilizer_1_pct      <- as.character(df1$high_utilizer_1_pct)
  df1$high_utilizer_5_pct      <- as.character(df1$high_utilizer_5_pct)
  df1$high_utilizer_10_pct     <- as.character(df1$high_utilizer_10_pct)

  df1$pc_hold             <- as.factor(df1$pc_hold)

  # Data labels
  var.labels <- c(id                       = "Unique ID",
                  inmate_id                = "Inmate ID",
                  booking_id               = "Booking id created by id and booking date",
                  yob                      = "Year of birth",
                  race_code                = "Original race code",
                  race                     = "Race",
                  gender                   = "Gender",
                  age                      = "Age (years)",
                  age_category             = "Age category",
                  housing                  = "Whether individual is housed or unhoused",
                  charge_code              = "Charge code",
                  charge_desc              = "Charge description",
                  booking_date             = "Booking date",
                  booking_type             = "Booking type",
                  release_date             = "Release date",
                  release_type             = "Release type",
                  sentence_status          = "Sentence status",
                  sentence_status_standard = "Sentence status standardized",
                  los_max                  = "Maximum length of stay (days) to account for release date errors",
                  county                   = "County",
                  fy                       = "Fiscal year",
                  num_entrances            = "Number of booking events for all years",
                  high_utilizer_4_times    = "Is a high utilizer (entered jail 4 or more times for all 3 yrs)",
                  high_utilizer_1_pct      = "Is a high utilizer (in top 1% percentile of jail entrances for all 3 yrs)",
                  high_utilizer_5_pct      = "Is a high utilizer (in top 5% percentile of jail entrances for all 3 yrs)",
                  high_utilizer_10_pct     = "Is a high utilizer (in top 10% percentile of jail entrances for all 3 yrs)",
                  pc_hold                  = "Protective custody hold (in booking type, charge type, sentence status, or release type)")
  # Add labels to data
  df1 <- labelled::set_variable_labels(df1, .labels = var.labels)

}

# Extract levels of booking type, sentence status, and release type to understand booking information by county
fnc_investigate_booking_recordings <- function(df){
  df1 <- df %>%
    mutate(charge_desc     = as.character(charge_desc),
           booking_type    = as.character(booking_type),
           release_type    = as.character(release_type),
           sentence_status = as.character(sentence_status),
           charge_desc     = toupper(charge_desc),
           booking_type    = toupper(booking_type),
           release_type    = toupper(release_type),
           sentence_status = toupper(sentence_status)) %>%
    select(id, booking_id, charge_desc, booking_type, sentence_status, release_type) %>%
    distinct() %>%
    group_by(booking_type, sentence_status) %>%
    summarise(total = n())
}

# Get prop of variable
fnc_variable_by_year <- function(df, variable_name){
  df$variable_name <- get(variable_name, df)
  df <- df %>% select(variable_name, booking_id) %>% distinct()
  df1 <- data.frame(summarytools::freq(df$variable_name, order = "freq", cum.percent = FALSE))
  df1 <- df1 %>% tibble::rownames_to_column("variable_name") %>%
    dplyr::select(variable_name,
                  count = Freq,
                  pct   = X..Valid)
}

# Replace NAs with blanks or no data
fnc_replace_nas <- function(df){
  df <- df %>%
    mutate_if(grepl('<NA>',.), ~replace(., grepl('<NA>', .), "NA")) %>%
    mutate_if(grepl('NA%',.),  ~replace(., grepl('NA%', .), "-"))
}

# Get row and column totals for tables
fnc_row_totals <- function(df){
  withnas <- df %>%
    adorn_totals("row") %>%
    mutate(total = count_19 + count_20 + count_21)
  nonas <- df %>%
    filter(dplyr::across(everything(), ~ !grepl("NA", .))) %>%
    filter(dplyr::across(everything(), ~ !grepl("Total", .))) %>%
    mutate(total = count_19 + count_20 + count_21) %>%
    mutate(freq = (total/sum(total, na.rm = TRUE))) %>%
    adorn_totals("row") %>%
    select(c(1, "freq"))
  df_freq <- merge(withnas, nonas, by.x = 1, by.y = 1, all.x = TRUE)
  return(df_freq)
}

###################################################################################################################

# Visualization Functions

###################################################################################################################

# ggplot theme no grid or labels for axis
theme_no_grid_no_labels <- theme_minimal(base_family = "Franklin Gothic Book") +
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

    axis.text    = element_text(size = 22),
    axis.title   = element_text(size = 22, color = "black"),
    # axis.title.y = element_text(size = 22, color = "black"),
    # axis.title.x = element_text(size = 22, color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y  = element_text(size = 22, color = "black"),
    axis.text.x  = element_text(size = 22, color = "black"),

    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),

    legend.position = "right",
    legend.title=element_blank(),
    legend.justification = c(0, 0.5),
    legend.text = element_text(family = "Franklin Gothic Book", size = 22, color = "black")
  )

# ggplot theme no grid but with axis labels
theme_no_grid_with_labels <- theme_minimal(base_family = "Franklin Gothic Book") +
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

    axis.text    = element_text(size = 22),
    axis.title   = element_text(size = 22, color = "black"),
    axis.title.y = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 22, color = "black"),
    axis.text.y  = element_text(size = 22, color = "black"),
    axis.text.x  = element_text(size = 22, color = "black"),

    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),

    legend.position = "right",
    legend.title=element_blank(),
    legend.justification = c(0, 0.5),
    legend.text = element_text(family = "Franklin Gothic Book", size = 22, color = "black")
  )

# ggplot theme with grid and labels for axis
theme_with_grid_with_labels <- theme_minimal(base_family = "Franklin Gothic Book") +
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

    axis.text    = element_text(size = 22),
    axis.title   = element_text(size = 22, color = "black"),
    axis.title.y = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 22, color = "black"),
    axis.text.y  = element_text(size = 22, color = "black"),
    axis.text.x  = element_text(size = 22, color = "black"),

    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),

    legend.position = "right",
    legend.justification = c(0, 0.5),
    legend.title=element_blank(),
    legend.text = element_text(family = "Franklin Gothic Book", size = 22, color = "black")
  )


# Set up highcharts download buttons
hc_setup <- function(x) {
  highcharter::hc_add_dependency(x, name = "plugins/series-label.js") %>%
    highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
    highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
    highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
    highcharter::hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    highcharter::hc_exporting(enabled = TRUE)
}

# Show number of X by fiscal year
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

# Get count and prop of X by FY
fnc_variable_table <- function(df_19, df_20, df_21, variable_name){

  df_19_new <- fnc_variable_by_year(df_19, variable_name)
  df_20_new <- fnc_variable_by_year(df_20, variable_name)
  df_21_new <- fnc_variable_by_year(df_21, variable_name)

  # Rename variables for merging, indicate which year
  df_19_new <- df_19_new %>% dplyr::rename(count_19 = count,
                                           pct_19   = pct)
  df_20_new <- df_20_new %>% dplyr::rename(count_20 = count,
                                           pct_20   = pct)
  df_21_new <- df_21_new %>% dplyr::rename(count_21 = count,
                                           pct_21   = pct)

  # Join data
  df <- merge(df_19_new, df_20_new, by = "variable_name", all.x = TRUE, all.y = TRUE)
  df <- merge(df, df_21_new, by = "variable_name", all.x = TRUE, all.y = TRUE)

  # Create row totals and frequencies
  df[df == "NA%"] = NA
  df[is.na(df)] = 0
  df <- df %>%
    filter(variable_name != "Total")
  df <- fnc_replace_nas(df)

  # Get totals and frequencies without including NAs
  df <- fnc_row_totals(df)

  # Divide pcts by 100
  df <- df %>% mutate(pct_19 = pct_19/100,
                      pct_20 = pct_20/100,
                      pct_21 = pct_21/100)

  # Arrange table data
  df <- fnc_variable_table_desc(df)
  df[is.na(df)] = 0

  return(df)
}

# Arrange data in descending order
fnc_variable_table_desc <- function(df){
  df <- df %>% arrange(-count_19)
  df <- df[1:length(df)]
  df$row_num <- seq.int(nrow(df))
  total_num <- as.character(as.numeric(max(df$row_num, na.rm = TRUE)) + 2)
  na_num <- as.character(as.numeric(max(df$row_num, na.rm = TRUE)) + 1)
  df$row_num <- as.character(df$row_num)
  df <- df %>%
    mutate(row_num = case_when(variable_name == "Total" ~ total_num,
                               variable_name == "NA" ~ na_num,
                               TRUE ~ row_num))
  df$row_num <- as.numeric(df$row_num)
  df <- df %>% arrange(row_num) %>% dplyr::select(-row_num)
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

# Add line break to ggplot axis text x
fnc_addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

