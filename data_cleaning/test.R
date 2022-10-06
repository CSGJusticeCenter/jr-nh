
county_adm_19 <- nh_sentence_19 %>% filter(county == "Sullivan")
county_adm_20 <- nh_sentence_20 %>% filter(county == "Sullivan")
county_adm_21 <- nh_sentence_21 %>% filter(county == "Sullivan")

df1 <- data.frame(summarytools::freq(county_adm_19$sex, order = "freq", cum.percent = FALSE))
df1 <- df1 %>% tibble::rownames_to_column("sex") %>%
  filter(sex != "<NA>") %>%
  dplyr::select(sex,
                count = Freq,
                pct   = X..Valid)

df2 <- data.frame(summarytools::freq(county_adm_20$sex, order = "freq", cum.percent = FALSE))
df2 <- df2 %>% tibble::rownames_to_column("sex") %>%
  dplyr::select(sex,
                count = Freq,
                pct   = X..Valid)

df3 <- data.frame(summarytools::freq(county_adm_21$sex, order = "freq", cum.percent = FALSE))
df3 <- df3 %>% tibble::rownames_to_column("sex") %>%
  dplyr::select(sex,
                count = Freq,
                pct   = X..Valid)

# get count and prop of sex by FY
sex_19 <- fnc_sex_by_year(df1)
sex_20 <- fnc_sex_by_year(df2)
sex_21 <- fnc_sex_by_year(df3)

# rename variables for merging, indicate which year
sex_19 <- sex_19 %>% dplyr::rename(count_19 = count,
                                   pct_19   = pct)
sex_20 <- sex_20 %>% dplyr::rename(count_20 = count,
                                   pct_20   = pct)
sex_21 <- sex_21 %>% dplyr::rename(count_21 = count,
                                   pct_21   = pct)

# join data
df <- merge(sex_19, sex_20, by = "sex", all.x = TRUE, all.y = TRUE)
df <- merge(df, sex_21, by = "sex", all.x = TRUE, all.y = TRUE)

# arrange table data
df <- fnc_sex_data_desc(df)

# create row totals and frequencies
df[df == "NA%"] = NA
df <- df %>%
  filter(sex != "Total")
df <- fnc_row_totals(df)
