
library(dplyr)
temp <- nh_booking %>% select(county,high_utilizer_1_pct, booking_id) %>% filter(county == "Belknap" ) %>% distinct()
dim(temp) # 319
length(unique(temp$booking_id))
dups <- temp[duplicated(temp$booking_id)|duplicated(temp$booking_id, fromLast=TRUE),]


temp <- nh_booking %>% filter(county == "Belknap" & high_utilizer_1_pct == "No" & fy == 2019)
dim(temp) # 1593
temp <- nh_booking %>% filter(county == "Belknap" & high_utilizer_1_pct == "No" & fy == 2020)
dim(temp) # 1275
temp <- nh_booking %>% filter(county == "Belknap" & high_utilizer_1_pct == "No" & fy == 2021)
dim(temp) # 970
1593+1275+970 # 3838

temp <- nh_booking %>% filter(county == "Belknap" & high_utilizer_1_pct == "Yes" & fy == 2019)
dim(temp) # 128
temp <- nh_booking %>% filter(county == "Belknap" & high_utilizer_1_pct == "Yes" & fy == 2020)
dim(temp) # 106
temp <- nh_booking %>% filter(county == "Belknap" & high_utilizer_1_pct == "Yes" & fy == 2021)
dim(temp) # 85
128+106+85 # 319

3838 + 319 # 4157


# 1%
county_hu_1_pct_prop <- map(.x = counties,  .f = function(x) {
  df_19 <- nh_booking_19 %>% filter(county == x)
  df_20 <- nh_booking_20 %>% filter(county == x)
  df_21 <- nh_booking_21 %>% filter(county == x)
  df <- fnc_variable_table(df_19, df_20, df_21, "high_utilizer_1_pct")
  df <- df %>% mutate(county = x) %>% select(high_utilizer_1_pct = variable_name, everything()) %>%
    filter(high_utilizer_1_pct != "Total")
})

county_hu_1_pct_prop <- bind_rows(county_hu_1_pct_prop)


df <- nh_booking %>% filter(county == "Belknap" & fy == 2021)
df1 <- data.frame(summarytools::freq(df$high_utilizer_1_pct, order = "freq", cum.percent = FALSE))
df1 <- df1 %>% tibble::rownames_to_column("high_utilizer_1_pct") %>%
  dplyr::select(high_utilizer_1_pct,
                count = Freq,
                pct   = X..Valid)
df1 # 970 87 variable_name, county, pct
# on website = 967 + 85


df <- nh_booking %>% filter(county == "Belknap" & fy == 2021) %>%
  select(high_utilizer_1_pct, fy, county, booking_id) %>% distinct() # NEW, may cause issues

df1 <- table(df$high_utilizer_1_pct, df$fy, df$county)
df1 <- as.data.frame(df1)
df1 <- df1 %>% select(high_utilizer_1_pct = Var1,
                      fy = Var2,
                      county = Var3,
                      new_variable_name = Freq)

df <- df %>% select(variable_name, fy, county, booking_id) %>% distinct() # NEW, may cause issues
df1 <- table(df$variable_name, df$fy, df$county)
df1 <- as.data.frame(df1)
df1 <- df1 %>% select(variable_name = Var1,
                      fy = Var2,
                      county = Var3,
                      new_variable_name = Freq)
