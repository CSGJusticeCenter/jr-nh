##########

# reactable table with number of people and proportion people enter for PC holds
# includes coos and strafford in the table but not the calculation

##########

# select variables
# count number of pc holds vs non-pc holds by county by fiscal year
df1 <- df_pch %>%
  ungroup() %>%
  select(id, fy, county, pc_hold_in_booking) %>%
  distinct()
dim(df1); length(unique(df1$id))
table(df1$pc_hold_in_booking)

df2 <- df1 %>%
  group_by(county, pc_hold_in_booking) %>%
  dplyr::summarise(total = n()) # 1161

# select variables
# count number of pc holds vs non-pc holds by county by fiscal year
df1 <- df_pch %>%
  ungroup() %>%
  select(id, county, pc_hold_in_booking) %>%
  distinct()
dim(df1); length(unique(df1$id))
table(df1$pc_hold_in_booking)

df2 <- df1 %>%
  group_by(county, pc_hold_in_booking) %>%
  dplyr::summarise(total = n()) # 1161
