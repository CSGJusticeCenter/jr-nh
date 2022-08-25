############################################
# Project: JRI New Hampshire
# File: loop.R
# Last updated: August 22, 2022
# Author: Mari Roberts

# combine county files
# loop through counties to generate plots
############################################

######
# Highchart: PC holds over time
######

counties_pch_time_highchart <- map(.x = counties,  .f = function(x) {
  df <- nh_booking %>% filter(county == x)
  fnc_pch_time_highchart(df)
})

counties_pch_time_highchart <-
  setNames(counties_pch_time_highchart,c("Belknap","Carroll","Cheshire", "Coos","Merrimack", "Sullivan","Rockingham"))

counties_pch_time_highchart$Belknap

######
# Save to SP
######

save(counties_pch_time_highchart, file=paste0(sp_data_path, "/Data/r_data/counties_pch_time_highchart.Rda",   sep = ""))
