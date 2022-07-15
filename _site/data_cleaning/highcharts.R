##############################################################################
# Highcharts
##############################################################################

hcmap("countries/us/us-nh-all") %>%
  hc_title(text = "New Hampshire Jail Admissions") %>%
  hc_subtitle(text = "Hover over your county to view jail admissions information from 2019-2021")
