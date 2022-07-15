
wd <- getwd()
  
library("rmarkdown")
for (county in unique(nh_adm$county)){
  subgroup <- county
  render("county_adm_template.Rmd",
         output_file = paste0(county, '_county_report', '.html'),
         output_dir = paste0(wd, "/_site"))    
}

