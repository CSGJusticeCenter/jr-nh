# Justice Reinvestment Intiative - New Hampshire

This project focuses on the prevalence of people with behavioral health needs moving through local county jails and the availability and impact of existing services and supports for this population statewide.  

# Repository Structure

    |-- data_cleaning 
      |-- 00_library.R                              # Load packages
      |-- 01_functions.R                            # Custom functions for efficient coding
      |-- 01_functions_visuals.R                    # Custom functions for efficient coding
      |-- 02_import.R                               # Import jail and medicaid data from sharepoint and D:
      |-- 03_belknap.R                              # Standardize and clean Belknap files 
      |-- 04_carroll.R                              # Standardize and clean Carroll files
      |-- 05_cheshire.R                             # Standardize and clean Cheshire files
      |-- 06_coos.R                                 # Standardize and clean Coos files
      |-- 07_hillsborough.R                         # Standardize and clean Hillsborough files
      |-- 08_merrimack.R                            # Standardize and clean Merrimack files
      |-- 09_rockingham.R                           # Standardize and clean Rockingham files
      |-- 10_strafford.R                            # Standardize and clean Strafford files
      |-- 11_sullivan.R                             # Standardize and clean Sullivan files
      |-- 12_dataframes.R                           # Combine jail data and create df's used in analyses
      |-- rdas.R                                    # Files used in rmds
      |-- render_site                               # Can use this file to run all code and render Netlify site
      
      
    |-- data_cleaning -> analysis_code       
      |-- 01_incarceration_patterns_bookings.R      # Analyze booking data and create viz deliverables
      |-- 02_incarceration_patterns_entrances.R     # Analyze entrances data and create viz deliverables
      |-- 03_pc_holds.R                             # Analyze PC holds and create viz deliverables
      |-- 04_high_utilizers.R                       # Analyze HU's (1%, 5%, 10%) and create viz deliverables
      |-- 05_non_high_utilizers.R                   # Analyze non-HU's (1%, 5%, 10%) and create viz deliverables
      |-- 06_los.R                                  # Analyze LOS and create viz deliverables
      |-- 07_data_availability.R                    # Table that shows data availability by county
      |-- 08_demographics.R                         # Analyze demographics and create viz deliverables
      |-- 09_county_reports_tables.R                # Create tables for county reports
      |-- 10_county_reports_plots.R                 # Create plots for county reports
      |-- 11_generate_county_reports.R              # Generate county report RMD's automatically
      
# Data  

- Administrative and behavioral treatment related files from jail partners
- Matched Medicaid data from DHHS

