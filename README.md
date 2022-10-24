# Justice Reinvestment Intiative - New Hampshire

This project focuses on the prevalence of people with behavioral health needs moving through local county jails and the availability and impact of existing services and supports for this population statewide.  

# Repository Structure

    |-- data_cleaning 
      |-- 00_library.R                           # Load packages
      |-- 01_functions.R                         # Custom functions for efficient coding
      |-- 01_functions_visuals.R                 # Custom functions for efficient coding

      |-- 02_import.R                            # Import jail data from sharepoint
      
      |-- 03_belknap.R                           # Standardize Belknap jail data 
      |-- 04_carroll.R                           # Standardize Carroll jail data 
      |-- 05_cheshire.R                          # Standardize Cheshire jail data 
      |-- 06_coos.R                              # Standardize Coos jail data 
      |-- 07_hillsborough.R                      # Standardize Hillsborough jail data 
      |-- 08_merrimack.R                         # clean Merrimack jail data 
      |-- 09_rockingham.R                        # Standardize Rockingham jail data 
      |-- 10_strafford.R                         # Standardize Strafford jail data 
      |-- 11_sullivan.R                          # Standardize Sullivan jail data
      
      |-- 12_dataframes.R                        # Combine jail data and create df's used in analysis
      
      |-- 13_incarceration_patterns_bookings.R   # Analyze booking data and create viz deliverables
      |-- 14_incarceration_patterns_entrances.R  # Analyze entrances data and create viz deliverables
      |-- 15_pc_holds.R                          # Analyze PC holds
      |-- 16_high_utilizers.R                    # Analyze HU's (1%, 5%, 10%)
      |-- 17_non_high_utilizers.R                # Analyze non-HU's (1%, 5%, 10%)
      |-- 18_los.R                               # Analyze LOS
      |-- 19_data_availability.R                 # Table that shows data availability by county
      |-- 20_demographics.R                      # Analyze demographics
      |-- 21_charges.R                           # Create charge categories
      
      |-- 22_county_reports_tables.R             # Create tables for county reports
      |-- 23_county_reports_plots.R              # Create plots for county reports
      |-- 24_generate_county_reports.R           # Generate county report RMD's automatically
      
# Data  

- Administrative and behavioral treatment related files from jail partners
- Matched Medicaid data from DHHS

