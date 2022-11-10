# Justice Reinvestment Intiative - New Hampshire

This project focuses on the prevalence of people with behavioral health needs moving through local county jails and the availability and impact of existing services and supports for this population statewide.  

# Repository Structure

    |-- data_cleaning 
      |-- 00_library.R                           # Load packages
      |-- 01_functions.R                         # Custom functions for efficient coding
      |-- 01_functions_visuals.R                 # Custom functions for efficient coding

      |-- 02_import.R                            # Import jail data from sharepoint
      
      |-- 03_belknap.R                           # Standardize Belknap variables 
      |-- 04_carroll.R                           # Standardize Carroll variables
      |-- 05_cheshire.R                          # Standardize Cheshire variables
      |-- 06_coos.R                              # Standardize Coos variables
      |-- 07_hillsborough.R                      # Standardize Hillsborough variables
      |-- 08_merrimack.R                         # Standardize Merrimack variables
      |-- 09_rockingham.R                        # Standardize Rockingham variables
      |-- 10_strafford.R                         # Standardize Strafford variables
      |-- 11_sullivan.R                          # Standardize Sullivan variables
      
      |-- 12_standardize_counties.R              # Further standardize charges and booking types
      |-- 13_dataframes.R                        # Combine jail data and create df's used in analyses
      
    |-- data_cleaning -> analysis_code       
      |-- incarceration_patterns_bookings.R      # Analyze booking data and create viz deliverables
      |-- incarceration_patterns_entrances.R     # Analyze entrances data and create viz deliverables
      |-- pc_holds.R                             # Analyze PC holds
      |-- high_utilizers.R                       # Analyze HU's (1%, 5%, 10%)
      |-- non_high_utilizers.R                   # Analyze non-HU's (1%, 5%, 10%)
      |-- los.R                                  # Analyze LOS
      |-- data_availability.R                    # Table that shows data availability by county
      |-- demographics.R                         # Analyze demographics

      |-- county_reports_tables.R                # Create tables for county reports
      |-- county_reports_plots.R                 # Create plots for county reports
      |-- generate_county_reports.R              # Generate county report RMD's automatically
      
# Data  

- Administrative and behavioral treatment related files from jail partners
- Matched Medicaid data from DHHS

