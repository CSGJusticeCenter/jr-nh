# Justice Reinvestment Intiative - New Hampshire

This project focuses on the prevalence of people with behavioral health needs moving through local county jails and the availability and impact of existing services and supports for this population statewide.   

# Repository Structure

    |-- code
      |-- 00_library.R                              # Load packages   
      |-- 01_functions.R                            # Custom functions for efficient coding  
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
      |-- 12_charges.R                              # Clean charge data for all jail files  
      |-- 13_dataframes.R                           # Combine jail data for analysis  
      |-- 14_medicaid.R                             # Creates Medicaid files for analysis  
      |-- 15_costs.R                                # Analyze jail costs per day   
      |-- rdas.R                                    # Files used in rmds and netlify 
      
    |-- netlify pages
      |-- nh_booking_types_analysis.Rmd                      # Jail data: booking trends  
      |-- nh_charge_type_analysis.Rmd                        # Jail data: booking charge types  
      |-- nh_data_availability.Rmd                           # Jail data: metrics provided by each county  
      |-- nh_data_dictionary.Rmd                             # Jail data: data dictionary for each county   
      |-- nh_demographics_analysis.Rmd                       # Jail data: demographics overview  
      |-- nh_high_utilizers_analysis.Rmd                     # DHHS data: high utilizers overview  
      |-- nh_homeless_analysis.Rmd                           # Jail data: homelessness at time of booking  
      |-- nh_incarceration_patterns.Rmd                      # DHHS data: overall incarceration patterns      
      |-- nh_los_analysis.Rmd                                # Jail/DHHS data: length of stay      
      |-- nh_medicaid_bh_encounters_mh_sud.Rmd               # DHHS data: Medicaid BH claims      
      |-- nh_medicaid_bh_encounters_visits.Rmd               # DHHS data: Medicaid ED visits       
      |-- nh_medicaid_diagram_codebook.Rmd                   # DHHS data: diagram of how Medicaid files were merged and created     
      |-- nh_medicaid_eligibility_jail_timing_analysis.Rmd   # DHHS data: Medicaid eligibility and timing      
      |-- nh_medicaid_homelessness_analysis.Rmd              # DHHS data: homlessness when enrolling in Medicaid  
      |-- nh_medicaid_match_bh_overview_analysis.Rmd         # DHHS data: match to Medicaid overview      
      |-- nh_medicaid_reimbursement_costs.Rmd                # DHHS data: reimbursement to service providers  
      |-- nh_racial_disparities.Rmd                          # DHHS data: racial/ethnic disparities in incarceration  
      |-- nh_resource_map.Rmd                                # Maps showing location of BH services in NH  

# Data  

- Administrative and behavioral treatment related files from jail partners  
- Matched Medicaid data from DHHS  

