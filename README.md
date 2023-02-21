# Justice Reinvestment Intiative - New Hampshire

This project focuses on the prevalence of people with behavioral health needs moving through local county jails and the availability and impact of existing services and supports for this population statewide.   

# Repository Structure

    |-- data_cleaning 
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
      |-- 15_costs.R                                # Analyze costs per day   
      |-- rdas.R                                    # Files used in rmds and netlify  

      
# Data  

- Administrative and behavioral treatment related files from jail partners  
- Matched Medicaid data from DHHS  

