

%>%

  mutate(booking_type = as.character(booking_type)) %>%

  # Unknown
  mutate(booking_type_standard = case_when((charge_desc == "TEMPORARY REMOVAL OR TRANSFER" |
                                              charge_desc == "RESIST ARREST OR DETENTION 642:2" |
                                              charge_desc == "DISORDERLY CONDUCT 644:2" |
                                              charge_desc == "DRIVING OR OPERATING UNDER THE INFLUENCE OF DRUGSOR LIQUOR 265-A:2" |
                                              charge_desc == "RESISTING ARREST 594:5"|
                                              charge_desc == "SIMPLE ASSAULT 631:2-A" |
                                              charge_desc == "VIOLATION OF PROTECTIVE ORDER")
                                           & booking_type == "PROTECTIVE CUSTODY"         ~ "Unknown",

                                           # Protective Custody
                                           str_detect("PROTECTIVE CUSTODY|PROTECTIVE CUSTODY/INTOXICATION", charge_desc) ~ "PROTECTIVE CUSTODY",


                                           TRUE ~ booking_type))

#


################################################################################

# Standardize booking types

################################################################################

# If PC hold is in charge description and it has been confirmed that the booking type is a PC hold,
#    change the booking_type (new variable, booking_type_withpcs, to preserve raw booking type) to a PC hold.
# Gets a more accurate count of other booking types and the number of PC holds.

adm_all <- adm_all %>%

  mutate(booking_type_withpcs =

           ###########
         # Change booking type to PC Hold
         ###########


         #county == "Coos"        no data





         #county == "Strafford"   no data


         TRUE ~ booking_type)) %>%

  select(id, booking_id, county, charge_desc, booking_type, booking_type_withpcs, sentence_status, release_type, everything())

# Combine some booking types together (some are the same or it makes sense to group them).
adm_all <- adm_all %>%
  mutate(booking_type_standard =
           case_when(# booking_type_withpcs == "DETAINEE REQUEST" |
             # booking_type_withpcs == "DETAINER"         |
             # booking_type_withpcs == "DETENTION ORDER"  |
             # booking_type_withpcs == "DETAINERS,WARRANTS,HOLDS" |
             #
             # booking_type_withpcs == "ARREST WARRANT" |
             # booking_type_withpcs == "ELECTRONIC BENCH WARRANT" |
             # booking_type_withpcs == "SUPERIOR COURT ARREST WARRANT" |
             # booking_type_withpcs == "WARRANT ARREST" |
             # booking_type_withpcs == "CAPIAS" |
             #
             # booking_type_withpcs == "24 HOUR DETENTION REQUEST" |
             # booking_type_withpcs == "FEDERAL HOLD" |
             # booking_type_withpcs == "HOLD FOR ANOTHER AGENCY" |
             # booking_type_withpcs == "HOLD SHEET" |
             # booking_type_withpcs == "OVERNIGHT HOLD" |
             # booking_type_withpcs == "ADULT ORDER OF COMMITMENT"       ~ "DETAINERS, WARRANTS, HOLDS",

             booking_type_withpcs == "ADMIN TRANSFER" |
               booking_type_withpcs == "ADMINISTRATIVE TRANSFER"         ~ "ADMINISTRATIVE TRANSFER",

             booking_type_withpcs == "BAIL ORDER" |
               booking_type_withpcs == "CIRCUIT COURT BAIL ORDER" |
               booking_type_withpcs == "SUPERIOR COURT BAIL ORDER"       ~ "BAIL ORDER (CIRCUIT/SUPERIOR)",

             booking_type_withpcs == "DRUG COURT" |
               booking_type_withpcs == "DRUG COURT SENTENCING ORDER"     ~ "DRUG COURT",

             booking_type_withpcs == "PROBATION" |
               booking_type_withpcs == "PROBATION DETENTION ORDER" |
               booking_type_withpcs == "PROBATION/PAROLE VIOLATION" |
               booking_type_withpcs == "PAROLE" |
               booking_type_withpcs == "VIOLATION OF PAROLE" |
               booking_type_withpcs == "VIOLATION OF PROBATION"          ~ "PROBATION/PAROLE (VIOLATION/DETENTION ORDER)",

             booking_type_withpcs == "CONVICTED" |      # not sure if this should be here
               booking_type_withpcs == "CONVICTED ROCK" | # not sure if this should be here
               booking_type_withpcs == "SENTENCED" |
               booking_type_withpcs == "SENTENCING" |
               booking_type_withpcs == "WALK IN-SENTENCED"               ~ "SENTENCED/SENTENCING",

             TRUE ~ booking_type_withpcs)) %>%

  select(id, booking_id, county, charge_desc, booking_type, booking_type_withpcs, booking_type_standard, sentence_status, release_type, everything())

# Change all "Unknown" to NA for booking types, sentence statuses, and release types
adm_all$booking_type[adm_all$booking_type                   == "UNKNOWN"] <- NA
adm_all$booking_type_standard[adm_all$booking_type_standard == "UNKNOWN"] <- NA
adm_all$sentence_status[adm_all$sentence_status             == "UNKNOWN"] <- NA
adm_all$release_type[adm_all$release_type                   == "UNKNOWN"] <- NA
