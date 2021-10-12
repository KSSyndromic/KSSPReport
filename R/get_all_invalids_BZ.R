#' Get Invalid Summaries
#' 
#' This function will call upon each of the functions that end with `_invalid` and combine the results from all into one data frame. All
#' invalid checks were done at the patient-visit-level: If *one* message in a patient-visit was found to be invalid, then the whole patient visit
#' was returned as invalid.
#' 
#' @param data The raw data from BioSense on which you will do the invalid checks.
#' @return A data frame that contains the results from all of the `_invalid` functions.
#' @import dplyr
#' @import tidyr
#' @export
get_all_invalids_BZ <- function(data) {
  
  # merge all invalid summaries
  # make summaries into a list
  invalid_summaries <- list(admit_source_invalid(data)[[2]], age_invalid(data)[[2]], any_e_invalid(data)[[2]],
                            blood_pressure_invalid(data)[[2]], cc_ar_invalid(data)[[2]], country_invalid(data)[[2]],
                            county_invalid(data)[[2]], date_time_invalid(data)[[2]],
                            death_invalid(data)[[2]], diagnosis_code_invalid(data)[[2]],diagnosis_type_invalid(data)[[2]], 
                            discharge_disposition_invalid(data)[[2]], ethnicity_invalid(data)[[2]], 
                            facility_type_invalid(data)[[2]], fpid_mrn_invalid(data)[[2]],
                            gender_invalid(data)[[2]], height_invalid(data)[[2]], patient_class_invalid(data)[[2]],
                            pulseox_invalid(data)[[2]], race_invalid(data)[[2]], smoking_status_invalid(data)[[2]],
                            state_invalid(data)[[2]], temperature_invalid(data)[[2]], weight_invalid(data)[[2]], 
                            zip_invalid(data)[[2]])
  
  state_invalids <- invalid_summaries[[1]] # initialize data frame
  for (i in 2:length(invalid_summaries)) { # for the rest of the data frames in the list
    state_invalids <- full_join(state_invalids, 
                                invalid_summaries[[i]], # join them to the state invalids summary 
                                by="C_Biosense_Facility_ID") # by facility id
  }
  
  # clean up state invalids
  state_invalids <- state_invalids %>% # take invalids
    gather(key, value, 2:ncol(.)) %>% # gather all columns, besides facility
    separate(key, c("Field", "Measure"), "\\.") %>% # separate field and type out by period
    spread(Field, value) %>% # spread it back out by facility
    select(-c(Missing_Death_Date_Time_Given_Indicator, Missing_Death_Given_Discharge_Disposition, Missing_Death_Indicator_Given_Date_Time,
              Height_Missing_Given_Height_Units,Height_Units_Missing_Given_Height,Initial_Pulse_Oximetry_Missing_Given_Units,
              Initial_Pulse_Oximetry_Units_Missing_Given_Pulse,Initial_Temp_Missing_Given_Units,Initial_Temp_Units_Missing_Given_Temp,
              Initial_Temp_Out_Of_Range,FPID_MRN_Mismatch,Systolic_Diastolic_Blood_Pressure_Missing_Given_BP_Units,
              Systolic_Diastolic_Blood_Pressure_Units_Missing_Given_BP,Visits_With_No_E_Pt_Class,Weight_Missing_Given_Weight_Units,Weight_Units_Missing_Given_Weight,
              Admit_Reason_Description_Short,Create_Processed_Date_Time,Create_Raw_Date_Time,Update_Processed_Date_Time)) %>%
    as.data.frame() # make as a classic data frame for writing to xlsx
  state_invalids[state_invalids=="NaN"] <- NA # replace nan with na
  
  return(
    merge(data%>%
            select(C_Biosense_Facility_ID)%>%
            distinct(),state_invalids, by="C_Biosense_Facility_ID")
  )
}
