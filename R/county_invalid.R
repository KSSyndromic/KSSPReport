#' Getting Invalid Examples and Summaries for Patient_State
#'
#' This function will generate two data frames: first, a frame to be used later to extract invalid examples from;
#' second, a frame that contains facility-level summaries for counts and percentages of invalid Patient_State.
#' 
#' The valid values were taken from the `PHVS_State_FIPS_5-2_V1.xls` file from 
#' Public Health Information Network Vocabulary Access and Distribution System value sets
#' (https://phinvads.cdc.gov/vads/ViewView.action?name=Syndromic%20Surveillance). The package will
#' be updated as the CDC provides new or different codes that are considered valid or invalid.
#' 
#' You can view the concept codes that are considered valid by calling `data("state")`. This data frame
#' leaves off the zeros in front of single-digit codes; codes with zeros in front of them are explicitly added
#' in the code for the function.
#' 
#' @param data The raw data from BioSense on which you will do the invalid patient state checks.
#' @return A list of two data frames: examples and summary for the Patient_State.
#' @import dplyr
#' @export
county_invalid <- function(data) {
  
  # generating valid values
  data("county",envir=environment())
  
  valid_county_values <- county %>% # take data
    select(Concept.Code) %>% # the variable we want is called concept code
    filter(!is.na(Concept.Code)) %>% # get rid of any nas
    c() %>% # turn this into a vector
    unlist() %>% # unlist them from the concept name object 
    unname()  # remove names
 
  
  # generate examples
  county_examples <- data %>% # take data
    select(c(C_Biosense_Facility_ID, C_BioSense_ID, C_Patient_County)) %>%  # taking just the variables we need
    mutate(Invalid_Patient_County=case_when(
      is.na(C_Patient_County) ~ NA, # if na then keep na
      C_Patient_County %in% valid_county_values ~ FALSE, # if county is found in valid values, false
      !C_Patient_County %in% valid_county_values ~ TRUE # if it isn't, then true
      ))
  
  # generate summary
  county_summary <- county_examples %>% # take the examples
    group_by(C_BioSense_ID) %>% # group by patient visit
    mutate(Any_Invalid_Patient_County=case_when(
      all(is.na(Invalid_Patient_County)) ~ NA, # if all is na, keep na
      sum(Invalid_Patient_County, na.rm=TRUE) == 0 ~ FALSE, # if none are true, then invalid is false
      TRUE ~ TRUE # otherwise, true
    )) %>%
    slice(1) %>% # take one row per patient visit
    ungroup() %>% # explicitly ungroup
    group_by(C_Biosense_Facility_ID) %>% # group by facility
    summarise(Patient_County.Percent=round(mean(Any_Invalid_Patient_County, na.rm=TRUE)*100,2), # percent
              Patient_County.Count=sum(Any_Invalid_Patient_County, na.rm=TRUE)) # count
  
  return(
    list(county_examples=county_examples,
         county_summary=county_summary)
  )
}
