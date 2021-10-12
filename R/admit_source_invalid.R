
#' Getting Invalid Examples and Summaries for Admit_Source
#'
#' @param data The raw data from BioSense on which you will do the invalid admit source checks.
#'
#' @return A list of two data frames: examples and summary for invalid Admit_Source.
#' @import dplyr
#' 
#' @export 
#'
#' @examples
#' \dontrun{
#'   admit_source_invalid(df)
#' }
admit_source_invalid <- function(data) {
  
  # import the valid admit values
  data("admit_source", envir=environment())
  
  valid_admit_values <- admit_source %>% # get value sets
    select(Concept.Code) %>% # the variable we want is called concept code
    filter(!is.na(Concept.Code)) %>% # get rid of any nas
    c() %>% # turn this into a vector
    unlist() %>% # unlist them from the concept name object 
    unname() %>% # remove the names to leave bare values
    # we will accept zero in front of any of these, so include those values
    c(., unlist(lapply(., function(x) paste0("0", x))))
  
  invalid_admit_examples <- data %>% # take data
    select(c(C_Biosense_Facility_ID, C_BioSense_ID, Admit_Source)) %>% # taking just variables we need
    mutate(Admit_Source=as.character(Admit_Source), # make admit source character
           Invalid_Admit_Source=case_when(
             is.na(Admit_Source) ~ NA, # if admit source is na, then invalid will be na
             Admit_Source %in% valid_admit_values ~ FALSE, # if admit source is in the valid list, invalid is false
             !Admit_Source %in% valid_admit_values ~ TRUE # if it is not, then invalid is true
           ))
  
  invalid_admit_summary <- invalid_admit_examples %>% # take these examples
    group_by(C_BioSense_ID) %>% # group by patient visit
    mutate(Any_Invalid=case_when( # making variable for if any is invalid
      all(is.na(Invalid_Admit_Source)) ~ NA, # if all the invalid checks are na, keep na
      sum(Invalid_Admit_Source, na.rm=TRUE) == 0 ~ FALSE, # if the sum of the ones that aren't na is 0, then false
      TRUE ~ TRUE # otherwise, invalid patient visit
      )) %>% 
    slice(1) %>% # get one observation per visit
    ungroup() %>% # explicitly ungroup by patient visit
    group_by(C_Biosense_Facility_ID) %>% # group by facility
    summarise(Admit_Source.Percent=round(mean(Any_Invalid, na.rm=TRUE)*100,2), # get percent
              Admit_Source.Count=sum(Any_Invalid, na.rm=TRUE)) # get count
  
  return(
    list(invalid_admit_examples=invalid_admit_examples,
         invalid_admit_summary=invalid_admit_summary)
  )
}
