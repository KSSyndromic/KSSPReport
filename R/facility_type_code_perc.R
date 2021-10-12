##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 
## If the input is empty, say "Missing"
##Facility_Type_Code

#' Facility Type Code Percent
#'
#' @param data a data frame
#'
#' @return a data frame
facility_type_code_perc<-function(data){
  number_visits=n_groups(group_by(data,C_BioSense_ID))
  
  Facility_Type_Code=data%>%
    select(C_BioSense_ID, Facility_Type_Code)%>%
    mutate(Facility_Type_Code=ifelse(is.na(Facility_Type_Code),"Missing",Facility_Type_Code))%>%
    distinct(C_BioSense_ID,Facility_Type_Code,.keep_all=TRUE)%>%
    count(Facility_Type_Code)%>%
    transmute(Facility_Type_Code,count=n,percentage=round(100*n/number_visits,2))
  return(
    Facility_Type_Code
  )
}
