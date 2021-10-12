##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for.
## If the input is empty, say "Missing"
## C_MFT_Patient_Class

#' Frequency of occurrence in data - what is actually occurring in the data 
#'
#' @param data a data frame
#'
#' @return a data frame
patient_class_perc<-function(data){
  number_visits=n_groups(group_by(data,C_BioSense_ID))
  
  Patient_Class=data%>%
    select(C_BioSense_ID, Patient_Class_Code)%>%
    mutate(Patient_Class_Code=ifelse(is.na(Patient_Class_Code),"Missing",Patient_Class_Code))%>%
    distinct(C_BioSense_ID,Patient_Class_Code,.keep_all=TRUE)%>%
    count(Patient_Class_Code)%>%
    transmute(Patient_Class_Code,count=n,percentage=round(100*n/number_visits,2))
  return(
    Patient_Class
  )
}
