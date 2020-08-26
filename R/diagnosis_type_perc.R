
##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 
## If the input is empty, say "Missing"
## Diagnosis_Type

diagnosis_type_perc<-function(data){
   number_visits=n_groups(group_by(data,C_BioSense_ID))
   
   Diagnosis_Type=data%>%
    select(C_BioSense_ID, Diagnosis_Type)%>%
    mutate(Diagnosis_Type=ifelse(is.na(Diagnosis_Type),"Missing",Diagnosis_Type))%>%
    distinct(C_BioSense_ID,Diagnosis_Type,.keep_all=TRUE)%>%
    count(Diagnosis_Type)%>%
    transmute(Diagnosis_Type,count=n,percentage=round(100*n/number_visits,2))
  return(
    Diagnosis_Type
  )
}
