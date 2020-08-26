##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 
## If the input is empty, say "Missing"
##Facility_Type_Description

facility_type_description_perc<-function(data){
  number_visits=n_groups(group_by(data,C_BioSense_ID))
  
  Facility_Type_Description=data%>%
    select(C_BioSense_ID, Facility_Type_Description)%>%
    mutate(Facility_Type_Description=toupper(Facility_Type_Description),
           Facility_Type_Description=ifelse(is.na(Facility_Type_Description),"Missing",Facility_Type_Description))%>%
    distinct(C_BioSense_ID,Facility_Type_Description,.keep_all=TRUE)%>%
    count(Facility_Type_Description)%>%
    transmute(Facility_Type_Description,count=n,percentage=round(100*n/number_visits,2))
  return(
    Facility_Type_Description
  )
}
