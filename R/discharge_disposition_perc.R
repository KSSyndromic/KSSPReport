##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 
## If the input is empty, say "Missing"
## discharge disposition
discharge_disposition_perc<-function(data){
  number_visits=n_groups(group_by(data,C_BioSense_ID))
  
  Discharge_Disposition=data%>%
    select(C_BioSense_ID, Discharge_Disposition)%>%
    mutate(Discharge_Disposition=ifelse(is.na(Discharge_Disposition),"Missing",Discharge_Disposition))%>%
    distinct(C_BioSense_ID,Discharge_Disposition,.keep_all=TRUE)%>%
    count(Discharge_Disposition)%>%
    transmute(Discharge_Disposition,count=n,percentage=round(100*n/number_visits,2))
  return(
    Discharge_Disposition
  )
}
