##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 
## If the input is empty, say "Missing"
## State
state_perc<-function(data){
  State=data%>%
    select(C_BioSense_ID, Patient_State)%>%
    mutate(Patient_State=ifelse(is.na(Patient_State),"Missing",Patient_State))%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Patient_State)%>%
    transmute(Patient_State,count=n,percentage=round(100*n/sum(n),2))
  return(
    State
  )
}
