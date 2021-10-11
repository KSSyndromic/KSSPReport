##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 
## If the input is empty, say "Missing"
##country
country_perc<-function(data){
  Country=data%>%
    select(C_BioSense_ID, Patient_Country)%>%
    mutate(Patient_Country=ifelse(is.na(Patient_Country),"Missing",Patient_Country))%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Patient_Country)%>%
    transmute(Patient_Country,count=n,percentage=round(100*n/sum(n),2))
  return(
    Country
  )
}
