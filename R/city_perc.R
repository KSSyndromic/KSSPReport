##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 
##If the input is empty, say "Missing"
##city

#' Frequency by City
#'
#' @param data a data frame 
#'
#' @return a data frame
city_perc<-function(data){
  City=data%>%
    select(C_BioSense_ID, Patient_City)%>%
     mutate(Patient_City=toupper(Patient_City),
            Patient_City=ifelse(is.na(Patient_City),"Missing",Patient_City))%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Patient_City)%>%
    transmute(Patient_City,count=n,percentage=round(100*n/sum(n),2))
  return(
    City
  )
}
