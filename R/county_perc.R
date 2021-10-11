##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for.
## If the input is empty, say "Missing"
##county
#' Frequency by County
#'
#' @param data a data frame
#'
#' @return a data frame
county_perc<-function(data){
  County=data%>%
    select(C_BioSense_ID, C_Patient_County)%>%
    mutate(C_Patient_County=ifelse(is.na(C_Patient_County),"Missing",C_Patient_County))%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(C_Patient_County)%>%
    transmute(C_Patient_County,count=n,percentage=round(100*n/sum(n),2))
  return(
    County
  )
}
