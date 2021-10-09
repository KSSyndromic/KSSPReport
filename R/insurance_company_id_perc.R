##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 
## If the input is empty, say "Missing"
## Insurance_Company_ID
#' Insurance Company ID Percent
#'
#' @param data a data frame
#'
#' @return a data frame
insurance_company_id_perc<-function(data){
  Insurance_Company_ID=data%>%
    select(C_BioSense_ID, Insurance_Company_ID)%>%
     mutate(Insurance_Company_ID=ifelse(is.na(Insurance_Company_ID),"Missing",Insurance_Company_ID))%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Insurance_Company_ID)%>%
    transmute(Insurance_Company_ID,count=n,percentage=round(100*n/sum(n),2))
  return(
    Insurance_Company_ID
  )
}
