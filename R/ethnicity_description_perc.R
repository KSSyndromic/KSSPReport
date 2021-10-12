##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 
## If the input is empty, say "Missing"
## for variable Race_Description

## Ethnicity_Description
#' Ethnicity_Description Percent
#'
#' @param data a data frame
#' @importFrom graphics title
#' @importFrom utils data
#'
#' @return a data frame
ethnicity_description_perc<-function(data){
  Ethnicity_Description=data%>%
    select(C_BioSense_ID,Ethnicity_Description)%>%
    mutate(Ethnicity_Description=toupper(Ethnicity_Description),
           Ethnicity_Description=ifelse(is.na(Ethnicity_Description),"Missing",Ethnicity_Description))%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Ethnicity_Description)%>%
    transmute(Ethnicity_Description,count=n,percentage=round(100*n/sum(n),2))
  
  return(
    Ethnicity_Description
  )
  
}
