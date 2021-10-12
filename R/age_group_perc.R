##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 
##For varaible C_Patient_Age_Years
#' Get variable C_Patient_Age_Years Frequency
#'
#' @param data a data frame
#'
#' @return a data frame
age_group_perc<-function(data){
  Age_Group=data%>%
    select(C_BioSense_ID,C_Patient_Age_Years)%>%
    mutate(Age_Group=case_when(C_Patient_Age_Years<= 4 ~"0-4",
                               C_Patient_Age_Years<=17 ~"5-17",
                               C_Patient_Age_Years<=44 ~"18-44",
                               C_Patient_Age_Years<=64 ~"45-64",
                               C_Patient_Age_Years>=65 ~"65+") )%>% ## create age groups for each visit
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Age_Group)%>%
    transmute(Age_Group,count=n, percentage=round(100*n/sum(n),2))
  
  return(
    Age_Group
  )
  
}
