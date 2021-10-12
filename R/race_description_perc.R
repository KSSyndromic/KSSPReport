##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 
## If the input is empty, say "Missing"
## for variable Race_Description

race_description_perc<-function(data){
  Race_Description=data%>%
    select(C_BioSense_ID,Race_Description)%>%
    mutate(Race_Description=toupper(Race_Description),
           Race_Description=ifelse(is.na(Race_Description),"Missing",Race_Description))%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Race_Description)%>%
    transmute(Race_Description,count=n,percentage=round(100*n/sum(n),2))
  
  return(
    Race_Description
  )
  
}
