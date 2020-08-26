##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 
## If the input is empty, say "Missing"
## Race_Code
race_code_perc<-function(data){
  Race_Code=data%>%
    select(C_BioSense_ID,Race_Code)%>%
    mutate(Race_Code=ifelse(is.na(Race_Code),"Missing",Race_Code))%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Race_Code)%>%
    transmute(Race_Code,count=n,percentage=round(100*n/sum(n),2))
  
  return(
    Race_Code
  )
  
}
