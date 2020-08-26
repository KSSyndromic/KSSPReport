##compute the average number of visits per day for each facility

avg_visit_per_day<-function(data){
  visit_per_day=data%>%
    select(C_Biosense_Facility_ID, C_BioSense_ID, C_Visit_Date_Time, C_Visit_Date)%>%
    group_by(C_Visit_Date)%>%
    distinct(C_BioSense_ID)%>%
    summarise(count=n())
  return(
    round(mean(visit_per_day$count),2)
  )
  
}
