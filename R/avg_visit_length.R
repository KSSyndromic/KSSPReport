##compute the average length of each visit

avg_visit_length<-function(data){
  Avg_Visit_Length=data%>%
    select(C_Biosense_Facility_ID, C_BioSense_ID, C_Visit_Date_Time,Discharge_Date_Time)%>%
    mutate(Visit=as.POSIXct(C_Visit_Date_Time,format="%Y-%m-%d %H:%M:%S"),
           Discharge=as.POSIXct(Discharge_Date_Time,format="%Y-%m-%d %H:%M:%S"),
           Visit_Length=as.numeric(difftime(Discharge,Visit,units="hours")))%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    summarise(Visit_Length=round(mean(Visit_Length,na.rm=TRUE),2))
  
  return(
    Avg_Visit_Length
  )
}
