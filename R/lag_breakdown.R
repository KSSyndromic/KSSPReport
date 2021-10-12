#' Lag Breakdown
#'
#' @param data a data frame
#'
#' @return a data frame
lag_breakdown<-function(data){
  LagTime=data%>%
    select(C_Biosense_Facility_ID, C_BioSense_ID, Arrived_Date_Time, C_Visit_Date_Time, Message_Date_Time,
           Recorded_Date_Time, Diagnosis_Code)%>% 
    mutate(Arrived=as.POSIXct(Arrived_Date_Time,format="%Y-%m-%d %H:%M:%S"),
           Visit=as.POSIXct(C_Visit_Date_Time,format="%Y-%m-%d %H:%M:%S"))%>%
    group_by(C_BioSense_ID)%>%
    slice(which.min(Arrived))
  
  
  Time_Diff=LagTime%>%
    mutate(
      lag_Arrival_Visit=as.numeric(difftime(Arrived,Visit,units="hours")) ,
      category_Arrival_Visit=cut(lag_Arrival_Visit, breaks=c(-Inf, 24, 48, Inf), labels=c("<24 Hours","24-48 Hours",">48 Hours")))

  Lag_Summary=Time_Diff %>% ungroup %>%
    group_by(C_Biosense_Facility_ID)%>%
    count(Timeliness=category_Arrival_Visit)%>%
    mutate(prop_Arrival_Visit = round(prop.table(n)*100),1) %>%
    select(-n) %>%
    complete(C_Biosense_Facility_ID,Timeliness,fill = list(prop_Arrival_Visit = 0))

  
  return(Lag_Summary)
  
  
}