## similar as va_lag.R, but we compute the time difference for the earliest Recorded_Date_Time
early_lag <- function(data) {
  LagTime=data %>%
    select(C_Biosense_Facility_ID, C_BioSense_ID, Arrived_Date_Time, C_Visit_Date_Time, Message_Date_Time, Recorded_Date_Time)%>% 
    mutate(Arrived=as.POSIXct(Arrived_Date_Time,format="%Y-%m-%d %H:%M:%S"),
           Visit=as.POSIXct(C_Visit_Date_Time,format="%Y-%m-%d %H:%M:%S"),
           Message=as.POSIXct(Message_Date_Time,format="%Y-%m-%d %H:%M:%S"),
           Record=as.POSIXct(Recorded_Date_Time,format="%Y-%m-%d %H:%M:%S")
    )
  
  
  Early_Lag=LagTime%>%
    group_by(C_BioSense_ID)%>%
    slice(which.min(Record))
  
  
  Time_Diff=Early_Lag%>%
    mutate(lag_Record_Visit=as.numeric(difftime(Record,Visit,units="hours")),
           lag_Message_Record=as.numeric(difftime(Message,Record,units="hours")),
           lag_Arrival_Message=as.numeric(difftime(Arrived,Message,units="hours")),
           lag_Arrival_Visit=as.numeric(difftime(Arrived,Visit,units="hours"))         
    )
  
 
  Lag_Summary=Time_Diff %>%
    group_by(C_Biosense_Facility_ID)%>%
    summarise(Record_Visit=round(mean(lag_Record_Visit,na.rm=TRUE),2),
              Message_Record=round(mean(lag_Message_Record,na.rm=TRUE),2),
              Arrival_Message=round(mean(lag_Arrival_Message,na.rm=TRUE),2),
              Arrival_Visit=round(mean(lag_Arrival_Visit,na.rm=TRUE),2)
             )
  
  
  return(Lag_Summary)
}
