## Lag by trigger event

#' Raise Lag by Trigger Event
#'
#' @param data a data frame
#'
#' @return a data frame
lag_by_trigger<-function(data){
  Lag_by_Trigger=data%>%
    select(C_Biosense_Facility_ID, C_BioSense_ID, Arrived_Date_Time, C_Visit_Date_Time, Message_Date_Time,
           Recorded_Date_Time, Trigger_Event)%>%
    mutate(Visit=as.POSIXct(C_Visit_Date_Time,format="%Y-%m-%d %H:%M:%S"),
           Record=as.POSIXct(Recorded_Date_Time,format="%Y-%m-%d %H:%M:%S"),
           Message=as.POSIXct(Message_Date_Time, format="%Y-%m-%d %H:%M:%S"),
           Arrived=as.POSIXct(Arrived_Date_Time, format="%Y-%m-%d %H:%M:%S"),
           lag_Record_Visit=as.numeric(difftime(Record,Visit,units="hours")),
           lag_Message_Record=as.numeric(difftime(Message,Record,units="hours")),
           lag_Arrival_Message=as.numeric(difftime(Arrived,Message,units="hours")),
           lag_Arrival_Visit=as.numeric(difftime(Arrived,Visit,units="hours"))    
    )%>%
    group_by(Trigger_Event)%>%
    summarise(Record_Visit=round(mean(lag_Record_Visit,na.rm=TRUE),2),
              Message_Record=round(mean(lag_Message_Record,na.rm=TRUE),2),
              Arrival_Message=round(mean(lag_Arrival_Message,na.rm=TRUE),2),
              Arrival_Visit=round(mean(lag_Arrival_Visit,na.rm=TRUE),2)
    )
  return(
    Lag_by_Trigger
  )
}
