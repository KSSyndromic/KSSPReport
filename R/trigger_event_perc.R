##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for.

##Trigger_Event

#' Frequency of occurrence in data
#'
#' @param data a data frame
#'
#' @return a data frame
trigger_event_perc<-function(data){
  number_visits=n_groups(group_by(data,C_BioSense_ID))
  
  Trigger_Event=data%>%
    select(C_BioSense_ID, Trigger_Event)%>%
    distinct(C_BioSense_ID,Trigger_Event,.keep_all=TRUE)%>%
    count(Trigger_Event)%>%
    transmute(Trigger_Event,count=n,percentage=round(100*n/number_visits,2))
  return(
    Trigger_Event
  )
}
