lag_graph<-function(data,start,end){
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
  
  Graph_data=Time_Diff %>%group_by(week = cut(Visit,'week')) %>% summarise(Arrival_Visit=mean(lag_Arrival_Visit))
  
  title=paste("Weekly Average Hours Delayed (Arrival_Visit) from",format(as.Date(start), "%b%d,%Y"), 'to', format(as.Date(start), "%b%d,%Y"))

 Graph1=ggplot(data=Graph_data, aes(x=as.Date(week), y=Arrival_Visit,group = 1, color = "Weekly Average Hours Delayed")) + geom_point()+geom_line()+theme_classic()+xlab("Week")+geom_hline( linetype="dashed", aes(yintercept=24,color = "24-hours (Recommended)"))+
   scale_colour_manual(values = c("red", "black"))+ylab('Average Hours Delayed')+ggtitle(title)+expand_limits(y=0)
 return(Graph1)
  
  
}