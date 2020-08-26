## compute the average number of messages (with same File_Name) per batch for each Feed_Name

mean_message_per_batch<-function(data){
  Message_Per_Batch=data%>%
    group_by(Feed_Name,File_Name)%>%
    summarise(count=n())%>%
    ungroup()%>%
    group_by(Feed_Name)%>%
    summarise(Batch_Mean=round(mean(count),2))
  
  return(Message_Per_Batch)
}
