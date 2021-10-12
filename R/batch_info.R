
#' compute the number of batches per day and the average time between batches for each facility
#'
#' @param data a data frame 
#'
#' @return a data frame
batch_info<-function(data){
  ## compute the number of batches per day for each Feed_Name
  Batch_Per_Day=data%>%
    group_by(Feed_Name,Arrived_Date)%>%
    summarise(N_Batch=n_distinct(File_Name))
  ## compute the time between batches in hours
  Time_Bet_Batch=data%>%
    group_by(Feed_Name,File_Name)%>%
    select(Feed_Name,File_Name, Arrived_Date_Time,Arrived_Date)%>%
    mutate(Arrived_Date_Time=as.POSIXct(Arrived_Date_Time,format="%Y-%m-%d %H:%M:%S"))%>%
    slice(which.min(Arrived_Date_Time))%>%
    group_by(Feed_Name,Arrived_Date)%>%
    arrange(Arrived_Date)%>%
    summarise(Time_Bet_Batch_Hours=round(as.numeric(difftime(max(Arrived_Date_Time),min(Arrived_Date_Time),units="hours"))/(n()-1),2))
  
  
  return(Batch_Per_Day%>%
    left_join(., Time_Bet_Batch, by = c("Feed_Name", "Arrived_Date"))
  )
}

