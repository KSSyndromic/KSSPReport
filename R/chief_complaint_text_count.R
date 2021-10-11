## Show each unique Chief_Complaint_Text for each visit (C_BioSense_ID) and each Medical_Record_Number
## and the frequency for each Chief_Complaint_Text.
chief_complaint_text_count<-function(data){
  chief_complaint_text=data%>%
    select(Chief_Complaint_Text,C_BioSense_ID,Medical_Record_Number)%>%
    filter(is.na(Chief_Complaint_Text)==FALSE)%>%
    arrange(Chief_Complaint_Text)%>%
    mutate(Chief_Complaint_Text=as.character(Chief_Complaint_Text))%>%
    distinct()%>%
    right_join(data%>%
                 select(Chief_Complaint_Text,C_BioSense_ID,Medical_Record_Number)%>%
                 filter(is.na(Chief_Complaint_Text)==FALSE)%>%
                 arrange(Chief_Complaint_Text)%>%
                 distinct()%>%
                 count(Chief_Complaint_Text)%>%
                 mutate(Chief_Complaint_Text=as.character(Chief_Complaint_Text)),.,by="Chief_Complaint_Text")%>%
    mutate(Field="Chief_Complaint_Text",Freq=n,Content=Chief_Complaint_Text)%>%
    select(Field,Content,Freq,C_BioSense_ID,Medical_Record_Number)
  
  
  return(chief_complaint_text)
}
