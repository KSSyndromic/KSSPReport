## Show each unique Clinical_Impression for each visit (C_BioSense_ID) and each Medical_Record_Number
## and the frequency for each Clinical_Impression.
clinical_impression_count<-function(data){
  clinical_impression=data%>%
    select(Clinical_Impression,C_BioSense_ID,Medical_Record_Number)%>%
    filter(is.na(Clinical_Impression)==FALSE)%>%
    arrange(Clinical_Impression)%>%
    mutate(Clinical_Impression=as.character(Clinical_Impression))%>%
    distinct()%>%
    right_join(data%>%
                 select(Clinical_Impression,C_BioSense_ID,Medical_Record_Number)%>%
                 filter(is.na(Clinical_Impression)==FALSE)%>%
                 arrange(Clinical_Impression)%>%
                 distinct()%>%
                 count(Clinical_Impression)%>%
                 mutate(Clinical_Impression=as.character(Clinical_Impression)),.,by="Clinical_Impression")%>%
    mutate(Field="Clinical_Impression",Freq=n,Content=Clinical_Impression)%>%
    select(Field,Content,Freq,C_BioSense_ID,Medical_Record_Number)
  
  return(clinical_impression)
}
