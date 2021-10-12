## Show each unique Admit_Reason_Description for each visit (C_BioSense_ID) and each Medical_Record_Number
## and the frequency for each Admit_Reason_Description
admit_reason_description_count<-function(data){
  admit_reason_description=data%>%
    select(Admit_Reason_Description,C_BioSense_ID,Medical_Record_Number)%>%
    filter(is.na(Admit_Reason_Description)==FALSE)%>%
    arrange(Admit_Reason_Description)%>%
    mutate(Admit_Reason_Description=as.character(Admit_Reason_Description))%>%
    distinct()%>%
    right_join(data%>%
                 select(Admit_Reason_Description,C_BioSense_ID,Medical_Record_Number)%>%
                 filter(is.na(Admit_Reason_Description)==FALSE)%>%
                 arrange(Admit_Reason_Description)%>%
                 distinct()%>%
                 count(Admit_Reason_Description)%>%
                 mutate(Admit_Reason_Description=as.character(Admit_Reason_Description)),.,by="Admit_Reason_Description")%>%
    mutate(Field="Admit_Reason_Description",Freq=n,Content=Admit_Reason_Description)%>%
    select(Field,Content,Freq,C_BioSense_ID,Medical_Record_Number)
    
    return(admit_reason_description)
}
