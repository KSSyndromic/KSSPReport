## Show each unique Triage_Notes for each visit (C_BioSense_ID) and each Medical_Record_Number
## and the frequency for each Triage_Notes.
#' Show each unique Triage_Notes for each visit
#'
#' @param data a data frame
#'
#' @return a data frame
triage_notes_count<-function(data){
  triage_notes=data%>%
    select(Triage_Notes,C_BioSense_ID,Medical_Record_Number)%>%
    filter(is.na(Triage_Notes)==FALSE)%>%
    arrange(Triage_Notes)%>%
    mutate(Trige_Notes=as.character(Triage_Notes))%>%
    distinct()%>%
    right_join(data%>%
                 select(Triage_Notes,C_BioSense_ID,Medical_Record_Number)%>%
                 filter(is.na(Triage_Notes)==FALSE)%>%
                 arrange(Triage_Notes)%>%
                 distinct()%>%
                 count(Triage_Notes)%>%
                 mutate(Trige_Notes=as.character(Triage_Notes)),.,by="Triage_Notes")%>%
    mutate(Field="Triage_Notes",Freq=n,Content=Triage_Notes)%>%
    select(Field,Content,Freq,C_BioSense_ID,Medical_Record_Number)
  return(triage_notes)
}
