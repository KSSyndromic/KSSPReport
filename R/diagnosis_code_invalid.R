## check the validity of diagnosis code; valid diagnosis code should contain letters
## if the diagnosis code is non NULL, and there are only numbers (no letters), it is defined as invalid
diagnosis_code_invalid<-function(data){
  snomed<-data%>%
    select(C_Biosense_Facility_ID, C_BioSense_ID,Diagnosis_Code)
  ## remove the semicolon ";"
  snomed$Diagnosis_Code_nosemi<-gsub(";","",snomed$Diagnosis_Code)
  ## pattern match: if any letters in diagnosis code, then TRUE
  snomed$letters<-grepl("[a-z]",snomed$Diagnosis_Code_nosemi,ignore.case = TRUE)
  ## compute frequency and percentage of invalid diagnosis code
  diagnosis_summary=snomed%>%
    filter(is.na(Diagnosis_Code_nosemi)==FALSE)%>%
    group_by(C_Biosense_Facility_ID) %>%
    summarise(Diagnosis_Code.Percent=round(mean(letters==FALSE)*100,2),
              Diagnosis_Code.Count=sum(letters==FALSE))
  
  diagnosis_examples=snomed%>%
    filter(is.na(Diagnosis_Code_nosemi)==FALSE)
  
  return(
    list(diagnosis_examples=diagnosis_examples,
         diagnosis_summary=diagnosis_summary)
    )
}
