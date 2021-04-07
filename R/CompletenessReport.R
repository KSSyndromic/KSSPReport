#' Write NSSP BioSense Platform Data Quality Summary Reports all facilities and select variables.
#'
#' @description
#' This function finds the average word counts and completeness for select variables.
#'
#' @param username Your BioSense username, as a string. This is the same username you may use to log into RStudio or Adminer.
#' @param password Your BioSense password, as a string. This is the same password you may use to log into RStudio or Adminer.
#' @param filename The name that you wish the final .csv to be .
#' @param start The start date time that you wish to begin pulling data from, as a string.
#' @param end The end data time that you wish to stop pulling data from, as a string.
#' @import dplyr
#' @import scales
#' @import RODBC
#' @import stringr 
#' @import ngram
#' 
#' @export
#' 
#' 

Completeness <- function(username, password, filename, start, end) {
  channel <- odbcConnect("BioSense_Platform", paste0("BIOSENSE\\", username), password)
  data.names <- sqlQuery(channel, "SELECT Facility_Name, C_Biosense_Facility_ID FROM KS_MFT")
  data <- sqlQuery(channel, paste0("SELECT C_Biosense_ID, C_Biosense_Facility_ID, Chief_Complaint_Text, Chief_Complaint_Combo, Diagnosis_Combo, Admit_Reason_Combo, Procedure_Code, Triage_Notes FROM KS_PR_PROCESSED  WHERE C_Visit_Date_Time Between  '", start,"' AND  '", end, "'"), as.is=TRUE)
  odbcCloseAll()
  ## Removing duplicate C_Biosense_ID
  data <- data[!duplicated(data[ , c("C_Biosense_ID")]), ]
  
  
  ##Get Word Counts for Each Column
  ChiefComplaintTextAvgWC <- ifelse(is.na(data$Chief_Complaint_Text) == 'TRUE', 0, lengths(gregexpr("[[:>:]]",data$Chief_Complaint_Text,perl=TRUE)))
  ChiefComplaintComboAvgWC <- ifelse(is.na(data$Chief_Complaint_Combo) == 'TRUE', 0, lengths(gregexpr("[[:>:]]",data$Chief_Complaint_Combo,perl=TRUE)))
  DiagnosisComboAvgWC <- ifelse(is.na(data$Diagnosis_Combo) == 'TRUE', 0, lengths(gregexpr("[[:>:]]",data$Diagnosis_Combo,perl=TRUE)))
  AdmitReasonComboAvgWC <- ifelse(is.na(data$Admit_Reason_Combo) == 'TRUE', 0, lengths(gregexpr("[[:>:]]",data$Admit_Reason_Combo,perl=TRUE)))
  ProcedureCodeAvgWC <- ifelse(is.na(data$Procedure_Code) == 'TRUE', 0, lengths(gregexpr("[[:>:]]",data$Procedure_Code,perl=TRUE)))
  TriageNotesAvgWC <- ifelse(is.na(data$Triage_Notes) == 'TRUE', 0, lengths(gregexpr("[[:>:]]",data$Triage_Notes,perl=TRUE)))
  
  ##Get Percentage Complete
  PercentCompleteChiefComplaintText <- ifelse(is.na(data$Chief_Complaint_Text) == 'TRUE', 0, 1)
  PercentCompleteChiefComplaintCombo <- ifelse(is.na(data$Chief_Complaint_Combo) == 'TRUE', 0, 1)
  PercentCompleteDiagnosisCombo <- ifelse(is.na(data$Diagnosis_Combo) == 'TRUE', 0, 1)
  PercentCompleteAdmitReasonCombo <- ifelse(is.na(data$Admit_Reason_Combo) == 'TRUE', 0, 1)
  PercentCompleteProcedureCode <- ifelse(is.na(data$Procedure_Code) == 'TRUE', 0, 1)
  PercentCompleteTriageNotes <- ifelse(is.na(data$Triage_Notes) == 'TRUE', 0, 1)
  
  my.data <- data.frame(data$C_Biosense_Facility_ID, ChiefComplaintTextAvgWC, ChiefComplaintComboAvgWC, DiagnosisComboAvgWC, AdmitReasonComboAvgWC, ProcedureCodeAvgWC, TriageNotesAvgWC)
  Percentmy.data <- data.frame(data$C_Biosense_Facility_ID, PercentCompleteChiefComplaintText, PercentCompleteChiefComplaintCombo, PercentCompleteDiagnosisCombo, PercentCompleteAdmitReasonCombo, PercentCompleteProcedureCode, PercentCompleteTriageNotes)
  
  
  #Find the Mean of Each Variable by the Facility ID number
  new.data <- aggregate(.~data.C_Biosense_Facility_ID, data=my.data, FUN=function(my.data) c(mean=mean(my.data)) )
  
  #Find Percent Complete
  Percent.data <- aggregate(.~data.C_Biosense_Facility_ID, data=Percentmy.data, FUN=function(my.data) c(mean=mean(my.data)) )
  
  ##Rounding
  Percent.data <- Percent.data %>% mutate_if(is.numeric, round, digits = 3)
  
  ##Making Percentages look nicer
  Percent.data$PercentCompleteChiefComplaintText <- label_percent()(Percent.data$PercentCompleteChiefComplaintText)
  Percent.data$PercentCompleteChiefComplaintCombo <- label_percent()(Percent.data$PercentCompleteChiefComplaintCombo)
  Percent.data$PercentCompleteDiagnosisCombo <- label_percent()(Percent.data$PercentCompleteDiagnosisCombo)
  Percent.data$PercentCompleteAdmitReasonCombo <- label_percent()(Percent.data$PercentCompleteAdmitReasonCombo)
  Percent.data$PercentCompleteProcedureCode <- label_percent()(Percent.data$PercentCompleteProcedureCode)
  Percent.data$PercentCompleteTriageNotes <- label_percent()(Percent.data$PercentCompleteTriageNotes)
  
  ## Finding Number of data points
  N <- aggregate(ChiefComplaintTextAvgWC~data.C_Biosense_Facility_ID, data=my.data, FUN=function(my.data){NROW(my.data)} )
  colnames(N)[2] <- "n"
  
  ##Combining all the data frames
  final.data <- merge(new.data, N, by.x="data.C_Biosense_Facility_ID", by.y="data.C_Biosense_Facility_ID", all.x=TRUE)
  final.data <- merge(final.data, data.names, by.x="data.C_Biosense_Facility_ID", by.y="C_Biosense_Facility_ID", all.x=TRUE)
  final.data <- merge(final.data, Percent.data, by.x="data.C_Biosense_Facility_ID", by.y="data.C_Biosense_Facility_ID", all.x=TRUE)
  
  
  ##Dropping Facility ID
  final.data <- subset(final.data, select = -c(data.C_Biosense_Facility_ID))
  
  ##Reorganizing
  final.data <- final.data[c("Facility_Name", "ChiefComplaintTextAvgWC", "PercentCompleteChiefComplaintText", "ChiefComplaintComboAvgWC", "PercentCompleteChiefComplaintCombo", "DiagnosisComboAvgWC", "PercentCompleteDiagnosisCombo", "AdmitReasonComboAvgWC", "PercentCompleteAdmitReasonCombo", "ProcedureCodeAvgWC", "PercentCompleteProcedureCode", "TriageNotesAvgWC", "PercentCompleteTriageNotes", "n")]
  
  ##Rounding
  final.data <- final.data %>% mutate_if(is.numeric, round, digits = 2)
  
  ##Alphabetical Order
  final.data <- final.data[order(final.data$Facility_Name),]
  
  ##Print to .csv file
  write.csv(final.data, file = paste0(filename, ".csv"), row.names = TRUE)
}


