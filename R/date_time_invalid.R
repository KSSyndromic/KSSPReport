## check the invalidness of the format of all _data_time related varialbes.
## The valid data_time format should be YYYY-mm-dd HH:MM:SS. IsDate function will check if a certain input satisty this format.
## check_** are categorical variables with 3 levels: TRUE if it is the valid format of YYYY-mm-dd HH:MM:SS, "Missing" if it is empty/NULL,
## and FALSE if it is non-empty but not the valid format of YYYY-mm-dd HH:MM:SS.
## The output of date_time_invalid() function is similar as other _invalid functions: a list of two (examples and summary)

date_time_invalid<-function(data){
  IsDate<-function(mydate,data.format="%Y-%m-%d %H:%M:%S"){
    tryCatch(!is.na(as.Date(mydate,data.format)),
             error=function(err){FALSE})
  }
  
  check_date_examples=data%>%
    select(C_Biosense_Facility_ID,C_BioSense_ID,Recorded_Date_Time,C_Visit_Date_Time,
           Admit_Date_Time, Birth_Date_Time, Create_Processed_Date_Time, Create_Raw_Date_Time,
           Death_Date_Time, Diagnosis_Date_Time, Discharge_Date_Time, Message_Date_Time,
           Observation_Date_Time, Procedure_Date_Time, Update_Processed_Date_Time)%>%
    mutate(check_record=IsDate(Recorded_Date_Time),check_record=ifelse(is.na(Recorded_Date_Time),"Missing",check_record),
           check_visit=IsDate(C_Visit_Date_Time),check_visit=ifelse(is.na(C_Visit_Date_Time),"Missing",check_visit),
           check_admit=IsDate(Admit_Date_Time),check_admit=ifelse(is.na(Admit_Date_Time),"Missing",check_admit),
           check_birth=IsDate(Birth_Date_Time),check_birth=ifelse(is.na(Birth_Date_Time),"Missing",check_birth),
           check_create_processed=IsDate(Create_Processed_Date_Time),check_create_processed=ifelse(is.na(Create_Processed_Date_Time),"Missing",check_create_processed),
           check_create_raw=IsDate(Create_Raw_Date_Time),check_create_raw=ifelse(is.na(Create_Raw_Date_Time),"Missing",check_create_raw),
           check_death=IsDate(Death_Date_Time),check_death=ifelse(is.na(Death_Date_Time),"Missing",check_death),
           check_diagnosis=IsDate(Diagnosis_Date_Time),check_diagnosis=ifelse(is.na(Diagnosis_Date_Time),"Missing",check_diagnosis),
           check_discharge=IsDate(Discharge_Date_Time),check_discharge=ifelse(is.na(Discharge_Date_Time),"Missing",check_discharge),
           check_message=IsDate(Message_Date_Time),check_message=ifelse(is.na(Message_Date_Time),"Missing",check_message),
           check_observation=IsDate(Observation_Date_Time),check_observation=ifelse(is.na(Observation_Date_Time),"Missing",check_observation),
           check_procedure=IsDate(Procedure_Date_Time),check_procedure=ifelse(is.na(Procedure_Date_Time),"Missing",check_procedure),
           check_update_processed=IsDate(Update_Processed_Date_Time),check_update_processed=ifelse(is.na(Update_Processed_Date_Time),"Missing",check_update_processed))
  
  check_date_summary=check_date_examples%>%
    group_by(C_Biosense_Facility_ID)%>%
    summarise(Recorded_Date_Time.Count=sum(check_record==FALSE),
              Recorded_Date_Time.Percent=round(mean(check_record==FALSE)*100,2),
              C_Visit_Date_Time.Count=sum(check_visit==FALSE),
              C_Visit_Date_Time.Percent=round(mean(check_visit==FALSE)*100,2),
              Admit_Date_Time.Count=sum(check_admit==FALSE),
              Admit_Date_Time.Percent=round(mean(check_admit==FALSE)*100,2),
              Birth_Date_Time.Count=sum(check_birth==FALSE),
              Birth_Date_Time.Percent=round(mean(check_birth==FALSE)*100,2),
              Create_Processed_Date_Time.Count=sum(check_create_processed==FALSE),
              Create_Processed_Date_Time.Percent=round(mean(check_create_processed==FALSE)*100,2),
              Create_Raw_Date_Time.Count=sum(check_create_raw==FALSE),
              Create_Raw_Date_Time.Percent=round(mean(check_create_raw==FALSE)*100,2),
              Death_Date_Time.Count=sum(check_death==FALSE),
              Death_Date_Time.Percent=round(mean(check_death==FALSE)*100,2),
              Diagnosis_Date_Time.Count=sum(check_diagnosis==FALSE),
              Diagnosis_Date_Time.Percent=round(mean(check_diagnosis==FALSE)*100,2),
              Discharge_Date_Time.Count=sum(check_discharge==FALSE),
              Discharge_Date_Time.Percent=round(mean(check_discharge==FALSE)*100,2),
              Message_Date_Time.Count=sum(check_message==FALSE),
              Message_Date_Time.Percent=round(mean(check_message==FALSE)*100,2),
              Observation_Date_Time.Count=sum(check_observation==FALSE),
              Observation_Date_Time.Percent=round(mean(check_observation==FALSE)*100,2),
              Procedure_Date_Time.Count=sum(check_procedure==FALSE),
              Procedure_Date_Time.Percent=round(mean(check_procedure==FALSE)*100,2),
              Update_Processed_Date_Time.Count=sum(check_update_processed==FALSE),
              Update_Processed_Date_Time.Percent=round(mean(check_update_processed==FALSE)*100,2))
  
  
  return(
    list(check_date_examples=check_date_examples,
         check_date_summary=check_date_summary))
  
}
