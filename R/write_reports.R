#' Write NSSP BioSense Platform Data Quality Summary Reports
#' 
#' @description 
#' This function calls upon all of the other functions in the package to write a large number of Excel workbooks. First,
#' it generates a state summary workbook that shows percents and counts of nulls and invalids at both the facility and statewide level,
#' as well as message delivery lag (i.e., timeliness reports). Second, it generates a summary workbook for every single facility
#' that includes only information for that facility. Third, it generates an example workbook for every single facility, including 
#' detailed information on records and visits that are null or invalid. These example workbooks make the function longer to run,
#' so by default this function does not generate them (see `nexamples` input below).
#' 
#' @param username Your BioSense username, as a string. This is the same username you may use to log into RStudio or Adminer.
#' @param password Your BioSense password, as a string. This is the same password you may use to log into RStudio or Adminer.
#' @param table The table that you want to retrieve the data from, as a string.
#' @param mft The MFT (master facilities table) from where the facility names will be retrieved, as a string.
#' @param start The start date time that you wish to begin pulling data from, as a string.
#' @param end The end data time that you wish to stop pulling data from, as a string.
#' @param directory The directory where you would like to write the reports to (i.e., "~/Documents/MyReports"), as a string.
#' @param nexamples An integer number of examples you would like for each type of invalid or null field in the examples workbooks for each facility.
#' This defaults to 0, which will not generate these example workbooks.
#' @param offset The number of hours you wish to offset Arrived_Date_Time (which is in UTC). The offset value is how far off your local time zone is from UTC. 
#' For example, the Central Time Zone would set this to 5 or 6, depending on if it is daylight savings or not. This value should be an integer. 
#' This is used for timeliness reports using the `va_lag` function.
#' @import dplyr
#' @import tidyr
#' @import openxlsx
#' @importFrom stringr str_replace_all
#' @export
write_reports <- function(username, password, table, mft,raw, start, end, directory="", nexamples=0, offset) {
  
  ## get data and names
  pull <- pull_data(username, password, table, mft, raw, start, end)
  # save data into data
  data <- pull$data
  if (nrow(data) == 0) stop("The query yielded no data.")
  # save names into names, getting rid of any duplicate names (take first listed)
  fnames <- pull$names %>% group_by(C_Biosense_Facility_ID) %>% slice(1) %>% ungroup()
  
  batchdata<-pull$batchdata
  
  
  ## state-wide summary
  # get facility-level state summary of required nulls
  state_req_nulls <- get_req_nulls(data)
  # get facility-level state summary of optional nulls
  state_opt_nulls <- get_opt_nulls(data)
  # get facility-level state summary of invalids
  state_invalids <- get_all_invalids(data)
  ## get state-wide average lag, remove the column of Facility_ID
  state_lag <-c((apply(va_lag(data)[,-1],2,function(s)round(mean(s),2))))
  ## get state-wide average earliest lag, remove the column of Facility_ID
  state_early_lag<-c((apply(early_lag(data)[,-1],2,function(s)round(mean(s),2))))
  ## get state-wide average earliest Non NA chief_complaint lag, remove the column of Facility_ID                          
  state_chief_complaint<-c((apply(lag_chief_complaint(data)[,-1],2,function(s)round(mean(s),2))))
  ## get state-wide average earliest Non NA diagnosis lag, remove the column of Facility_ID
  state_diagnosis<-c((apply(lag_diagnosis(data)[,-1],2,function(s)round(mean(s),2))))                         
  # overall , state-level average
  statewides <- statewide(data, state_req_nulls, state_opt_nulls, state_invalids)
  
                            
  # writing xlsx
  wb <- createWorkbook() # create workbook
  hs <- createStyle(fgFill="#4f81bd", halign="left", valign="top", textDecoration="bold", wrapText=TRUE)
  # sheet 1: required nulls
  sheet1 <- addWorksheet(wb, "Required Nulls")
  # putting statewide above the filter
  writeData(wb, sheet1, statewides$statewide_reqnull,
            startCol=4, startRow=1, colNames=FALSE)
  # writing data table below
  writeDataTable(wb, sheet1,
                 state_req_nulls %>% 
                   right_join(fnames, ., by = "C_Biosense_Facility_ID"),
                 startCol=1, startRow=3,headerStyle=hs, bandedRows=TRUE)
  # formatting widths, freeze panes, and color
  setColWidths(wb, sheet1, 1:ncol(right_join(fnames, state_req_nulls, by = "C_Biosense_Facility_ID")), "auto")
  freezePane(wb, sheet1, firstActiveRow=4, firstActiveCol=6)
  addStyle(wb, sheet1, createStyle(fgFill="#4f81bd", fontColour="#ffffff", textDecoration = "bold"),
           rows=1:3, cols=1:ncol(right_join(fnames, state_req_nulls, by = "C_Biosense_Facility_ID")), gridExpand=TRUE)
  # sheet 2: optional nulls
  sheet2 <- addWorksheet(wb, "Optional Nulls")
  # putting statewide above the filter
  writeData(wb, sheet2, statewides$statewide_optnull, 
            startCol=4, startRow=1, colNames=FALSE)
  # writing data table below
  writeDataTable(wb, sheet2,
                 state_opt_nulls %>% 
                   right_join(fnames, ., by = "C_Biosense_Facility_ID"),
                 startCol=1, startRow=3,headerStyle=hs, bandedRows=TRUE)
  # formatting widths, freeze panes, and color
  setColWidths(wb, sheet2, 1:ncol(right_join(fnames, state_opt_nulls, by = "C_Biosense_Facility_ID")), "auto")
  freezePane(wb, sheet2, firstActiveRow=4, firstActiveCol=6)
  addStyle(wb, sheet2, createStyle(fgFill="#4f81bd", fontColour="#ffffff", textDecoration = "bold"),
           rows=1:3, cols=1:ncol(right_join(fnames, state_opt_nulls, by = "C_Biosense_Facility_ID")), gridExpand=TRUE)
  # sheet 3: invalids
  sheet3 <- addWorksheet(wb, "Invalids")
  # putting statewide above the filter
  writeData(wb, sheet3, statewides$statewide_invalids, 
            startCol=4, startRow=1, colNames=FALSE)
  # writing data table below
  writeDataTable(wb, sheet3,
                 state_invalids %>% 
                   right_join(fnames, ., by = "C_Biosense_Facility_ID"),
                 startCol=1, startRow=3,headerStyle=hs, bandedRows=TRUE)
  # formatting widths, freeze panes, and color
  setColWidths(wb, sheet3, 1:ncol(right_join(fnames, state_invalids, by = "C_Biosense_Facility_ID")), "auto")
  freezePane(wb, sheet3, firstActiveRow=4, firstActiveCol=6)
  addStyle(wb, sheet3, createStyle(fgFill="#4f81bd", fontColour="#ffffff", textDecoration = "bold"),
           rows=1:3, cols=1:ncol(right_join(fnames, state_invalids, by = "C_Biosense_Facility_ID")), gridExpand=TRUE)

  
  # write workbook
  saveWorkbook(wb, paste0(directory, "/State_Summary.xlsx"), overwrite=TRUE)




  ## facility by facility summary
  for (i in data$C_Biosense_Facility_ID[!duplicated(data$C_Biosense_Facility_ID)]) { # for every unique facility id
    # getting first and last visit date times
    vmin <- min(as.character(filter(data, C_Biosense_Facility_ID==i)$C_Visit_Date_Time))
    vmax <- max(as.character(filter(data, C_Biosense_Facility_ID==i)$C_Visit_Date_Time))
    amin <- min(as.character(filter(data, C_Biosense_Facility_ID==i)$Arrived_Date_Time))
    amax <- max(as.character(filter(data, C_Biosense_Facility_ID==i)$Arrived_Date_Time))
    # get hl7 values
    data("hl7_values", envir=environment())
    hl7_values$Field <- as.character(hl7_values$Field)
    # get name of facility
    fname <- as.character(unlist(unname(c(fnames[which(fnames$C_Biosense_Facility_ID==i),1]))))

    # write to xlsx
    # initialize workbook
    wb <- createWorkbook()
    # sheet 1: facility information
    sheet1 <- addWorksheet(wb, "Facility Information")
    subdata=data%>%
            filter(C_Biosense_Facility_ID==i)
    visits_per_day=avg_visit_per_day(subdata)
    visit_length=avg_visit_length(subdata)$Visit_Length
    
    facility_table=suppressWarnings(data %>% # take data
                     select(c(C_Biosense_Facility_ID, Sending_Facility_ID, Sending_Application, 
                              Treating_Facility_ID, Receiving_Application, Receiving_Facility)) %>% # taking only variables we want
                     filter(C_Biosense_Facility_ID==i) %>% # taking only rows with the same facility ID
                     gather(key=Field, value=Value, convert=TRUE) %>% # suppressed warnings because this will tell you it converted all to characters
                     distinct() %>% # get only distinct entries
                     bind_rows(data.frame(Field="Facility_Name", Value=fname), .) %>% # add name to the top
                     # bind with date ranges and number of records and visits
                     bind_rows(data.frame(Field=c("Patient_Visit_Dates", "Message_Arrival_Dates", 
                                                  "Number of Records", "Number of Visits",
                                                 "Average Number of Visits per Day","Average Visit Length in Hours"),
                                          Value=c(paste("From", vmin, "to", vmax),
                                                  paste("From", amin, "to", amax),
                                                  nrow(filter(data, C_Biosense_Facility_ID==i)), 
                                                  n_groups(group_by(filter(data, C_Biosense_Facility_ID==i), C_BioSense_ID)),
                                                  visits_per_day,
                                                  visit_length
                                                  ))) %>% 
                     right_join(hl7_values, ., by="Field"))
    
    ## Record: the time that the first message is entered into system; Message: the time that the first message was sent;
    ## Arrival: the time that the first message arraived at CDC; Visit: patient visit time
    
    ## compute the average time difference between Record and Visit, Message and Record, Arrival and Message, Arrival and Visit
    ## for each visit.
    Lag<-data.frame(
      HL7=c("EVN-2.1","MSH-7.1,EVN-2.1","MSH-7.1",""),
       Lag_Name=c("Record_Visit","Message_Record","Arrival_Message","Arrival_Visit"),
      Lag_Between=c("Record_Date_Time-Visit_Date_Time","Message_Date_Time-Record_Date_Time","Arrival_Date_Time-Message_Date_Time","Arrival_Date_Time-Visit_Date_Time"),
      Average_Lag_hours=t(va_lag(subdata)[-1]),
      State_wide_Average= state_lag
      )
    
    ##Time difference for the earliest Recorded_Date_Time
    Early_Lag<-data.frame(
      HL7=c("EVN-2.1","MSH-7.1,EVN-2.1","MSH-7.1",""),
       Lag_Name=c("Record_Visit","Message_Record","Arrival_Message","Arrival_Visit"),
      Lag_Between=c("Record_Date_Time-Visit_Date_Time","Message_Date_Time-Record_Date_Time","Arrival_Date_Time-Message_Date_Time","Arrival_Date_Time-Visit_Date_Time"),
      Early_Lag_hours= t(early_lag(subdata)[-1]),
      State_wide_Average=state_early_lag
      )
    
    ## Time diference for the earliest recorded non NA C_Chief_Complaints
    Chief_Complaint<-data.frame(
      HL7=c("EVN-2.1","MSH-7.1,EVN-2.1","MSH-7.1",""),
      Lag_Name=c("Record_Visit","Message_Record","Arrival_Message","Arrival_Visit"),
      Lag_Between=c("Record_Date_Time-Visit_Date_Time","Message_Date_Time-Record_Date_Time","Arrival_Date_Time-Message_Date_Time","Arrival_Date_Time-Visit_Date_Time"),
      Earliest_Non_NA_Chief_Complaint_Lag=t(lag_chief_complaint(subdata)[-1]),
      State_wide_Average= state_chief_complaint
      )
    ## Time difference for the earliest recorded non NA Diagnosis_Code
     Diagnosis<-data.frame(
      HL7=c("EVN-2.1","MSH-7.1,EVN-2.1","MSH-7.1",""),
       Lag_Name=c("Record_Visit","Message_Record","Arrival_Message","Arrival_Visit"),
      Lag_Between=c("Record_Date_Time-Visit_Date_Time","Message_Date_Time-Record_Date_Time","Arrival_Date_Time-Message_Date_Time","Arrival_Date_Time-Visit_Date_Time"),
      Earliest_Non_NA_Diagnosis_Code_Lag=t(lag_diagnosis(subdata)[-1]),
      State_wide_Average= state_diagnosis
      )
     ## Time difference for various Trigger_Event
    Trigger<-data.frame(
      HL7=c("EVN-2.1","MSH-7.1,EVN-2.1","MSH-7.1",""),
       Lag_Name=c("Record_Visit","Message_Record","Arrival_Message","Arrival_Visit"),
      Lag_Between=c("Record_Date_Time-Visit_Date_Time","Message_Date_Time-Record_Date_Time","Arrival_Date_Time-Message_Date_Time","Arrival_Date_Time-Visit_Date_Time"),
      A01_Trigger_Event=t(lag_by_trigger(subdata)[lag_by_trigger(subdata)$Trigger_Event=="A01",][-1]),
      A03_Trigger_Event=t(lag_by_trigger(subdata)[lag_by_trigger(subdata)$Trigger_Event=="A03",][-1]),
      A04_Trigger_Event=t(lag_by_trigger(subdata)[lag_by_trigger(subdata)$Trigger_Event=="A04",][-1]),
      A06_Trigger_Event=t(lag_by_trigger(subdata)[lag_by_trigger(subdata)$Trigger_Event=="A06",][-1]),
      A08_Trigger_Event=t(lag_by_trigger(subdata)[lag_by_trigger(subdata)$Trigger_Event=="A08",][-1])
      )
   
    writeDataTable(wb, sheet1,facility_table,headerStyle=hs,firstColumn=TRUE, bandedRows=TRUE)
    writeDataTable(wb,sheet1,Lag,startCol=1,startRow=nrow(facility_table)+2, headerStyle=hs,colNames=TRUE,rowNames=FALSE,firstColumn=TRUE)
    writeDataTable(wb,sheet1,Early_Lag,startCol=1,startRow=nrow(facility_table)+7,headerStyle=hs, colNames=TRUE,rowNames=FALSE,firstColumn=TRUE)
    writeDataTable(wb,sheet1,Chief_Complaint,startCol=1,startRow=nrow(facility_table)+12,headerStyle=hs, colNames=TRUE,rowNames=FALSE,firstColumn=TRUE)
    writeDataTable(wb,sheet1,Diagnosis,startCol=1,startRow=nrow(facility_table)+17,headerStyle=hs, colNames=TRUE,rowNames=FALSE,firstColumn=TRUE)
    writeDataTable(wb,sheet1,Trigger,startCol=1,startRow=nrow(facility_table)+22,headerStyle=hs, colNames=TRUE,rowNames=FALSE,firstColumn=TRUE)
      
    setColWidths(wb, sheet1, 1:9, "auto")
    ## sheet 2: required nulls
    sheet2 <- addWorksheet(wb, "Required Nulls") # initialize sheet
     ##making data for it
    facsheet2data <- statewides$statewide_reqnull %>% # take state average
     filter(Measure=="Percent") %>% # only percent
      select(-Location, -Measure) %>% # select vars only needed
      gather(Field, State_Percent, 1:ncol(.)) %>% # put into long format
      left_join(one_facility_summary(state_req_nulls[,-c(2,3)], i), ., by="Field") # join with one facility summary
    writeDataTable(wb, sheet2, facsheet2data, firstColumn=TRUE,headerStyle=hs, bandedRows=TRUE) # write to table
    setColWidths(wb, sheet2, 1:ncol(facsheet2data), "auto") # format sheet
    freezePane(wb, sheet2, firstActiveRow=2) # format sheet
    # sheet 3: optional nulls
    sheet3 <- addWorksheet(wb, "Optional Nulls") # initialize sheet
    # making data for it
    facsheet3data <- statewides$statewide_optnull %>% # take state average
      filter(Measure=="Percent") %>% # only percent
      select(-Location, -Measure) %>% # select vars only needed
      gather(Field, State_Percent, 1:ncol(.)) %>% # put into long format
      left_join(one_facility_summary(state_opt_nulls[,-c(2,3)], i), ., by="Field") # join with one facility summary
    writeDataTable(wb, sheet3, facsheet3data, firstColumn=TRUE,headerStyle=hs, bandedRows=TRUE) # write to table
    setColWidths(wb, sheet3, 1:ncol(facsheet3data), "auto") # format sheet
    freezePane(wb, sheet3, firstActiveRow=2) # format sheet
    # sheet 4: invalids
    sheet4 <- addWorksheet(wb, "Invalids") # initialize sheet
    # making data for it
    facsheet4data <- statewides$statewide_invalids %>% # take state average
      filter(Measure=="Percent") %>% # only percent
      select(-Location, -Measure) %>% # select vars only needed
      gather(Field, State_Percent, 1:ncol(.)) %>% # put into long format
      left_join(one_facility_summary(state_invalids[,-c(2,3)], i), ., by="Field") # join with one facility summary
    writeDataTable(wb, sheet4, facsheet4data, firstColumn=TRUE,headerStyle=hs, bandedRows=TRUE) # write to table
    setColWidths(wb, sheet4, 1:ncol(facsheet4data), "auto") # format sheet
    freezePane(wb, sheet4, firstActiveRow=2) # format sheet

    
    
    ## batch information
    sheet5 <- addWorksheet(wb, "Batch_Information")
    ##compute the average number of messages per batch for each Feed_Name
    Batch_Mean=subdata%>%
    select(Feed_Name,C_Biosense_Facility_ID)%>%
    merge(mean_message_per_batch(batchdata),.,by="Feed_Name")%>%
    distinct()
    
    ## compute the # of batches per day for each Feed_Name/facility
    Batch_Info=batch_info(batchdata)
    Batch_Data=Batch_Info%>%
               inner_join(Batch_Mean,.,by="Feed_Name")%>%
               select(Feed_Name, Arrived_Date, N_Batch, Time_Bet_Batch_Hours)

    writeDataTable(wb, sheet5, Batch_Mean, firstColumn=TRUE, headerStyle=hs,bandedRows=TRUE)
    writeDataTable(wb,sheet5,Batch_Data,startCol=1,startRow=3,headerStyle=hs, colNames=TRUE,rowNames=FALSE,firstColumn=TRUE)
    setColWidths(wb, sheet5, 1:4, "auto")
    
    ## sheet 6
    sheet6 <- addWorksheet(wb, "Race and Ethnicity")
    ## the frequency and percentage of all visits for race and ethnicity
    Race_Description=race_description_perc(subdata)
    Race_Code=race_code_perc(subdata)
    
    Ethnicity_Description=ethnicity_description_perc(subdata)
    Ethnicity_Code=ethnicity_code_perc(subdata)
    
    writeDataTable(wb, sheet6, Race_Description,colNames=TRUE,rowNames=FALSE, firstColumn=TRUE, headerStyle=hs,bandedRows=TRUE)
    writeDataTable(wb,sheet6,Race_Code,startCol=5,startRow=1, colNames=TRUE,rowNames=FALSE,headerStyle=hs,firstColumn=TRUE)
    writeDataTable(wb,sheet6,Ethnicity_Description,startCol=1,startRow=nrow(Race_Code)+3, headerStyle=hs,colNames=TRUE,rowNames=FALSE,firstColumn=TRUE)
    writeDataTable(wb,sheet6,Ethnicity_Code,startCol=5,startRow=nrow(Race_Code)+3,headerStyle=hs, colNames=TRUE,rowNames=FALSE,firstColumn=TRUE)
    setColWidths(wb, sheet6, 1:8, "auto")
    
    ## sheet 7
    sheet7 <- addWorksheet(wb, "Patient Location")
    ## the frequency and percentage of patients' location: country, state, city and county.
    Country=country_perc(subdata)
    State=state_perc(subdata)
    County=county_perc(subdata)
    City=city_perc(subdata)
    
    writeDataTable(wb, sheet7, Country,colNames=TRUE,rowNames=FALSE, firstColumn=TRUE,headerStyle=hs, bandedRows=TRUE)
    writeDataTable(wb, sheet7, State,colNames=TRUE,rowNames=FALSE, firstColumn=TRUE,startRow=nrow(Country)+2,headerStyle=hs, bandedRows=TRUE)
    writeDataTable(wb, sheet7, County,colNames=TRUE,rowNames=FALSE, firstColumn=TRUE,startRow=nrow(Country)+nrow(State)+3, headerStyle=hs,bandedRows=TRUE)
    writeDataTable(wb, sheet7, City,colNames=TRUE,rowNames=FALSE, firstColumn=TRUE,startRow=nrow(Country)+nrow(State)+nrow(County)+4,headerStyle=hs, bandedRows=TRUE)
    setColWidths(wb, sheet7, 1:3, "auto")
    
    ## sheet 8
    sheet8 <- addWorksheet(wb,"Other Patient Information")
    ## the frequency and percentage of insurance company, patient class, age group, and trigger event and if trigger event is A03
    ## also check for smoking code, smoking description, and discharge disposition
    Insurance=insurance_company_id_perc(subdata)
    Patient_Class=patient_class_perc(subdata)
    Age_Group=age_group_perc(subdata)
    Trigger_Event=trigger_event_perc(subdata)
    Trigger_Event_A03=subdata%>%
                     filter(Trigger_Event=="A03")%>%
                     select(C_BioSense_ID,Trigger_Event,Discharge_Date_Time,Discharge_Disposition)%>%
                     mutate(Discharge_Disposition=ifelse(is.na(Discharge_Disposition),"NA",Discharge_Disposition),
                     Discharge_Date_Time=ifelse(is.na(Discharge_Disposition),"NA",Discharge_Date_Time))%>%
                     transmute(Trigger_Event,
                               Discharge_Disposition_Perc=round(100*mean(Discharge_Disposition!="NA"),2),
                               Discharge_Date_Time_Perc=round(100*mean(Discharge_Date_Time!="NA"),2))%>%
                     distinct()

    Smoking_Desc=smoking_status_description_perc(subdata)
    Smoking_Code=smoking_status_code_perc(subdata)
    Discharge_Dis=discharge_disposition_perc(subdata)
    
    writeDataTable(wb, sheet8, Insurance,colNames=TRUE,rowNames=FALSE, firstColumn=TRUE, headerStyle=hs,bandedRows=TRUE)
    writeDataTable(wb, sheet8, Patient_Class,colNames=TRUE,rowNames=FALSE, firstColumn=TRUE,headerStyle=hs,startRow=nrow(Insurance)+2, bandedRows=TRUE)
    writeDataTable(wb, sheet8, Age_Group,colNames=TRUE,rowNames=FALSE, firstColumn=TRUE,headerStyle=hs,startRow=nrow(Insurance)+nrow(Patient_Class)+3, bandedRows=TRUE)
    writeDataTable(wb, sheet8, Trigger_Event,colNames=TRUE,rowNames=FALSE, firstColumn=TRUE,headerStyle=hs,
                   startRow=nrow(Insurance)+nrow(Patient_Class)+nrow(Age_Group)+4, bandedRows=TRUE)
    writeDataTable(wb, sheet8, Trigger_Event_A03,colNames=TRUE,rowNames=FALSE, firstColumn=TRUE,headerStyle=hs,
                   startRow=nrow(Insurance)+nrow(Patient_Class)+nrow(Age_Group)+4, startCol=4,bandedRows=TRUE)
    
    writeDataTable(wb, sheet8, Smoking_Desc,colNames=TRUE,rowNames=FALSE, firstColumn=TRUE,headerStyle=hs,
                   startRow=nrow(Insurance)+nrow(Patient_Class)+nrow(Age_Group)+nrow(Trigger_Event)+5, bandedRows=TRUE)
    writeDataTable(wb, sheet8, Smoking_Code,colNames=TRUE,rowNames=FALSE, firstColumn=TRUE, headerStyle=hs,
                   startRow=nrow(Insurance)+nrow(Patient_Class)+nrow(Age_Group)+nrow(Trigger_Event)+nrow(Smoking_Desc)+6, bandedRows=TRUE)
    writeDataTable(wb, sheet8, Discharge_Dis,colNames=TRUE,rowNames=FALSE, firstColumn=TRUE,headerStyle=hs,
                   startRow=nrow(Insurance)+nrow(Patient_Class)+nrow(Age_Group)+nrow(Trigger_Event)+nrow(Smoking_Desc)+nrow(Smoking_Code)+7, bandedRows=TRUE)
    setColWidths(wb, sheet8, 1:6, "auto")
    
    ## sheet 9
    sheet9 <- addWorksheet(wb,"Facility and Diagnosis")
    ## frequency and percentage of facility description/code, and diagnosis type/code
    Facility_Desc=facility_type_description_perc(subdata)
    Facility_Code=facility_type_code_perc(subdata)
    Diagnosis_Type=diagnosis_type_perc(subdata)
    Diagnosis_Code=diagnosis_code_perc(subdata)
    
    writeDataTable(wb,sheet9,Facility_Desc,colNames=TRUE,rowNames=FALSE, firstColumn=TRUE, headerStyle=hs,bandedRows=TRUE)
    writeDataTable(wb,sheet9,Facility_Code,colNames=TRUE,rowNames=FALSE, firstColumn=TRUE,startCol=5, headerStyle=hs,bandedRows=TRUE)
    writeDataTable(wb,sheet9,Diagnosis_Type,colNames=TRUE,rowNames=FALSE,headerStyle=hs,
                   firstColumn=TRUE,startRow=max(nrow(Facility_Code),nrow(Facility_Desc))+3,bandedRows=TRUE)
    writeDataTable(wb,sheet9,Diagnosis_Code,colNames=TRUE,rowNames=FALSE, headerStyle=hs,
                   firstColumn=TRUE,startRow=max(nrow(Facility_Code),nrow(Facility_Desc))+3,startCol=5, bandedRows=TRUE)
    setColWidths(wb, sheet9, 1:7, "auto")
    
    
    ## sheet 10
    sheet10 <- addWorksheet(wb,"Chief_Complaint_Check")
    ## frequency and percentage for each Chief_Complaint_Text, Admit_Reason_Description, Triage_Notes, and Clinical_Impression
    Chief_Complaint_Text=chief_complaint_text_count(subdata)
    Admit_Reason=admit_reason_description_count(subdata)
    Triage_Notes=triage_notes_count(subdata)
    Clinical_Impression=clinical_impression_count(subdata)
    output=bind_rows(Chief_Complaint_Text,Admit_Reason,Triage_Notes,Clinical_Impression) ## remove the column name
    
    writeDataTable(wb,sheet10,output,colNames=TRUE,rowNames=FALSE,headerStyle=hs, firstColumn=TRUE, bandedRows=TRUE)

    setColWidths(wb,sheet10,1:5,"auto")
    
    
    ## sheet 11 check Recorded_Date_Time and C_Visit_Date_time
    sheet11<- addWorksheet(wb, "Invalid Date_Time")
    check_date=date_time_invalid(subdata)[[2]]%>%
                    gather(key, value, 2:ncol(.))%>%
                    separate(key, c("Field","Measure"), "\\.")%>%
                    spread(Measure, value)%>%
                    select(-C_Biosense_Facility_ID)
    
    writeDataTable(wb,sheet11,check_date,colNames=TRUE,rowNames=FALSE,headerStyle=hs, firstColumn=TRUE, bandedRows=TRUE)
    setColWidths(wb,sheet11,1:3,"auto")
   # write to file
    filename <- str_replace_all(fname, "[^[a-zA-z\\s0-9]]", "") %>% # get rid of punctuation from faciltiy name
      str_replace_all("[\\s]", "_") # replace spaces with underscores
    saveWorkbook(wb, paste0(directory, "/", filename, "_Summary.xlsx"), overwrite=TRUE)
  }
  
  
  
  
  ## facility by facility examples
  if (nexamples > 0) {
    
    # get list of invalid examples data frames
    # DO NOT CHANGE THE ORDER OF THIS LIST
    invalid_examples <- list(admit_source_invalid(data)[[1]], # 1
                             age_invalid(data)[[1]], # 2
                             any_e_invalid(data)[[1]], # 3
                             blood_pressure_invalid(data)[[1]], # 4
                             cc_ar_invalid(data)[[1]], # 5
                             country_invalid(data)[[1]], # 6
                             county_invalid(data)[[1]], # 7
                             death_invalid(data)[[1]], # 8
                             diagnosis_type_invalid(data)[[1]], # 9
                             discharge_disposition_invalid(data)[[1]], # 10
                             ethnicity_invalid(data)[[1]], # 11 
                             facility_type_invalid(data)[[1]], # 12
                             fpid_mrn_invalid(data)[[1]], # 13
                             gender_invalid(data)[[1]], # 14
                             height_invalid(data)[[1]], # 15
                             patient_class_invalid(data)[[1]], # 16
                             pulseox_invalid(data)[[1]], # 17
                             race_invalid(data)[[1]], # 18
                             smoking_status_invalid(data)[[1]], # 19
                             state_invalid(data)[[1]], # 20
                             temperature_invalid(data)[[1]], # 21
                             weight_invalid(data)[[1]], # 22
                             zip_invalid(data)[[1]], # 23
                             diagnosis_code_invalid(data)[[1]], #24
                             date_time_invalid(data)[[1]])  #25
    
    for (i in data$C_Biosense_Facility_ID[!duplicated(data$C_Biosense_Facility_ID)]) { # for every unique facility id
      inv_examples <- examples_invalids(i, invalid_examples) # get examples of invalids from this facility
      null_examples <- examples_nulls(i, data) # get examples of nulls from this faciltiy
      # join with other relevant fields
      inv_examples <- inv_examples %>% # take examples
        left_join(., select(data, c(C_BioSense_ID, C_Visit_Date, C_Visit_Date_Time, First_Patient_ID, 
                              C_Unique_Patient_ID, Medical_Record_Number, Visit_ID, Admit_Date_Time, 
                              Recorded_Date_Time, Message_Date_Time, Create_Raw_Date_Time, 
                              Message_Type, Trigger_Event, Message_Structure, Message_Control_ID,
                              Patient_City, Patient_State, C_Patient_County, Patient_Country, Patient_Zip,
                              Age_Reported, Age_Units_Reported, Birth_Date_Time,
                              Height, Height_Units,Weight, Weight_Units,
                              Admit_Reason_Description, Chief_Complaint_Text, Clinical_Impression, Triage_Notes,
                              Diagnosis_Code, Diagnosis_Description, Diagnosis_Type,
                              Ethnicity_Code, Ethnicity_Description, Race_Code, Race_Description,
                              Facility_Type_Code, Facility_Type_Description,
                              Smoking_Status_Code, Smoking_Status_Description,
                              Initial_Temp, Initial_Temp_Units,
                              Initial_Pulse_Oximetry, Initial_Pulse_Oximetry_Units,
                              Death_Date_Time, C_Death, Discharge_Disposition, Discharge_Date_Time,
                              Systolic_Blood_Pressure, Systolic_Blood_Pressure_Units,
                              Diastolic_Blood_Pressure, Diastolic_Blood_Pressure_Units)),
                  by="C_BioSense_ID") %>% # join with all these fields, for every record of that visit
        rename(Invalid_Field=Field) %>% # make it clearer that that field is the one that is invalid
        group_by(Invalid_Field) %>% # group by type of field
        slice(1:nexamples) # get nexamples
      # do the same for nulls
      null_examples <- null_examples %>% 
        left_join(., select(data, c(C_BioSense_ID, C_Visit_Date, C_Visit_Date_Time, First_Patient_ID, 
                                    C_Unique_Patient_ID, Medical_Record_Number, Visit_ID, Admit_Date_Time, 
                                    Recorded_Date_Time, Message_Date_Time, Create_Raw_Date_Time, 
                                    Message_Type, Trigger_Event, Message_Structure, Message_Control_ID,
                                    Patient_City, Patient_State, C_Patient_County, Patient_Country, Patient_Zip,
                                    Age_Reported, Age_Units_Reported, Birth_Date_Time,
                                    Height, Height_Units,Weight, Weight_Units,
                                    Admit_Reason_Description, Chief_Complaint_Text, Clinical_Impression, Triage_Notes,
                                    Diagnosis_Code, Diagnosis_Description, Diagnosis_Type,
                                    Ethnicity_Code, Ethnicity_Description, Race_Code, Race_Description,
                                    Facility_Type_Code, Facility_Type_Description,
                                    Smoking_Status_Code, Smoking_Status_Description,
                                    Initial_Temp, Initial_Temp_Units,
                                    Initial_Pulse_Oximetry, Initial_Pulse_Oximetry_Units,
                                    Death_Date_Time, C_Death, Discharge_Disposition, Discharge_Date_Time,
                                    Systolic_Blood_Pressure, Systolic_Blood_Pressure_Units,
                                    Diastolic_Blood_Pressure, Diastolic_Blood_Pressure_Units)),
                  by="C_BioSense_ID") %>% # join with all these fields, for every record of that visit
        group_by(Null_Field) %>% # group by type of field
        slice(1:nexamples) # get nexamples
      
      # write to excel workbook
      wb <- createWorkbook()
      # sheet 1: invalids
      sheet1 <- addWorksheet(wb, "Invalids")
      writeDataTable(wb, sheet1, inv_examples, firstColumn=TRUE, headerStyle=hs,bandedRows=TRUE)
      setColWidths(wb, sheet1, 1:ncol(inv_examples), "auto")
      freezePane(wb, sheet1, firstActiveRow=2, firstActiveCol=4)
      # sheet2: nulls
      sheet2 <- addWorksheet(wb, "Nulls")
      writeDataTable(wb, sheet2, null_examples, firstColumn=TRUE,headerStyle=hs, bandedRows=TRUE)
      setColWidths(wb, sheet2, 1:ncol(null_examples), "auto")
      freezePane(wb, sheet2, firstActiveRow=2, firstActiveCol=3)
      # write sheet
      fname <- as.character(unlist(unname(c(fnames[which(fnames$C_Biosense_Facility_ID==i),1])))) # get facility name
      filename <- str_replace_all(fname, "[^[a-zA-z\\s0-9]]", "") %>% # get rid of punctuation from faciltiy name
        str_replace_all("[\\s]", "_") # replace spaces with underscores
      saveWorkbook(wb, paste0(directory, "/", filename, "_Examples.xlsx"), overwrite=TRUE)
    }
  }
}
