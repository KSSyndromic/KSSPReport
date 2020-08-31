#' Write NSSP BioSense Platform Data Quality Summary Reports for One Facility
#'
#' @description
#' This function is a `write_reports` function. It will generate summary report for one specified facility.
#' The summary workbook shows percents and counts of nulls and invalids, Additionally it generates a timeliness
#' report and creates a table. The program can send out a report to designated email address
#'
#' @param username Your BioSense username, as a string. This is the same username you may use to log into RStudio or Adminer.
#' @param password Your BioSense password, as a string. This is the same password you may use to log into RStudio or Adminer.
#' @param table The table that you want to retrieve the data from, as a string.
#' @param mft The MFT (master facilities table) from where the facility name will be retrieved, as a string.
#' @param start The start date time that you wish to begin pulling data from, as a string.
#' @param end The end data time that you wish to stop pulling data from, as a string.
#' @param facility The C_Biosense_Facility_ID for the facility that you wish to generate and write the report for.
#' @param directory The directory where you would like to write the reports to (i.e., "~/Documents/MyReports"), as a string.
#' @param field Default NA. Can add a string with delimiter of ':'. Only fields that countain those words will be included in the final report.
#' @param exclude Default NA. Can add a string with delimiter of ':'. Exclude fields with certain keywords in the final report.
#' @param optional Default True. If False then remove all optional fields
#' @param email Default False. If True, then the function will atempt to send out a form
#' @param sender Email address of sender. Make sure it's kdhe.KS.gov
#' @param receiver Email address of receiver.
#' @param email_password Your Email Password
#' @param personname Your Name to be used in your email text
#' @param title Your job title to be used in your email text
#' @param phone Your phone number to be used in your email text
#' @return A report table stored at directory location. If email=TRUE, then a email will be sent, along with a confirmation of email being sent. 
#' @examples 
#' library(emayili)
#' library(ggplot2)
#' library(keyring)
#' library(readxl)
#' ## store passwords for essence
#' key_set(service = "essence")
#' ## store passwords for email
#' key_set(service = "email")
#' ## if you want the report only 
#' write_facility_report(username="bzhang02", password=key_get("essence"), 
#'                    table="KS_PR_Processed", mft="KS_MFT",
#'                    start="2020-06-1 00:00:00", 
#'                    end="2020-07-31 23:59:59",
#'                    facility=3890,
#'                    directory="~") 
#'  ## if you want to only include field containing sex and age
#' write_facility_report(username="bzhang02", password=key_get("essence"), 
#'                    table="KS_PR_Processed", mft="KS_MFT",
#'                    start="2020-06-1 00:00:00", 
#'                    end="2020-07-31 23:59:59", field="sex;age"
#'                    facility=3890,
#'                    directory="~") 
#' ## if you want to exclude certain fields
#' write_facility_report(username="bzhang02", password=key_get("essence"), 
#'                    table="KS_PR_Processed", mft="KS_MFT",
#'                    start="2020-06-1 00:00:00", 
#'                    end="2020-07-31 23:59:59",
#'                    exclude=field
#'                    facility=3890,
#'                    directory="~") 
#' ## if you want to send out an email                  
#' write_facility_report(username="bzhang02", password=key_get("essence"), 
#'                    table="KS_PR_Processed", mft="KS_MFT",
#'                    start="2020-06-1 00:00:00", 
#'                    end="2020-07-31 23:59:59",
#'                    facility=3890,
#'                    directory="~",
#'                    email =TRUE,sender='bo.zhang@@kdhe.ks.gov',receiver="bo.zhang@@kdhe.ks.gov;Greg.Crawford@@ks.gov",
#'                    email_password=key_get("email"),personname='Bo Zhang',title='intern',phone='630-457-8915')
#' 
#'
#' @import dplyr
#' @import tidyr
#' @import openxlsx
#' @import RODBC
#' @import emayili
#' @import ggplot2
#' @importFrom stringr str_replace_all
#' @export
write_facility_report <- function(username, password, table, mft, start, end, facility, directory="",field=NA, exclude=NA, optional=TRUE, email=FALSE, sender=NA,receiver=NA,email_password=NA, personname=NA,title=NA, phone=NA,message=NA) {
      start1=as.POSIXct(start)
    end1=as.POSIXct(end)
  if (start1>end1){
    stop("Error: Start time after end time")
  } else {  

    channel <- odbcConnect("BioSense_Platform", paste0("BIOSENSE\\", username), password) # open channel
    data <- sqlQuery(
      channel,
      paste0("SELECT * FROM ", table, " WHERE C_Visit_Date_Time >= '", start1, "' AND C_Visit_Date_Time <= '", end1, "' AND C_Biosense_Facility_ID = ", facility) # create sql query
      , as.is=TRUE)
    
    if (nrow(data) == 0) {
      emailed='query yielded no data'
      odbcCloseAll()
      print("The query yielded no data.")
    }
    else{
      name <- as.character(unlist(unname(c(sqlQuery(channel, paste0("SELECT Facility_Name FROM ", mft, " WHERE C_Biosense_Facility_ID = ", facility)))))) # get name from mft
      
      odbcCloseAll() # close connection
      
      if (length(name)>1) {
        name=name[1]
      }
      
      field=unlist(strsplit(as.character(field), ';|,'))
      exclude=unlist(strsplit(as.character(exclude), ';|,'))
      # get hl7 values
      data("hl7_values", envir=environment())
      hl7_values$Field <- as.character(hl7_values$Field)
      
      
      # get facility-level state summary of required nulls
      req_nulls <- get_req_nulls_BZ(data) %>%
        select(-c(C_Biosense_Facility_ID)) %>%
        gather(Field, Value, 2:ncol(.)) %>%
        spread(Measure, Value) %>%
        right_join(hl7_values, ., by = "Field")
      # get facility-level state summary of optional nulls
      opt_nulls <- get_opt_nulls_BZ(data) %>%
        select(-c(C_Biosense_Facility_ID)) %>%
        gather(Field, Value, 2:ncol(.)) %>%
        spread(Measure, Value) %>%
        right_join(hl7_values, ., by = "Field")
      # get facility-level state summary of invalids
      invalids <- get_all_invalids_BZ(data) %>%
        select(-c(C_Biosense_Facility_ID)) %>%
        gather(Field, Value, 2:ncol(.)) %>%
        spread(Measure, Value) %>%
        right_join(hl7_values, ., by = "Field")
      
      # get overall complete by merging req_null and opt_null
      overall_complete<- full_join(req_nulls,opt_nulls) %>%
        mutate(Percent_Complete=100-Percent) %>%
        select(-c(Count,Percent))
      #create a table of overall complete% and overall valid
      overall<-full_join(overall_complete,invalids) %>%
        mutate(Percent_Valid=100-Percent) %>%
        select(-c(Count,Percent)) 
      
      nrow1=length(overall$Field)
      #add warnings
      for (i in 1:nrow1){
        if(is.na(overall$Percent_Valid[i]) & is.na(overall$Percent_Complete[i])) {
          overall$Warning[i]="Warning: Percent Complete and Percent Valid Missing"
        } else if (is.na(overall$Percent_Complete[i])& overall$Percent_Valid[i]<90){
          overall$Warning[i]="Warning: Percent Valid Under 90"
        } else if (is.na(overall$Percent_Valid[i])& overall$Percent_Complete[i]<90){
          overall$Warning[i]="Warning: Percent Complete Under 90"
        }  else if(overall$Percent_Valid[i]<90 & overall$Percent_Complete[i]<90) {
          overall$Warning[i]="Warning: Percent Complete and Percent Valid Under 90"
        } else {
          overall$Warning[i]=NA
        }
      }
      
      #Declare optional field
      overall$Optional=ifelse(overall$Field %in% opt_nulls$Field,"Optional", "Required" )
      #Remove optional if optional=False
      if (optional==F){
        overall<-overall %>%
          filter(overall$Optional=='Required')
      }
      #select the field needed
      if (is.na(field)==F){
        field1=paste(field,collapse="|")
        overall<-overall%>%
          filter(grepl(field1,overall$Field, ignore.case = T))
      }
      #exclude the select field
      if (is.na(exclude)==F){
        exclude1=paste(exclude,collapse="|")
        overall<-overall%>%
          filter(!grepl(exclude1,overall$Field, ignore.case = T))
      }
      nrow=length(overall$Field)
      # getting first and last visit date times
      vmin <- min(as.character(data$C_Visit_Date_Time))
      vmax <- max(as.character(data$C_Visit_Date_Time))
      amin <- min(as.character(data$Arrived_Date_Time))
      amax <- max(as.character(data$Arrived_Date_Time))
      Lag_Summary=lag_breakdown(data)
      Lag<-data.frame(
        Lag_Name=c("Arrival_Visit"),
        Lag_Between=c("Arrival_Date_Time-Visit_Date_Time"),
        Group1=t(Lag_Summary[1,3]),
        Group2=t(Lag_Summary[2,3]),
        Group3=t(Lag_Summary[3,3])
      )
      colnames(Lag)[3:5]=c("% Visit Received in <24 Hr","% Visit Received in 24-48 Hr", "% Visit Received in >48 Hr")
      filename <- str_replace_all(name, "[^[a-zA-z\\s0-9]]", "") %>% # get rid of punctuation from faciltiy name
        str_replace_all("[\\s]", "_") # replace spaces with underscores
      sheettitle=as.data.frame(paste("Syndromic Surveillance Quality Report for",as.character(gsub('_',' ',filename)), "from",format(as.Date(start1), "%b%d,%Y"), 'to', format(as.Date(end1), "%b%d,%Y")))
      ##create overall powerpoint
      wb <- createWorkbook()
      hs <- createStyle(fgFill="#4f81bd", halign="left", valign="top", textDecoration="bold", wrapText=TRUE)
      sheet1<- addWorksheet(wb, "Summary")
      mergeCells(wb, sheet1, cols = 1:6, rows = 1)
      writeData(wb, sheet1,sheettitle , startRow=1, colNames = F) # write table title
      writeDataTable(wb, sheet1, overall, firstColumn=TRUE,startRow=2, headerStyle=hs, bandedRows=TRUE, colNames=TRUE,rowNames=FALSE) # write Completeness to table
      setColWidths(wb, sheet1, 2:ncol(overall), "auto") # format sheet
      freezePane(wb, sheet1, firstActiveRow=3) # format sheet
      writeDataTable(wb,sheet1,Lag,startCol=1,startRow=nrow+4, headerStyle=hs, colNames=TRUE,rowNames=FALSE) #write Timeliness to table
      
      ##colorcode sheet
      negStyle <- createStyle(fontColour = "#000000", bgFill = "#FFC7CE")
      posStyle <- createStyle(fontColour = "#000000", bgFill = "#C6EFCE")
      negStyle1 <- createStyle(fontColour = "#000000", fgFill = "#FFC7CE")
      posStyle1 <- createStyle(fontColour = "#000000", fgFill = "#C6EFCE")
      midStyle <- createStyle(fontColour = "#000000", fgFill = "#FFFF00")
      naStyle <- createStyle(bgFill = "#808080")
      
      conditionalFormatting(wb, sheet1, cols=3, rows=2:(nrow+2), rule="$C2<90", style = negStyle)
      conditionalFormatting(wb, sheet1, cols=3, rows=2:(nrow+2), rule="$C2>=90", style = posStyle) 
      conditionalFormatting(wb, sheet1, cols=3, rows=2:(nrow+2), rule="ISBLANK($C2)=TRUE", style = naStyle)
      conditionalFormatting(wb, sheet1, cols=4, rows=2:(nrow+2), rule="$D2<90", style = negStyle)
      conditionalFormatting(wb, sheet1, cols=4, rows=2:(nrow+2), rule="$D2>=90", style = posStyle)
      conditionalFormatting(wb, sheet1, cols=4, rows=2:(nrow+2), rule="ISBLANK($D2)=TRUE", style = naStyle)
      addStyle(wb, sheet1, cols=3, rows=(nrow+5), style = posStyle1)
      addStyle(wb, sheet1, cols=4, rows=(nrow+5), style = midStyle)
      addStyle(wb, sheet1, cols=5, rows=(nrow+5), style = negStyle1)
      ##Create Graph of change in delay over time
      
      ##insert plot of mean weekly delay plot if the time interval greater than 14 days
      if (difftime(end1,start1,units = "day")>14){
      ggsave(file=paste0( filename, "_WeeklyDelay.png"),lag_graph(data,start1,end1),dpi = 300,path = directory,width=8, height=6, unit ="in")
      insertImage(wb, sheet1, paste0( directory, "/", filename, "_WeeklyDelay.png"), startRow = (nrow+8), startCol = 1, width = 8, height = 6)
      }
      
      # write sheet
      saveWorkbook(wb, paste0(directory, "/", filename, "_Overall.xlsx"), overwrite=TRUE)
      if (email==F){ emailed='Reporter generated Email not supplied'}
      else if (email==T){
        #compose email message
        warningcount=which(!is.na(overall$Warning))
        nwarning= length(warningcount)
        subject= paste(gsub('_',' ',filename),"Syndromic Surveillance Quality Report from KDHE")
        if (is.na(message)){
        bodytext= paste("<p style='color: rgb(50, 49, 48); font-style: normal; font-variant-ligatures: normal; font-variant-caps: normal; font-weight: 400; letter-spacing: normal; orphans: 2; text-align: start; text-indent: 0px; text-transform: none; white-space: normal; widows: 2; word-spacing: 0px; -webkit-text-stroke-width: 0px; background-color: rgb(255, 255, 255); text-decoration-style: initial; text-decoration-color: initial; font-size: 11pt; font-family: Calibri, sans-serif; margin: 0px;'>All,</p>
<p style='color: rgb(50, 49, 48); font-style: normal; font-variant-ligatures: normal; font-variant-caps: normal; font-weight: 400; letter-spacing: normal; orphans: 2; text-align: start; text-indent: 0px; text-transform: none; white-space: normal; widows: 2; word-spacing: 0px; -webkit-text-stroke-width: 0px; background-color: rgb(255, 255, 255); text-decoration-style: initial; text-decoration-color: initial; font-size: 11pt; font-family: Calibri, sans-serif; margin: 0px;'>&nbsp;</p>
<p style='color: rgb(50, 49, 48); font-style: normal; font-variant-ligatures: normal; font-variant-caps: normal; font-weight: 400; letter-spacing: normal; orphans: 2; text-align: start; text-indent: 0px; text-transform: none; white-space: normal; widows: 2; word-spacing: 0px; -webkit-text-stroke-width: 0px; background-color: rgb(255, 255, 255); text-decoration-style: initial; text-decoration-color: initial; font-size: 11pt; font-family: Calibri, sans-serif; margin: 0px;'>        Greetings, this is a data quality summary for your hospital's submission from" , as.Date(start), " to", as.Date(end),"from the Kansas Syndromic Surveillance Program at the Kansas Department of Health and Environment.  We provide the attached report as a way letting you know about the completeness, validity and timeliness of your emergency department data submitted to our program.  There may be many reasons why the data sent to us failed the Centers for Disease Control and Prevention 90 percent standard. We would be happy to work with you if you have questions about the report or why your data may not meet standards. If the attached report does not have any red highlights for lower quality, then you don't need to take any further action.  We appreciate your attention to data quality.  For more information on the fields we received from your Electronic Health Records system, please visit this link:<a href='https://www.kdheks.gov/phi/download/Emergency_Department_Visit_Records_Data_Quality_Report.pdf'> Emergency Department Visit Records Data Quality Report</a>. We know you may have concerns about authenticity of this message. To verify this is authentic, you may contact me,",personname,", ",title,", ", sender, " to discuss any questions you may have. Please visit our Kansas Syndromic Surveillance Program (KSSP) <a href='https://www.kdheks.gov/phi/KSSP.htm'>web page</a><a href='https://www.kdheks.gov/phi/KSSP.htm'>&nbsp;</a></span><span style='color: rgb(50, 49, 48); font-family: Calibri, sans-serif; font-size: 14.6667px; font-style: normal; font-variant-ligatures: normal; font-variant-caps: normal; font-weight: 400; letter-spacing: normal; orphans: 2; text-align: start; text-indent: 0px; text-transform: none; white-space: normal; widows: 2; word-spacing: 0px; -webkit-text-stroke-width: 0px; background-color: rgb(255, 255, 255); text-decoration-style: initial; text-decoration-color: initial; display: inline !important; float: none;'>  to learn more about the program. The Kansas Syndromic Surveillance Program (KSSP) group can be contacted by email <a href='mailto:kdhe.syndromic@ks.gov'>kdhe.syndromic@ks.gov</a>.</span></p>
<p style='color: rgb(50, 49, 48); font-style: normal; font-variant-ligatures: normal; font-variant-caps: normal; font-weight: 400; letter-spacing: normal; orphans: 2; text-align: start; text-indent: 0px; text-transform: none; white-space: normal; widows: 2; word-spacing: 0px; -webkit-text-stroke-width: 0px; background-color: rgb(255, 255, 255); text-decoration-style: initial; text-decoration-color: initial; font-size: 11pt; font-family: Calibri, sans-serif; margin: 0px;'><strong><span style='margin: 0px; padding: 0px; border: 0px; font-style: inherit; font-variant: inherit; font-weight: inherit; font-stretch: inherit; font-size: 14pt; line-height: inherit; font-family: inherit; vertical-align: baseline; color: inherit;'>&nbsp;</span></strong></p>")
        }else{
          message=unlist(strsplit(as.character(message), '\n'))
          bodytext=NA
          for (j in 1:length(message)){
            if (message[j]="",{
              bodytext=paste(bodytext,"<p>&nbsp; </p>") 
            }else{
              bodytext=paste(bodytext,"<p>",message[j],"</p>")
            }
          }
        }
        
        bodytext=paste(bodytext,"<p>&nbsp;</p>
<p style= 'color: rgb(32, 31, 30); font-style: normal; font-variant-ligatures: normal; font-variant-caps: normal; font-weight: 400; letter-spacing: normal; orphans: 2; text-align: start; text-indent: 0px; text-transform: none; white-space: normal; widows: 2; word-spacing: 0px; -webkit-text-stroke-width: 0px; background-color: rgb(255, 255, 255); text-decoration-style: initial; text-decoration-color: initial; font-size: 11pt; font-family: Calibri, sans-serif; margin: 0px 0px 0px 0pt;'>Kansas Syndromic Surveillance Program</p>
                 <p style='color: rgb(32, 31, 30); font-style: normal; font-variant-ligatures: normal; font-variant-caps: normal; font-weight: 400; letter-spacing: normal; orphans: 2; text-align: start; text-indent: 0px; text-transform: none; white-space: normal; widows: 2; word-spacing: 0px; -webkit-text-stroke-width: 0px; background-color: rgb(255, 255, 255); text-decoration-style: initial; text-decoration-color: initial; font-size: 11pt; font-family: Calibri, sans-serif; margin: 0px 0px 0px 0pt;'>",personname,", ", title, "</p>
                 <p style='color: rgb(32, 31, 30); font-style: normal; font-variant-ligatures: normal; font-variant-caps: normal; font-weight: 400; letter-spacing: normal; orphans: 2; text-align: start; text-indent: 0px; text-transform: none; white-space: normal; widows: 2; word-spacing: 0px; -webkit-text-stroke-width: 0px; background-color: rgb(255, 255, 255); text-decoration-style: initial; text-decoration-color: initial; font-size: 11pt; font-family: Calibri, sans-serif; margin: 0px 0px 0px 0pt;'>Bureau of Epidemiology and Public Health Informatics</p>
                 <p style='color: rgb(32, 31, 30); font-style: normal; font-variant-ligatures: normal; font-variant-caps: normal; font-weight: 400; letter-spacing: normal; orphans: 2; text-align: start; text-indent: 0px; text-transform: none; white-space: normal; widows: 2; word-spacing: 0px; -webkit-text-stroke-width: 0px; background-color: rgb(255, 255, 255); text-decoration-style: initial; text-decoration-color: initial; font-size: 11pt; font-family: Calibri, sans-serif; margin: 0px 0px 0px 0pt;'>Kansas Department of Health and Environment</p>
")
        receiver=unlist(strsplit(as.character(receiver), ';|,'))
        
        emailor <- envelope() %>%
          from(sender) %>%
          to(receiver) %>%
          cc("kdhe.syndromic@ks.gov") %>%
          subject(subject) %>%
          html(bodytext) %>%
          attachment(path=paste0(directory, "/", filename, "_Overall.xlsx"))
        
        smtp <- server(host = "smtp.office365.com",
                       port = 587,
                       username = sender,
                       password = email_password,
                       reuse= F)
        
        smtp(emailor, verbose = TRUE)
        emailed='email sent'
        
      }
    }
  }
  return(emailed)
  
}


