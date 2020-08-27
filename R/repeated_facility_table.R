#' Write NSSP BioSense Platform Data Quality Summary Reports for Multiple Facility
#'
#' @description
#' This function iteratively performs `write_facilty_report`  function for all . It will generate summary report for all specified facility.
#' The summary workbook shows percents and counts of nulls and invalids, Additionally it generates a timeliness
#' report and creates a table. The program can send out a report to designated email address
#' @param contact A datatable of 2 colomns, facility is the facility number, receiver is the receive email address
#' @param username Your BioSense username, as a string. This is the same username you may use to log into RStudio or Adminer.
#' @param password Your BioSense password, as a string. This is the same password you may use to log into RStudio or Adminer.
#' @param table The table that you want to retrieve the data from, as a string.
#' @param mft The MFT (master facilities table) from where the facility name will be retrieved, as a string.
#' @param start The start date time that you wish to begin pulling data from, as a string.
#' @param end The end data time that you wish to stop pulling data from, as a string.
#' @param directory The directory where you would like to write the reports to (i.e., "~/Documents/MyReports"), as a string.
#' @param field Default NA. Can add a string with delimiter of ':'. Only fields that countain those words will be included in the final report.
#' @param exclude Default NA. Can add a string with delimiter of ':'. Exclude fields with certain keywords in the final report.
#' @param optional Default True. If False then remove all optional fields
#' @param email Default False. If True, then the function will atempt to send out a form
#' @param sender Email address of sender. Make sure it's kdhe.KS.gov
#' @param email_password Your Email Password
#' @param personname Your Name to be used in your email text
#' @param title Your job title to be used in your email text
#' @param phone Your phone number to be used in your email text
#' @return Report table stored at directory location. If email=TRUE, then a email will be sent. A table with facility, receiver and conformation of email being sent. 
#' 
#' @examples 
#' library(emayili)
#' library(ggplot2)
#' library(readxl)
#' library(keyring)
#' 
#' ## store passwords for essence
#' key_set(service = "essence")
#' ## store passwords for email
#' key_set(service = "email")
#' Facilities_Spreadsheet <- read_excel("Facilities Spreadsheet.xlsx", 
#' sheet = "Data Quality Reports")
#' contact<-as.data.frame(cbind(Facilities_Spreadsheet$C_Biosense_Facility_ID, Facilities_Spreadsheet$`Facilty Contact Email`)) 
#' colnames(contact)=c('facility','receiver')
#' repeated_facility_table(contact=contact, username="bzhang02", password=key_get("essence"),
#'  table="KS_PR_Processed", mft="KS_MFT",  start="2020-05-01 00:00:00", 
#'  end="2020-05-30 23:59:59", directory="~",
#'  email =TRUE,sender="bo.zhang@@kdhe.ks.gov",
#' email_password=key_get("email"),personname='Bo Zhang',title='intern',phone='630-457-8915')
#' @export
repeated_facility_table<-function(contact,table, mft, username,password,start, end,directory,field=NA,exclude=NA,optional=T,email=F, sender,email_password,personname=NA,title=NA, phone=NA){
niter= nrow(contact)
success=NA
for (i in 1:niter){
  if(is.na(contact$receiver[i])==F & is.na(contact$facility[i])==F){
success[i]=write_facility_report(username=username, password=password, 
                      table=table, mft=mft,
                      start=start, 
                      end=end,
                      facility=contact$facility[i],
                      directory=directory,field=field,exclude=exclude,
                      email =email, sender=sender,receiver=as.character(contact$receiver[i]),
                      email_password=email_password,personname=personname,title=title, phone=phone)
  }else {
  success[i]='Missing receiver email or facility id'
}

  }
report=cbind(contact,success)
write.csv(report,paste0(directory, "/AdminReport",format(Sys.Date(),'%b_%d_%Y'),".csv"), row.names = TRUE)
  return(report)
}
