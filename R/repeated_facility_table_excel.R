#' Write NSSP BioSense Platform Data Quality Summary Reports for Multiple Facility, Use excel as input
#'
#' @description
#' This performs `write_facilty_report`  function for all . It will generate summary report for all specified facility.
#' This function uses excel generated information 
#' The summary workbook shows percents and counts of nulls and invalids, Additionally it generates a timeliness
#' report and creates a table. The program can send out a report to designated email address
#' @param input location of input.xlsx file.
#' @param contact A datatable of 2 colomns, facility is the facility number, receiver is the receive email address
#' @return Report table stored at directory location. If email=TRUE, then a email will be sent. A table with facility, receiver and conformation of email being sent. 
#' 
#' @examples 
#' library(emayili)
#' library(ggplot2)
#' library(readxl)
#' repeated_facility_excel("Input.xlsx", contact = contact)
#' @import dplyr
#' @import tidyr
#' @import readxl
#' @export
repeated_facility_excel <- function(input, contact){
  Input <- read_excel(input, col_names = FALSE)
  
  username <- as.character(Input[1,2])
  password <- as.character(Input[2,2])
  table <- as.character(Input[3,2])
  mft <- as.character(Input[4,2])
  start <- as.POSIXct(as.numeric(Input[5,2] ) *(60*60*24),origin= '1899-12-30')  
  end <- as.POSIXct(as.numeric(Input[6,2] ) *(60*60*24),origin= '1899-12-30')  

  directory <- as.character(Input[8,2])
  field <- ifelse(is.na(Input[9,2]),NA, as.character(Input[9,2]))
  exclude <- ifelse(is.na(Input[10,2]),NA, as.character(Input[10,2]))
  email <-as.logical (Input[11,2])
  sender<-as.character(Input[12,2])
  email_password <-as.character(Input[13,2])
  
  personname<-as.character(Input[15,2])
  title<- as.character(Input[16,2])
  phone<- as.character(Input[17,2])
  
  repeated_facility_table(contact=contact, username=username, password=password, 
                        table=table, mft=mft,
                        start=start, 
                        end=end,
                        directory=directory,exclude=exclude,
                        email =email, sender=sender,
                        email_password=email_password,personname=personname,title=title, phone=phone)
}