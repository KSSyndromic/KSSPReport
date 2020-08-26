#' Write NSSP BioSense Platform Data Quality Summary Reports for one Facility, Use excel as input
#'
#' @description
#' This performs `write_facilty_report`  function for all . It will generate summary report for all specified facility.
#' This function uses excel generated information 
#' The summary workbook shows percents and counts of nulls and invalids, Additionally it generates a timeliness
#' report and creates a table. The program can send out a report to designated email address
#' @param input location of input.xlsx file.
#' @examples 
#' library(emayili)
#' library(ggplot2)
#' library(readxl)
#' write_facility_bo_excel("Input.xlsx")
#' @import dplyr
#' @import tidyr
#' @import readxl
#' @export
write_facility_bo_excel <- function(input){
  Input <- read_excel(input, col_names = FALSE)
  
  username <- as.character(Input[1,2])
  password <- as.character(Input[2,2])
  table <- as.character(Input[3,2])
  mft <- as.character(Input[4,2])
  start <- as.POSIXct(as.numeric(Input[5,2] ) *(60*60*24),origin= '1899-12-30')  
  end <- as.POSIXct(as.numeric(Input[6,2] ) *(60*60*24),origin= '1899-12-30')  
  facility <- as.character(Input[7,2])
  directory <- as.character(Input[8,2])
  field <- ifelse(is.na(Input[9,2]),NA, as.character(Input[9,2]))[1]
  exclude <- ifelse(is.na(Input[10,2]),NA, as.character(Input[10,2]))[1]
  email <-as.logical (Input[11,2])
  sender<-as.character(Input[12,2])
  email_password <-as.character(Input[13,2])
  receiver <-as.character(Input[14,2])
  personname<-as.character(Input[15,2])
  title<- as.character(Input[16,2])
  phone<- as.character(Input[17,2])
    
  write_facility_report(username=username, password=password, 
                        table=table, mft=mft,
                        start=start, 
                        end=end,
                        facility=facility,
                        directory=directory,field=field,exclude=exclude,
                        email =email, sender=sender,receiver=receiver,
                        email_password=email_password,personname=personname,title=title, phone=phone)
}
