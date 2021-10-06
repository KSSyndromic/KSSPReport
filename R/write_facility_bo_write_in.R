#' Write Facility Write-In
#'
#' @return a data frame produced by write_facility_report
write_facility_bo_write_in <- function(){
  
  
  username <- readline("What is your BioSense username?")  
  password <- readline("What is your BioSense password?")
  table <- readline("What table do you wish to use?")
  mft <- readline("What master facilities table do you want to use?")
 start <- readline("What is the start date")  
  end <- readline("What is the end date")
  facility <- readline("What is the facility number?")
  directory <- readline("Where do you want to save")
  field <- readline("What specific field do you want to select?")  
  exclude <- readline("What specific field do you want to exclude")
  email <- readline("Do you want to email this? True or False")
  email <-as.logical(email)
  if (email==T){
  sender <- readline("What is the sender email address?")
  email_password <- readline("What is the sender email password?")  
  receiver <- readline("What is the receiving email address")
  }
  
 

  
  write_facility_report(username=username, password=password, 
                        table=table, mft=mft,
                        start=start, 
                        end=end,
                        facility=facility,
                        directory=directory,field=field,exclude=exclude,
                        email =email, sender=sender,receiver=receiver,
                        email_password=email_password,personname=presonname,title=title, phone=phone)
}