portreq <- function(){
  
  #########################
  ##
  ##  data analysis on port request problems
  ##
  ########################
  
  library(dplyr)
  library(lubridate)
  library(ggplot2)

  ## read the pids input file
  pids1 <- read.csv("C:/Users/andrewll/Documents/metrics/inputpids.csv", 
                    header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  
  ## read the port requests input file
  preqs1 <- read.csv("C:/Users/andrewll/Documents/metrics/portrequests.csv",
                    header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  
  ## read the work orders input file
  wo1 <- read.csv("C:/Users/andrewll/Documents/metrics/workorders.csv", 
                    header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  
  ##create dataframes
  pids2 <- tbl_df(pids1)
  preqs2<- tbl_df(preqs1)
  wo2 <- tbl_df(wo1)
  
  ##trim the work orders down to 4 types
  ##netnames<-c("Submit NetworkPort Request","Fulfill Network Port","Start NDT","Complete NDT")
  netnames<-c("Submit NetworkPort Request","Fulfill Network Port")
  wo3<-wo2[which(wo2$Work.Order.Name==netnames),]
  
  ##convert the date fields to dates
  preqs2$Created.Date<-as.Date(preqs2$Created.Date,format = "%m/%d/%Y")
  preqs2$Actual_Finish<-as.Date(preqs2$Actual_Finish,format = "%m/%d/%Y")
  
  pids2$DemandCreatedDate<-as.Date(pids2$DemandCreatedDate,format = "%m/%d/%Y")
  pids2$ProjectCreationDate<-as.Date(pids2$ProjectCreationDate,format = "%m/%d/%Y")
  pids2$RTEGActualDeliveryDate<-as.Date(pids2$RTEGActualDeliveryDate,format = "%m/%d/%Y")
  pids2$CommittedDeliveryDate1<-as.Date(pids2$CommittedDeliveryDate1,format = "%m/%d/%Y")
  pids2$woadDock2<-as.Date(pids2$woadDock2,format = "%m/%d/%Y")
  pids2$RequestedDeliveryDate3<-as.Date(pids2$RequestedDeliveryDate3,format = "%m/%d/%Y")
  
  wo3$Work.Order.Actual.Start.Date<-as.Date(wo3$Work.Order.Actual.Start.Date,format = "%m/%d/%Y")
  wo3$Work.Order.Actual.End.Date<-as.Date(wo3$Work.Order.Actual.End.Date,format = "%m/%d/%Y")
  
  
  ##convert the DeliveryNumber field to character
  
  ##remove the dots from the column names
  
  ##join the tables by DeliveryNumber
  
  ##calculate new variables
  
  
  
  
  
  
  
  
  
  
  
}