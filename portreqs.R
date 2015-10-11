portreq <- function(){
  
  #########################
  ##
  ##  data analysis on port request problems
  ##
  ########################
  
  library(ggplot2)
  library(dplyr)
  library(sqldf)
  library(scales)
  library(reshape2)
  library(lubridate)

  ## read the pids input file
  ##pids1 <- read.csv("C:/Users/andrewll/Documents/metrics/inputpids.csv", 
  ##                  header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  pids1 <- read.csv("C:/Users/andrewll/Documents/metrics/inputpids2.csv", 
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
  ##pids2$CommittedDeliveryDate1<-as.Date(pids2$CommittedDeliveryDate1,format = "%m/%d/%Y")
  pids2$CommittedDeliveryDate<-as.Date(pids2$CommittedDeliveryDate,format = "%m/%d/%Y")
  ##pids2$woadDock2<-as.Date(pids2$woadDock2,format = "%m/%d/%Y")
  pids2$WorkOrderActualDockDate<-as.Date(pids2$WorkOrderActualDockDate,format = "%m/%d/%Y")
  ##pids2$RequestedDeliveryDate3<-as.Date(pids2$RequestedDeliveryDate3,format = "%m/%d/%Y")
  pids2$RequestedDeliveryDate<-as.Date(pids2$RequestedDeliveryDate,format = "%m/%d/%Y")
  
  wo3$Work.Order.Actual.Start.Date<-as.Date(wo3$Work.Order.Actual.Start.Date,format = "%m/%d/%Y")
  wo3$Work.Order.Actual.End.Date<-as.Date(wo3$Work.Order.Actual.End.Date,format = "%m/%d/%Y")
  
  
  ##convert the DeliveryNumber field to character
  preqs2$GFSD.UTS <- as.character(preqs2$GFSD.UTS)
  pids2$DeliveryNumber <- as.character(pids2$DeliveryNumber)
  wo3$DeliveryNumber <- as.character(wo3$DeliveryNumber)
  
  ##remove the dots and underscores from the column names
  preqs2names <- gsub("\\.","",names(preqs2))
  colnames(preqs2) <- c(preqs2names)

  wo3names <- gsub("\\.","",names(wo3))
  colnames(wo3) <- c(wo3names)
  
  ##make column name for primary key the same for all input tables
  preqs3 <- mutate(preqs2, DeliverNumber = GFSDUTS)
  
  ##join the tables by DeliveryNumber
  SQLQuery1 <- "SELECT t.ID
  ,t.GFSDUTS
  ,t.CreatedDate
  ,t.Actual_Finish
  ,t.EG
  ,p.DeliveryNumber
  ,p.EG
  ,p.WorkOrderActualDockDate
  ,p.CommittedDeliveryDate
  ,p.RTEGActualDeliveryDate
  ,p.DemandCreatedDate
  ,p.ProjectCreationDate
  ,p.DockToRTEG
  FROM preqs3 t
  LEFT JOIN pids2 p
  ON t.GFSDUTS = p.DeliveryNumber"
  
  result <- sqldf(SQLQuery1)
  
  ##calculate new variables
  
  
  
  
  
  
  
  
  
  
  
}