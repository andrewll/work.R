cycletime2<-function(){
  
  #################
  ##
  ##  Purpose: calculate cycle time for 3 major phases: PIDCreate-to-POCreate, POCreate-to-POApprove, POApprove-to-Dock, Dock-to-RTEG
  ##  This is a derivative of both the simulaterteg script and the cycletime script
  ##
  #################
  
  
  library(ggplot2)
  library(dplyr)
  library(sqldf)
  library(scales)
  library(reshape2)
  library(lubridate)
  
  EGList <- c("O365 Exchange", "AP", "O365 SharePoint","CRM","XBOX","ISSD (Azure AAD)")
  
  ## read data containing milestone and PO information
  dat <- read.csv("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in/DeliveryPerformanceWithMilestone.csv", stringsAsFactors = TRUE)
  
  ##read data containing pidcreate information
  mydf<-read.csv("C:/Users/andrewll/Documents/R/MCIOdata/All/DelCap-Jul1-Oct26-alleg-networkandservers.csv", stringsAsFactors = FALSE)
  
  

  
  ##convert Delivery Number to correct format
  dat$DeliveryNumber<-as.character(dat$DeliveryNumber)
  mydf$DeliveryNumber<-as.character(mydf$DeliveryNumber)
  
  ##remove dots in header names in pids table
  pidsnames <- gsub("\\.","",names(dat))
  colnames(dat) <- c(pidsnames)
  
  ##merge the two dataframes
  SQLQuery1 <- "SELECT p.DeliveryNumber
  ,p.ProjectTitle
  ,p.RTEGActualDeliveryDate
  ,p.RequestedDeliveryDate
  ,p.EG
  ,p.ProjectCategory
  ,p.DeploymentClass
  ,w.DemandCreatedDate
  ,w.ProjectCreationDate
  ,w.DTR1
  ,w.woadDock2
  ,p.MaxPOCreateDate
  ,p.MaxPOApproveDate
  FROM dat p
  LEFT JOIN mydf w 
  ON p.DeliveryNumber = w.DeliveryNumber"
  
  pids <- sqldf(SQLQuery1)
  
  ##convert dates into correct format
  pids$RTEGActualDeliveryDate <- as.Date(pids$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  pids$woadDock2<- as.Date(pids$woadDock2, format = "%m/%d/%Y")
  
  
  
  
}