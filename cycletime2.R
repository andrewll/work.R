cycletime2<-function(){
  
  #################
  ##
  ##  Purpose: calculate cycle time for 3 major phases: PIDCreate-to-POCreate, POCreate-to-POApprove, POApprove-to-Dock, Dock-to-RTEG
  ##  This is a derivative of both the simulaterteg script and the cycletime script
  ##
  #################
  
  
  library(dplyr)
  library(lubridate)
  
  EGList <- c("O365 Exchange", "AP", "O365 SharePoint","CRM","XBOX","ISSD (Azure AAD)")
  
  ## read data containing milestone and PO information
  dat1 <- read.csv("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in/DeliveryPerformanceWithMilestone.csv", stringsAsFactors = TRUE)
  
  ##read data containing pidcreate information
  mydf<-read.csv("C:/Users/andrewll/Documents/R/MCIOdata/All/DelCap-Jul1-Oct26-alleg-networkandservers.csv", stringsAsFactors = FALSE)
  
  
  ##convert dates into correct format
  dat1$RTEGActualDeliveryDate <- as.Date(dat1$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  dat1$Woad.Dock<- as.Date(dat1$Woad.Dock, format = "%m/%d/%Y")
  
  ##convert Delivery Number to correct format
  dat1$DeliveryNumber<-as.character(dat1$DeliveryNumber)
  
  
  ##remove dots in header names in pids table
  pidsnames <- gsub("\\.","",names(dat1))
  colnames(dat1) <- c(pidsnames)
  
  ## select desired variables
  dat2<- subset(dat1, select = c("DeliveryNumber"
                                 ,"DeploymentClass"
                                 ,"EG"
                                 ,"ProjectCategory"
                                 ,"DataCenter"
                                 ,"RTEGActualDeliveryDate"
                                 ,"NetworkReadinessValue"
                                 ,"DCReadinessValue"
                                 ,"ProcurementValue"
                                 ,"ReceivingValue"
                                 ,"BoltandEnergizeValue"
                                 ,"PhysicalCablingValue" 
                                 ,"ConfigureVerifyNetworkValue"
                                 ,"OperationalAcceptanceValue"
                                 ,"WoadDock"
                                 ,"MaxPOCreateDate"
                                 ,"MaxPOApproveDate"))
  
  mydf2<- subset(mydf, select = c("DeliveryNumber"
                                  ,"DemandCreatedDate"
                                  ,"ProjectCreationDate"))
  
  ##merge the two dataframes
  
  
}