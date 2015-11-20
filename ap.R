ap<-function(){
  
  ###############################
  ##
  ##  The purpose of this function is to correctly categorize AP deployment data based on criteria submitted by Von and Lorinda
  ##
  ##
  ##
  ###############################
  
  ##load the libraries
  library(ggplot2)
  library(dplyr)
  library(sqldf)
  library(scales)
  library(reshape2)
  library(lubridate)
  
  ##set the EG variable
  ##EGList <- c("O365 Exchange", "AP", "O365 SharePoint","CRM","XBOX","ISSD (Azure AAD)")
  ##EGList <- c("O365 Exchange","O365 SharePoint")
  ##EGList <- c("O365 Exchange")
  ##EGList <- c("Networking")
  EGList <- c("AP", "Not Set", "Default", "NonWebComm")
  
  ##set DeploymentClass and ProjectCategory variables
  desiredDeploymentClass<-c("New Deployment")
  desiredProjectCategory<-c("PRD","Discrete","Network","SAN","ITPAC")
  
  ##set Von's variables
  voneg<-c("AP")
  vonpg<-c("Azure ECN", "Search")
  
  ##read data 
  pids<-read.csv("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in/DeliveryProjectStatusReport.csv", stringsAsFactors = FALSE)

  ##convert Delivery Number to correct format
  pids$DeliveryNumber<-as.character(pids$DeliveryNumber)
  
  ##convert dates into correct format
  pids$RTEGActualDeliveryDate <- as.Date(pids$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  pids$DMEstimatedRTEGDate <- as.Date(pids$DMEstimatedRTEGDate, format = "%m/%d/%Y")
  pids$DemandCreatedDate<- as.Date(pids$DemandCreatedDate, format = "%m/%d/%Y")
  pids$ProjectCreationDate<- as.Date(pids$ProjectCreationDate, format = "%m/%d/%Y")
  pids$ReceivingDate<- as.Date(pids$ReceivingDate, format = "%m/%d/%Y")
  pids$WorkOrderActualDockDate<- as.Date(pids$WorkOrderActualDockDate, format = "%m/%d/%Y")
  pids$POCreatedDate<- as.Date(pids$POCreatedDate, format = "%m/%d/%Y")
  pids$POApprovedDate<- as.Date(pids$POApprovedDate, format = "%m/%d/%Y")
  
  ##remove dots in header names in pids table
  pidsnames <- gsub("\\.","",names(pids))
  colnames(pids) <- c(pidsnames)
  
  ##extract for desired EG
  pids3<-pids[which(pids$EG %in% EGList),]
  
  ##select desired variables
  pids5<-subset(pids3, select = c("DeliveryNumber"
                                    ,"EG"
                                    ,"PropertyGroup"
                                    ,"DPM"
                                    ,"ProjectTitle" 
                                    ,"DeploymentClass"
                                    ,"ProjectCategory"  
                                    ,"DemandCreatedDate"
                                    ,"ProjectCreationDate"
                                    ,"RTEGActualDeliveryDate"
                                    ,"POCreatedDate"
                                    ,"POApprovedDate" 
                                    ,"WorkOrderActualDockDate"
                                    ,"ReceivingDate"
                                    ,"DMEstimatedRTEGDate"))
  
  
  ##extract Von's pids based on Von's pre-set variables
  pidsvon<-pids5[which(pids5$EG %in% voneg),]
  pidsvon3<-pidsvon[which(pidsvon$PropertyGroup %in% vonpg),]
  pidsvon5<-pidsvon3[which(pidsvon3$DeploymentClass %in% desiredDeploymentClass),]
  pidsvon7<-pidsvon5[which(pidsvon5$ProjectCategory %in% desiredProjectCategory),]

  
  
  
}