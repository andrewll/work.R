nonapct<-function(){
  
  #################
  ##
  ##  Purpose: calculate cycle time for 5 major phases: DemandCreate-to-PIDCreate, PIDCreate-to-POCreate, POCreate-to-POApprove, POApprove-to-Dock, Dock-to-RTEG
  ##  This is a derivative of both the simulaterteg script and the cycletime script.
  ##
  ##  This script is a derivative of cycletime2 and apct
  ##
  ##  
  ##
  #################
  
  ##load the libraries
  library(ggplot2)
  library(dplyr)
  library(sqldf)
  library(scales)
  library(reshape2)
  library(lubridate)
  
  ##set the EG variable
  EGList <- c("O365 Exchange", "AP", "O365 SharePoint","CRM","XBOX","ISSD (Azure AAD)")
  ##EGList <- c("O365 Exchange","O365 SharePoint")
  ##EGList <- c("O365 Exchange")
  ##EGList <- c("Networking")
  ##EGList <- c("AP", "Not Set", "Default", "NonWebComm")
  
  ##set DeploymentClass and ProjectCategory variables
  desiredDeploymentClass<-c("New Deployment")
  desiredProjectCategory<-c("PRD","Discrete","Network","SAN","ITPAC")
  
  ##set variables
  voneg<-c("AP")
  vonpg<-c("Azure ECN", "Search")
  lorindaeg<-c("AP","Not Set","Default","NonWebComm")
  lorindapg<-c("Cosmos","Search","TechOps", "Visual Studio Cloud Services (QBuild)", "Sustained Engineering", "OneDrive")
  sandeepeg<-c("XBOX","ISSD (Azure AAD)")
  andreweg<-c("O365 SharePoint")
  chandraeg<-c("O365 Exchange")
  
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
  
  ##filter on deploymentclass and projectcategory
  pids1<-pids[which(pids$DeploymentClass %in% desiredDeploymentClass),]
  pids2<-pids1[which(pids1$ProjectCategory %in% desiredProjectCategory),]
  
  ##filter for PIDs in current fiscal year
  pids3<-pids2[which(pids2$RTEGActualDeliveryDate>'2015-06-30'),]
  
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
                                  ,"DMEstimatedRTEGDate"
                                  ,"DockToRTEG"))
  
  ##calculate year_delivered and month_delivered
  pids7 <- mutate(pids5, Year_Delivered = format(RTEGActualDeliveryDate,"%Y"), 
                  Month_Delivered = format(RTEGActualDeliveryDate, "%Y-%m"))
  
  ##extract Von's pids based on Von's pre-set variables
  pidsvon<-pids7[which(pids5$EG %in% voneg),]
  pidsvon3<-pidsvon[which(pidsvon$PropertyGroup %in% vonpg),]

  ##extract Lorinda's pids based on Lorinda's pre-set variables
  pidslorinda<-pids7[which(pids5$EG %in% lorindaeg),]
  pidslorinda3<-pidslorinda[which(pidslorinda$PropertyGroup %in% lorindapg),]

  ##extract Sandeep's pids based on Sandeep's pre-set variables
  pidssandeep<-pids7[which(pids5$EG %in% sandeepeg),]

  ##extract Andrew's pids based on Andrew's pre-set variables
  pidsandrew<-pids7[which(pids7$EG %in% andreweg),]

  ##calculate cycle time numbers for Von
  pidsvon9 <- mutate(pidsvon3,
                      demandcreate_to_pidcreate = as.numeric(ProjectCreationDate - DemandCreatedDate),
                      pidcreate_to_pocreate = as.numeric(POCreatedDate - ProjectCreationDate), 
                      pocreate_to_poapprove = as.numeric(POApprovedDate - POCreatedDate),
                      poapprove_to_dock = as.numeric(WorkOrderActualDockDate - POApprovedDate),
                      dock_to_rteg = as.numeric(DockToRTEG))
  
  ##calculate cycle time numbers for Lorinda
  pidslorinda13 <- mutate(pidslorinda3, 
                          demandcreate_to_pidcreate = as.numeric(ProjectCreationDate - DemandCreatedDate),
                          pidcreate_to_pocreate = as.numeric(POCreatedDate - ProjectCreationDate), 
                          pocreate_to_poapprove = as.numeric(POApprovedDate - POCreatedDate),
                          poapprove_to_dock = as.numeric(WorkOrderActualDockDate - POApprovedDate),
                          dock_to_rteg = as.numeric(DockToRTEG))
  
  ##calculate cycle time numbers for Sandeep
  pidssandeep7 <- mutate(pidssandeep, 
                         demandcreate_to_pidcreate = as.numeric(ProjectCreationDate - DemandCreatedDate),
                         pidcreate_to_pocreate = as.numeric(POCreatedDate - ProjectCreationDate), 
                         pocreate_to_poapprove = as.numeric(POApprovedDate - POCreatedDate),
                         poapprove_to_dock = as.numeric(WorkOrderActualDockDate - POApprovedDate),
                         dock_to_rteg = as.numeric(DockToRTEG))
  
  
  ##calculate cycle time numbers for Andrew
  pidsandrew3 <- mutate(pidsandrew, Month_Delivered = format(RTEGActualDeliveryDate, "%Y-%m"),
                         demandcreate_to_pidcreate = as.numeric(ProjectCreationDate - DemandCreatedDate),
                         pidcreate_to_pocreate = as.numeric(POCreatedDate - ProjectCreationDate), 
                         pocreate_to_poapprove = as.numeric(POApprovedDate - POCreatedDate),
                         poapprove_to_dock = as.numeric(WorkOrderActualDockDate - POApprovedDate),
                         dock_to_rteg = as.numeric(DockToRTEG))
  
  
  
  
  
  ##output all dataframe
  write.csv(pidsvon9,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/pidsvon.csv")
  write.csv(pidslorinda13,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/pidslorinda.csv")
  write.csv(pidssandeep7,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/pidssandeep.csv")
  write.csv(pidsandrew3,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/pidsandrew.csv")
  
  
  
}