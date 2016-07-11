cycletime4<-function(){
  
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
  
  ##set the path to DeploymentPerformance file
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")
  ##define the deloyments file
  ##file1 <- "DeliveryPerformance.csv"
  file1 <- "DeliveryProjectStatusReport.csv"
  ##define Azure deliveries file
  file2 <- "AzureDeliveries.csv"
  ##define the milestone file
  file3 <- "MilestonePerformance.csv"
  ##define the milestone sequence file
  file4 <- "MilestoneSeq.csv"
  ##define SPO waves
  file5 <- "spowaves.csv"
  ##define Delivery Pipeline file
  ##file6 <- "C:/Users/andrewll/Documents/R/MCIOdata/All/DelPipe-10-5-alleg-networkandservers.csv"
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  ##define Azure deliveries file path
  file_loc2 <- file.path(path, file2)
  ##define the Milestone file path
  file_loc3 <- file.path(path, file3)
  ##define the Milestone sequence file path
  file_loc4 <- file.path(path, file4)
  ##define the spo waves file path
  file_loc5 <- file.path(path, file5)
  ##define the Delivery Pipeline path
  ##file_loc6 <- file.path(path, file6)
  
  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  
  ##change column name for duplicate variable
  pidsdup<-gsub("Current.Committed.Dock.Date","Current.Committed.Dock.Date.Dup",names(pids))
  colnames(pids) <- c(pidsdup)
  
  ## read the waves table
  spowaves <- read.csv(file_loc5,header = TRUE, colClasses = NA, na.strings = "N/A", stringsAsFactors = TRUE)
  
  ##remove dots in header names in pids table
  pidsnames <- gsub("\\.","",names(pids))
  colnames(pids) <- c(pidsnames)
  spowavenames <-gsub("\\.","",names(spowaves))
  colnames(spowaves) <-c(spowavenames)
  
  ##convert dates into correct format
  pids$RTEGActualDeliveryDate <- as.Date(pids$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  pids$DMEstimatedRTEGDate <- as.Date(pids$DMEstimatedRTEGDate, format = "%m/%d/%Y")
  pids$DemandCreatedDate<- as.Date(pids$DemandCreatedDate, format = "%m/%d/%Y")
  pids$ProjectCreationDate<- as.Date(pids$ProjectCreationDate, format = "%m/%d/%Y")
  pids$ReceivingDate<- as.Date(pids$ReceivingDate, format = "%m/%d/%Y")
  pids$WorkOrderActualDockDate<- as.Date(pids$WorkOrderActualDockDate, format = "%m/%d/%Y")
  pids$POCreatedDate<- as.Date(pids$POCreatedDate, format = "%m/%d/%Y")
  pids$POApprovedDate<- as.Date(pids$POApprovedDate, format = "%m/%d/%Y")
  
  

  ##join the merge table with the pids table
  SQLQuery1 <- "SELECT p.DeliveryNumber
  ,p.EG
  ,p.PropertyGroup
  ,p.DPM
  ,p.ProjectTitle
  ,p.DeploymentClass
  ,p.ProjectCategory
  ,p.DemandCreatedDate
  ,p.ProjectCreationDate
  ,p.RTEGActualDeliveryDate
  ,p.POCreatedDate
  ,p.POApprovedDate
  ,p.WorkOrderActualDockDate
  ,p.RequestedDeliveryDate
  ,p.ReceivingDate
  ,p.DMEstimatedRTEGDate
  ,p.DockToRTEG
  ,w.WaveCategory
  FROM pids p
  LEFT JOIN spowaves w 
  ON p.DeliveryNumber = w.DeliveryNumber"
  
  pids2 <- sqldf(SQLQuery1)
  
  ##convert Delivery Number to correct format
  pids2$DeliveryNumber<-as.character(pids2$DeliveryNumber)
  
  ##filter on deploymentclass and projectcategory
  pids4<-pids2[which(pids2$DeploymentClass %in% desiredDeploymentClass),]
  pids5<-pids4[which(pids4$ProjectCategory %in% desiredProjectCategory),]
  
  ##filter for PIDs in current fiscal year
  pids6<-pids5[which(pids5$RTEGActualDeliveryDate>'2015-06-30'),]
  
  ##extract Andrew's pids based on Andrew's pre-set variables
  pids7<-pids6[which(pids6$EG %in% andreweg),]
  
  ##calculate cycle time numbers for Andrew
  pids9 <- mutate(pids7, Year_Delivered = format(RTEGActualDeliveryDate,"%Y"), 
                  Month_Delivered = format(RTEGActualDeliveryDate, "%Y-%m"))
  
  ##pids11 <- mutate(pids9, demandcreate_to_pidcreate = as.numeric(ProjectCreationDate - DemandCreatedDate),
  ##                      pidcreate_to_pocreate = as.numeric(POCreatedDate - ProjectCreationDate), 
  ##                      pocreate_to_poapprove = as.numeric(POApprovedDate - POCreatedDate),
  ##                      poapprove_to_dock = as.numeric(WorkOrderActualDockDate - POApprovedDate),
  ##                      dock_to_rteg = as.numeric(DockToRTEG))
  
  pids11 <- mutate(pids9,pidcreate_to_pocreate = as.numeric(POCreatedDate - ProjectCreationDate),
                   poapprove_to_dock = as.numeric(WorkOrderActualDockDate - POCreatedDate),
                   dock_to_rteg = as.numeric(DockToRTEG))
  
  pids21 <- mutate(pids9, pid_to_rteg = RTEGActualDeliveryDate - ProjectCreationDate)
  
  ##Subset to just PRDs and Network pids
  pids13 <- pids11[which(pids11$ProjectCategory=="PRD"),]
  pids15 <- pids11[which(pids11$ProjectCategory=="Network"),]
  pids23 <- pids21[which(pids21$ProjectCategory=="PRD"),]
  pids25 <- pids21[which(pids21$ProjectCategory=="Network"),]
  
  ##output all dataframe
  ##write.csv(pidsvon9,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/pidsvon.csv")
  ##write.csv(pidslorinda13,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/pidslorinda.csv")
  ##write.csv(pidssandeep7,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/pidssandeep.csv")
  write.csv(pids13,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/SPO-PRD-CT-majormilestones.csv")
  write.csv(pids15,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/SPO-Network-CT-majormilestones.csv")
  write.csv(pids23,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/SPO-PRD-CT.csv")
  write.csv(pids25,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/SPO-Network-CT.csv")
  
  
  ##plot
  ##png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_boxplot_network_configverifymilestone.png", 
  ##    width = 960, height = 480, units = "px")
  ##g<-ggplot(pids13, aes(x=Month_Delivered, y=ConfigureVerifyNetworkValue))
  ##g+geom_boxplot()+labs(title="SPO Network PIDs Config&Verify Milestone Cycle Times by Month of RTEG", x="Month of RTEG", y="Cycle Time in days")
  ##dev.off()
  
  
}