requestedrteg<-function(EGtype, filetype1){
  ########################
  ##
  ##  purpose: to build a report on performance against Requested Delivery Date (RTEG)
  ##
  ########################
  
  library(ggplot2)
  library(plyr)
  library(sqldf)
  library(scales)
  library(reshape2)
  library(lubridate)
  
  
  ##set the path to DeploymentPerformance file
  ##path <- paste0("C:/Users/answami/Documents",
  ##               "/WindowsPowerShell/Scripts/Deployments")
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data")
  
  
  ##define the deloyments file
  file1 <- "DeliveryPerformance.csv"
  ##define Azure deliveries file
  file2 <- "AzureDeliveries.csv"
  ##define the milestone file
  file3 <- "MilestonePerformance.csv"
  ##define the milestone sequence file
  file4 <- "MilestoneSeq.csv"
  ##define SPO waves
  file5 <- "SPO Waves.csv"
  
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
  
  
  
  
  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  
  ## read the Azure deliveries file
  ##azurepids <- read.csv(file_loc2, 
  ##                      header = TRUE, colClasses = NA, na.strings = "#N/A")
  
  ## read the Milestone CT file
  
  ##milestones <- read.csv(file_loc3, 
  ##                       header = TRUE, colClasses = NA, na.strings = "#N/A")
  
  ## read the Milestone Seq file
  ##milestone_seq <- read.csv(file_loc4, 
  ##                          header = TRUE, colClasses = NA, na.strings = "#N/A")
  
  #select only some fields for azurepids
  ##azurepids <- azurepids[,which(grepl("SC|Deploy|Plan", names(azurepids)) == TRUE)]
  
  ## convert PID# number to character
  ##azurepids$Deploy_GFSDProjectID = as.character(azurepids$Deploy_GFSDProjectID)
  
  ## read the waves table
  spowaves <- read.csv(file_loc5,
                       header = TRUE, colClasses = NA, na.strings = "N/A", stringsAsFactors = TRUE)
  
  ##convert dates to date format
  pids$RTEGActualDeliveryDate <- as.Date(pids$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  pids$ReceivingDate <- as.Date(pids$ReceivingDate, format = "%m/%d/%Y")
  pids$woadDock <- as.Date(pids$woadDock, format = "%m/%d/%Y")
  pids$DM.Estimated.RTEG.Date <- as.Date(pids$DM.Estimated.RTEG.Date, format = "%m/%d/%Y")
  pids$RequestedDeliveryDate <- as.Date(pids$RequestedDeliveryDate, format = "%m/%d/%Y")
  pids$ProjectCreationDate <- as.Date(pids$ProjectCreationDate, format = "%m/%d/%Y")
  pids$PO.Confirmed.Dock.Date <- as.Date(pids$PO.Confirmed.Dock.Date, format = "%m/%d/%Y")
  pids$Current.Committed.Dock.Date <- as.Date(pids$Current.Committed.Dock.Date, format = "%m/%d/%Y")
  
  ##milestones$woadDock <- as.Date(milestones$woadDock, format = "%m/%d/%Y")
  
  ##remove dots in header names
  pidsnames <- gsub("\\.","",names(pids))
  colnames(pids) <- c(pidsnames)
  
  ##join the merge table with the pids table
  SQLQuery1 <- "SELECT p.DeliveryNumber
  ,p.ProjectTitle
  ,p.RTEGActualDeliveryDate
  ,p.EG
  ,p.ProjectCategory
  ,p.DemandCreatedDate
  ,p.ProjectCreationDate
  ,p.POConfirmedDockDate
  ,p.CurrentCommittedDockDate
  ,p.RequestedDeliveryDate
  ,p.DMEstimatedRTEGDate
  ,w.WaveCategory
  FROM pids p
  LEFT JOIN spowaves w 
  ON p.DeliveryNumber = w.DeliveryNumber"
  
  pids2 <- sqldf(SQLQuery1)
  

  
  ##create column for performance to RRTEG
  pids3 <- mutate(pids2, PerformanceToRequestedRTEG = DMEstimatedRTEGDate - RequestedDeliveryDate)
  
  ##create table report of all PIDs
  pids4 <- subset(pids3, select = c("EG", 
                                    "DeliveryNumber",
                                    "ProjectTitle",
                                    "ProjectCategory",
                                    "ProjectCreationDate",
                                    "CurrentCommittedDockDate",
                                    "RequestedDeliveryDate",
                                    "DMEstimatedRTEGDate",
                                    "PerformanceToRequestedRTEG",
                                    "WaveCategory"))
  
  ##select only the desired categories and EG
  pids5<-pids4[which(pids4$ProjectCategory=="PRD"),]
  pids6<-pids5[which(pids5$EG=="O365 SharePoint"),]
  
  ##print output file
  write.csv(pids6,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/ouput_rrteg_report.csv")
  
  
}

