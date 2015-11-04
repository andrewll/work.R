requestedrteg<-function(){
  ########################
  ##
  ##  purpose: to build a report on performance against Requested Delivery Date (RTEG)
  ##  Provide the report in the weekly status meeting with SPO
  ##  Measure how we're doing against the Requested RTEG, categorize according to the SPO waves
  ##
  ##  For delivered pids, calculate the actual dock-to-RTEG
  ##  For active pids, calculate the dock-to-RTEG based on CCDT and current date
  ##
  ########################
  
  library(ggplot2)
  library(dplyr)
  library(sqldf)
  library(scales)
  library(reshape2)
  library(lubridate)
  
  
  ##set the path to DeploymentPerformance file
  ##path <- paste0("C:/Users/answami/Documents",
  ##               "/WindowsPowerShell/Scripts/Deployments")
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")
  
  
  ##define the deloyments file
  file1 <- "DeliveryPerformance.csv"
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
  
  ## read the Delivery Pipeline file
  ##delpipe <- read.csv(file6, 
  ##                    header = TRUE, colClasses = NA, na.strings = "N/A", stringsAsFactors = TRUE)
  
  ##convert dates to date format for pids table
  pids$RTEGActualDeliveryDate <- as.Date(pids$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  pids$ReceivingDate <- as.Date(pids$ReceivingDate, format = "%m/%d/%Y")
  pids$woadDock <- as.Date(pids$woadDock, format = "%m/%d/%Y")
  pids$DM.Estimated.RTEG.Date <- as.Date(pids$DM.Estimated.RTEG.Date, format = "%m/%d/%Y")
  pids$RequestedDeliveryDate <- as.Date(pids$RequestedDeliveryDate, format = "%m/%d/%Y")
  pids$ProjectCreationDate <- as.Date(pids$ProjectCreationDate, format = "%m/%d/%Y")
  pids$PO.Confirmed.Dock.Date <- as.Date(pids$PO.Confirmed.Dock.Date, format = "%m/%d/%Y")
  pids$Current.Committed.Dock.Date <- as.Date(pids$Current.Committed.Dock.Date, format = "%m/%d/%Y")
  pids$rtegActualMonth <- as.Date(pids$rtegActualMonth, format = "%m/%d/%Y")
  
  ##convert dates to date format for delpipe table
  
  
  
  
  ##milestones$woadDock <- as.Date(milestones$woadDock, format = "%m/%d/%Y")
  
  ##remove dots in header names in pids table
  pidsnames <- gsub("\\.","",names(pids))
  colnames(pids) <- c(pidsnames)
  
  spowavenames <-gsub("\\.","",names(spowaves))
  colnames(spowaves) <-c(spowavenames)
  
  ##remove underscores in header names in delpipe table
  ##delpipenames <- gsub("\\_","",names(delpipe))
  ##colnames(delpipe) <- c(delpipenames)
  
  ##match delpipe names with pids names
  ##delpipe2 <- tbl_df(delpipe)
  ##delpipe3 <- mutate(delpipe2, EG = EG1)
  
  ##select only the colummsn to merge with the delpipe table
  ##pids2 <- select(.pids, EG, DeliveryNumber, ProjectTitle, DataCenter, ProjectCategory, RequestedDeliveryDate, DMEstimatedRTEGDate)
  
  ##join the merge table with the pids table
  SQLQuery1 <- "SELECT p.DeliveryNumber
  ,p.ProjectTitle
  ,p.RTEGActualDeliveryDate
  ,p.EG
  ,p.ProjectCategory
  ,p.DeploymentClass
  ,p.DemandCreatedDate
  ,p.ProjectCreationDate
  ,p.DTR
  ,p.ReceivingDate
  ,p.POConfirmedDockDate
  ,p.CurrentCommittedDockDate
  ,p.RequestedDeliveryDate
  ,p.rtegActualMonth
  ,p.DMEstimatedRTEGDate
  ,w.WaveCategory
  FROM pids p
  LEFT JOIN spowaves w 
  ON p.DeliveryNumber = w.DeliveryNumber"
  
  pids2 <- sqldf(SQLQuery1)
  
  ##extract active pids where CurrentCommittedDockDate is not NULL
  activepids<-pids2[which(is.na(pids$RTEGActualDeliveryDate)),]
  activepids2<-activepids[which(!is.na(activepids$CurrentCommittedDockDate)),]
  
  ##calculate the DTR for active pids
  CurrentDay <- as.Date(today())
  activepids3<- activepids2 %>%
    mutate(activedtr = CurrentDay - CurrentCommittedDockDate)%>%
    arrange(desc(activedtr))
  
  ##create column for performance to RRTEG
  pids3 <- mutate(pids2, PerformanceToRequestedRTEG = DMEstimatedRTEGDate - RequestedDeliveryDate)
  
  ##Add a monthname column
  pids3$rtegmonthname <- format(pids3$rtegActualMonth, format = "%Y-%m")
  
  ##create table report of all PIDs
  pids4 <- subset(pids3, select = c("EG", 
                                    "DeliveryNumber",
                                    "ProjectTitle",
                                    "ProjectCategory",
                                    "DeploymentClass",
                                    "ProjectCreationDate",
                                    "ReceivingDate",
                                    "POConfirmedDockDate",
                                    "CurrentCommittedDockDate",
                                    "RequestedDeliveryDate",
                                    "DMEstimatedRTEGDate",
                                    "PerformanceToRequestedRTEG",
                                    "rtegActualMonth",
                                    "rtegmonthname",
                                    "WaveCategory",
                                    "DTR"))
  
  ##select only the desired RTEG dates and EG. NA means the PID is still active
  quarteryear <- c("2015-07", "2015-08", "2015-09","2015-10",NA)
  
  ##take the time period in question and filter for just the desired EG
  pids5<-pids4[which(pids4$rtegmonthname %in% quarteryear),]
  pids6<-pids5[which(pids5$EG=="O365 SharePoint"),]
    
  ##label active pids and delivered pids by wave
  pids7<-mutate(pids6, DeliveryStatus = "Delivered")
  for(i in 1:nrow(pids7)){
    if(is.na(pids7[i,]$rtegActualMonth))  pids7[i,]$DeliveryStatus <- c("Active")
  }


  ##print output file for Delivered PIDs
  write.csv(pids7,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/ouput_rrteg_report_delivered_pids.csv")

  ##print output file for Active PIDs where Current Committed Dock Date is not NULL
  write.csv(activepids3,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/ouput_rrteg_report_active_pids.csv")

  ##calculate pidcount
  ##pidcount <- count(pids6, vars = c("EG", "as.factor(rtegmonthname)"))
  ##names(pidcount) <- c("EG", "rtegmonthname", "pidcount")
  
  
}

