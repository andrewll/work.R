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
  ##file1 <- "DeliveryProjectStatusReport.csv"
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
  
  ##set valriable for delivered Network PIDs
  nw_wave_category<-c("Wave 01 Network", "Wave 02 Network", "Wave 03 Network", "Wave 04 Network",
                      "Wave 05 Network", "Wave 06 Network", "Wave 07 Network", "Wave 08 Network", 
                      "Wave 8.5 Network", "Wave 09 Network", "Wave 10 Network", "Wave 11 Network")
  
  ##set valriable for delivered Server PIDs
  svr_wave_category<-c("Wave 01 Server", "Wave 02 Server", "Wave 03 Server", "Wave 04 Server",
                       "Wave 05 Server", "Wave 06 Server", "Wave 07 Server", "Wave 08 Server", 
                       "Wave 8.5 Server", "Wave 09 Server", "Wave 10 Server", "Wave 11 Server")
  
  
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
  ,p.DataCenter
  ,p.ProjectCategory
  ,p.DeploymentClass
  ,p.PreRackCount
  ,p.DemandCreatedDate
  ,p.ProjectCreationDate
  ,p.DTR
  ,p.ReceivingDate
  ,p.POConfirmedDockDate
  ,p.CurrentCommittedDockDate
  ,p.RequestedDeliveryDate
  ,p.rtegActualMonth
  ,p.DMEstimatedRTEGDate
  ,p.CommittedDeliveryDate
  ,w.WaveCategory
  FROM pids p
  LEFT JOIN spowaves w 
  ON p.DeliveryNumber = w.DeliveryNumber"
  
  pids2 <- sqldf(SQLQuery1)
  
  ##extract active pids where CurrentCommittedDockDate is not NULL
  activepids<-pids2[which(is.na(pids$RTEGActualDeliveryDate)),]
  activepids2<-activepids[which(!is.na(activepids$CurrentCommittedDockDate)),]
  activepids3<-activepids2[which(activepids2$EG=="O365 SharePoint"),]
  
  
  ##calculate the DTR for active pids
  CurrentDay <- as.Date(today())
  activepids5<- activepids3 %>%
    mutate(activedtr = CurrentDay - CurrentCommittedDockDate)%>%
    arrange(desc(activedtr))
  
  ##create column for performance to RRTEG
  pids3 <- mutate(pids2, PerformanceToRequestedRTEG = DMEstimatedRTEGDate - RequestedDeliveryDate, PIDcreateToRTEG = RTEGActualDeliveryDate-ProjectCreationDate)
  
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
                                    "DataCenter",
                                    "PreRackCount",
                                    "POConfirmedDockDate",
                                    "CurrentCommittedDockDate",
                                    "CommittedDeliveryDate", 
                                    "RequestedDeliveryDate",
                                    "DMEstimatedRTEGDate",
                                    "PerformanceToRequestedRTEG",
                                    "RTEGActualDeliveryDate",
                                    "rtegActualMonth",
                                    "rtegmonthname",
                                    "PIDcreateToRTEG",
                                    "WaveCategory",
                                    "DTR"))
  
  ##select only the desired RTEG dates and EG. NA means the PID is still active
  ##quarteryear <- c("2015-07", "2015-08", "2015-09","2015-10","2015-11", "2015-12")
  
  ##take the time period in question and filter for just the desired EG
  ##pids5<-pids4[which(pids4$rtegmonthname %in% quarteryear),]
  pids6<-pids4[which(pids4$EG=="O365 SharePoint"),]
    
  ##label active pids and delivered pids by wave
  pids7<-mutate(pids6, DeliveryStatus = "Delivered")
  for(i in 1:nrow(pids7)){
    if(is.na(pids7[i,]$rtegActualMonth))  pids7[i,]$DeliveryStatus <- c("Active")
  }
  
  ##subset on the network pids
  pids9 <- pids7[which(pids7$WaveCategory %in% nw_wave_category),]
  pids11 <- pids9[which(pids9$DeliveryStatus=="Delivered"),]
  

  ##print output file for Delivered PIDs
  write.csv(pids7,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/ouput_rrteg_report_delivered_pids.csv")

  ##print output file for Active PIDs where Current Committed Dock Date is not NULL
  write.csv(activepids5,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/ouput_rrteg_report_active_pids.csv")

  ##calculate pidcount
  ##pidcount <- count(pids6, vars = c("EG", "as.factor(rtegmonthname)"))
  ##names(pidcount) <- c("EG", "rtegmonthname", "pidcount")
  
  ##subset on the network pids
  pids9 <- pids7[which(pids7$WaveCategory %in% nw_wave_category),]
  pids11 <- pids9[which(pids9$DeliveryStatus=="Delivered"),]
  
  ##chart boxplots of network pids - cycle time for PID lifecycle
  png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_boxplot_network.png", width = 960, height = 480, units = "px")
  g<-ggplot(pids11, aes(WaveCategory, PIDcreateToRTEG))
  g + geom_boxplot()
  dev.off()
  
  ##subset on server pids
  pids13<-pids7[which(pids7$WaveCategory %in% svr_wave_category),]
  pids15<-pids13[which(pids13$DeliveryStatus=="Delivered"),]
  
  ##chart boxplots of server pids - cycle time for PID lifecycle
  png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_boxplot_servers.png", width = 960, height = 480, units = "px")
  g<-ggplot(pids15, aes(WaveCategory, PIDcreateToRTEG))
  g + geom_boxplot()
  dev.off()
  
  
  
  
}

