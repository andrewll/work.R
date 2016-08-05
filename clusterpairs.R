clusterpair<-function(){
  
  library(ggplot2)
  library(dplyr)
  library(sqldf)
  library(scales)
  library(reshape2)
  library(lubridate)
  
  # basic set up clear all existing variables 
  rm(list = ls(all=T))
  
  ##set the path to DeploymentPerformance file
  ##path <- paste0("C:/Users/answami/Documents",
  ##               "/WindowsPowerShell/Scripts/Deployments")
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")
  
  
  ##define the deloyments file
  file1 <- "DeliveryPerformance.csv"
  ##define SPO pairs
  file5 <- "spo-pairing-fy16.csv"
  
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  ##define the spo pairs file path
  file_loc5 <- file.path(path, file5)

  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  
  ## read the waves table
  spopairs <- read.csv(file_loc5,
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
  pids$CommittedDeliveryDate <- as.Date(pids$CommittedDeliveryDate, format = "%m/%d/%Y")
  pids$RequestedDeliveryDate <- as.Date(pids$RequestedDeliveryDate, format = "%m/%d/%Y")
  
  ##convert spopairs DeliveryNumber to character
  spopairs$DeliveryNumber<-as.character(spopairs$DeliveryNumber)
  pids$DeliveryNumber<-as.character(pids$DeliveryNumber)
  
  ##remove dots in header names in pids table
  pidsnames <- gsub("\\.","",names(pids))
  colnames(pids) <- c(pidsnames)
  
  spopairnames <-gsub("\\.","",names(spopairs))
  colnames(spopairs) <-c(spopairnames)
  
  pids3<-pids[which(pids$DeploymentClass=="New Deployment"),]
  pids5<-pids3[which(pids3$ProjectCategory=="PRD"),]
  pids7<-pids5[which(pids5$EG=="O365 SharePoint"),]
  
  ##join the merge table with the pids table
  SQLQuery1 <- "SELECT p.DeliveryNumber
  ,p.Pair
  ,p.Region
  ,p.Intent
  ,p.fiscalyear
  ,w.ProjectTitle
  ,w.RequestedDeliveryDate
  ,w.DMEstimatedRTEGDate
  ,w.CommittedDeliveryDate
  ,w.RTEGActualDeliveryDate
  ,w.DataCenter

  FROM spopairs p
  LEFT JOIN pids7 w
  ON p.DeliveryNumber = w.DeliveryNumber"
  
  pids9 <- sqldf(SQLQuery1)
  
  ##subset to just the PIDs in the pairing list
  pids11<-pids9[which(!is.na(pids9$Pair)),]
  
  ##format dataframe for output - Region variable and RTEG variable
  pids13<-mutate(pids11, RTEG = as.Date(NA), Status = " ")
  for(i in 1:nrow(pids13)){
    if(is.na(pids13[i,]$RTEGActualDeliveryDate)) 
      pids13[i,]$RTEG<-as.Date(pids13[i,]$DMEstimatedRTEGDate,format = "%m/%d/%Y")
    else pids13[i,]$RTEG<-as.Date(pids13[i,]$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  }
  
  pids15<-mutate(pids13, Live = RTEG + 15, crteg_month = month(CommittedDeliveryDate, label = TRUE)) 
  
  pids17<-subset(pids15,select = c("fiscalyear","DeliveryNumber",
                                   "Region","Pair","Status","Intent","RequestedDeliveryDate","RTEG","Live","CommittedDeliveryDate","crteg_month"))
  pids19<-arrange(pids17,fiscalyear,Pair,RTEG)
                 

  ##print output
  write.csv(pids19,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/output_cluster_pairs.csv")
  
  
}
