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
  
  ##setup special variables
  ##go_locals<-c("457404","457405","458216","458217","458967","458968","458964","458965","458966")
  go_locals<-c("458216","458217")
  
  
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
  ,w.woadDock
  ,w.RequestedDeliveryDate
  ,w.DMEstimatedRTEGDate
  ,w.CommittedDeliveryDate
  ,w.RTEGActualDeliveryDate
  ,w.DataCenter

  FROM spopairs p
  LEFT JOIN pids7 w
  ON p.DeliveryNumber = w.DeliveryNumber"
  
  pids9 <- sqldf(SQLQuery1)
  
  ##convert dates to date format for pids table
  pids9$RTEGActualDeliveryDate <- as.Date(pids9$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  pids9$woadDock <- as.Date(pids9$woadDock, format = "%m/%d/%Y")
  pids9$DMEstimatedRTEGDate <- as.Date(pids9$DMEstimatedRTEGDate, format = "%m/%d/%Y")
  pids9$RequestedDeliveryDate <- as.Date(pids9$RequestedDeliveryDate, format = "%m/%d/%Y")
  pids9$CommittedDeliveryDate <- as.Date(pids9$CommittedDeliveryDate, format = "%m/%d/%Y")
  pids9$RequestedDeliveryDate <- as.Date(pids9$RequestedDeliveryDate, format = "%m/%d/%Y")
  
  ##subset to just the PIDs in the pairing list
  pids11<-pids9[which(!is.na(pids9$Pair)),]
  
  ##format dataframe for output - Region variable and RTEG variable
  pids13<-mutate(pids11, RTEG = as.Date(NA), Status = " ")
  for(i in 1:nrow(pids13)){
    if(is.na(pids13[i,]$RTEGActualDeliveryDate)) 
      pids13[i,]$RTEG<-as.Date(pids13[i,]$DMEstimatedRTEGDate,format = "%m/%d/%Y")
    else pids13[i,]$RTEG<-as.Date(pids13[i,]$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  }
  
  ##calculate Live date and RTEG month
  pids15<-mutate(pids13, Live = RTEG + 15, crteg_month = format(CommittedDeliveryDate, "%Y-%m"), 
                 dm_rteg_month = format(DMEstimatedRTEGDate,"%Y-%m"))
  
  ##calculate correct Live date for go_locals
  for(i in 1:nrow(pids15)){
    if(pids15[i,]$DeliveryNumber %in% go_locals)
      pids15[i,]$Live<-pids15[i,]$RTEG+30
  }
  
  pids17<-subset(pids15,select = c("fiscalyear","DeliveryNumber",
                                   "Region","Pair","Status","Intent","RequestedDeliveryDate","RTEG","Live","CommittedDeliveryDate",
                                   "crteg_month","dm_rteg_month","woadDock","DataCenter"))
  pids19<-arrange(pids17,fiscalyear,Pair,RTEG)
                 

  ##print output
  write.csv(pids19,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/output_cluster_pairs.csv")
  
  
}
