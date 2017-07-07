afdrackcount<-function(){
  
  library(ggplot2)
  library(dplyr)
  library(sqldf)
  library(scales)
  library(reshape2)
  library(lubridate)
  
  # basic set up clear all existing variables 
  rm(list = ls(all=T))
  
  ##set the path to DeploymentPerformance file
  rteg_to_live_value <-as.integer("30")
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")
  
  ##define the deloyments file
  file1 <- "DeliveryPerformance.csv"
  ##define SPO pairs
  file5 <- "afdpidlist.csv"
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  ##define the afd list of PIDs
  file_loc5 <- file.path(path, file5)
  
  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  
  ## read the waves table
  afdlist <- read.csv(file_loc5,
                       header = TRUE, colClasses = NA, na.strings = "N/A", stringsAsFactors = TRUE)
  
  ##convert spopairs DeliveryNumber to character
  afdlist$DeliveryNumber<-as.character(afdlist$DeliveryNumber)
  pids$DeliveryNumber<-as.character(pids$DeliveryNumber)
  
  ##remove dots in header names in pids table
  pidsnames <- gsub("\\.","",names(pids))
  colnames(pids) <- c(pidsnames)
  
  afdlistnames <-gsub("\\.","",names(afdlist))
  colnames(afdlist) <-c(afdlistnames)
  
  pids3<-pids[which(pids$DeploymentClass=="New Deployment"),]
  pids5<-pids3[which(pids3$ProjectCategory=="PRD"),]
  pids7<-pids5[which(pids5$EG=="Azure Front Door"),]
  
  ##join the merge table with the pids table
  SQLQuery1 <- "SELECT p.DeliveryNumber
  ,w.ProjectTitle
  ,w.woadDock
  ,w.RequestedDeliveryDate
  ,w.DMEstimatedRTEGDate
  ,w.CommittedDeliveryDate
  ,w.RTEGActualDeliveryDate
  ,w.DataCenter
  
  FROM afdlist p
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
  
  ##subset to just the PIDs that are active
  pids11<-pids9[which(is.na(pids9$RTEGActualDeliveryDate)),]
  
  ##format dataframe for output - Region variable and RTEG variable
  pids13<-mutate(pids11, RTEG = as.Date(NA), Status = " ")
  for(i in 1:nrow(pids13)){
    if(is.na(pids13[i,]$RTEGActualDeliveryDate)) 
      pids13[i,]$RTEG<-as.Date(pids13[i,]$DMEstimatedRTEGDate,format = "%m/%d/%Y")
    else pids13[i,]$RTEG<-as.Date(pids13[i,]$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  }
  
  ##calculate Live date and RTEG month
  pids15<-mutate(pids13, Live = RTEG + rteg_to_live_value, crteg_month = format(CommittedDeliveryDate, "%Y-%m"), 
                 dm_rteg_month = format(DMEstimatedRTEGDate,"%Y-%m"))
  
  
  
}
