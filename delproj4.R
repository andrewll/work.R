delproj4<-function(){
  
  ###################
  ##
  ##This script is designed to count On-Time projections for the current month and next month
  ##Use in exec reviews
  ##Input file will be Delivery Performance report from DERA
  ##
  ##Include fix for scenarios when the DM Est is greater than the Comm RTEG, the count_towards_month should be set to the DM_Est, starting 
  ## on line 68
  ##
  ###################
  
  library(dplyr)
  library(lubridate)
  
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")
  
  ##define the deloyments file
  file1 <- "DeliveryPerformance.csv"
  ##file1 <- "DeliveryProjectStatusReport.csv"
  ##define SPO waves
  file2 <- "spowaves.csv"
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  ##define the spo waves file path
  file_loc2 <- file.path(path, file2)
  
  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  
  
  ##Calculate Current Month and Next Month
  CurrentMonth_real <- month(Sys.time())
  CurrentMonth<-CurrentMonth_real
  ##NextMonth<-CurrentMonth_real+1
  ##CurrentMonth <- CurrentMonth_real-1
  NextMonth <- CurrentMonth + 1
  CurrentDay <- ymd(today())
  
  message("This script assumes that calculations are needed for the current and next month.")
  message("The current month is ", CurrentMonth)
  print(format(Sys.Date(), "%B %Y"))
  message("and the next month is ", NextMonth)
  print(format(Sys.Date()+31, "%B %Y"))
  
  ##convert dates to date format for pids table
  pids$RTEGActualDeliveryDate <- as.Date(pids$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  pids$ReceivingDate <- as.Date(pids$ReceivingDate, format = "%m/%d/%Y")
  pids$woadDock <- as.Date(pids$woadDock, format = "%m/%d/%Y")
  pids$CommittedDeliveryDate <-as.Date(pids$CommittedDeliveryDate, format = "%m/%d/%Y")
  pids$DM.Estimated.RTEG.Date <- as.Date(pids$DM.Estimated.RTEG.Date, format = "%m/%d/%Y")
  pids$RequestedDeliveryDate <- as.Date(pids$RequestedDeliveryDate, format = "%m/%d/%Y")
  pids$ProjectCreationDate <- as.Date(pids$ProjectCreationDate, format = "%m/%d/%Y")
  pids$PO.Confirmed.Dock.Date <- as.Date(pids$PO.Confirmed.Dock.Date, format = "%m/%d/%Y")
  pids$Current.Committed.Dock.Date <- as.Date(pids$Current.Committed.Dock.Date, format = "%m/%d/%Y")
  pids$rtegActualMonth <- as.Date(pids$rtegActualMonth, format = "%m/%d/%Y")
  
  ##remove dots in header names in pids table
  pidsnames <- gsub("\\.","",names(pids))
  colnames(pids) <- c(pidsnames)
  
  #remove rows where EG is NULL
  pids2<-pids[which(!is.na(pids$EG)),]
  
  ##Create month column
  pids3<-subset(pids2,select=c("EG","DeliveryNumber","ProjectTitle","RTEGOTDF","RTEGActualDeliveryDate","DMEstimatedRTEGDate","CommittedDeliveryDate"))
  pids4<-tbl_df(pids3)
  pids5<-mutate(pids4,count_towards_month = format(ymd(RTEGActualDeliveryDate),"%b")) ##takes Committed RTEG as value for count_towards_month by default
  
  ##Set count_towards_month based on DM Est RTEG Date
  for(i in 1:nrow(pids5)){
    if(pids5[i,4]=="Active"){ ##RTEGOTDF="Active" means RTEGActualDeliveryDate is NULL and DMEstRTEG should not be NULL
      pids5[i,8]<-format(pids5[i,6],"%b")
    } 
  }
  
  
  ##Calculate count_towards_month for instances where DM_Est_RTEG is greater than Committed RTEG
  for(i in 1:nrow(pids5)){
    if(pids5[i,4]=="Active"&&!is.na(pids5[i,6])&&!is.na(pids5[i,7])){
      if(pids5[i,6] > pids5[i,7]) pids5[i,8]<-format(pids5[i,6],"%b") ##set count_towards_month 
    }
  }
  
  
  
  
  
}