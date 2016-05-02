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
  
  ##define fiscal year
  fy16<-c("2015-07","2015-08","2015-09","2015-10","2015-11","2015-12",
          "2016-01","2016-02","2016-03","2016-04","2016-05","2016-06")
  
  
  #remove rows where EG is NULL
  pids2<-pids[which(!is.na(pids$EG)),]
  
  ##create month column so we can evaluate fiscal year month
  fymonth_table<-tbl_df(pids2)
  fymonth_table2<-mutate(fymonth_table, fymonth = format(fymonth_table$RTEGActualDeliveryDate, format = "%Y-%m")) ##default on RTEGActualDeliveryDate
  for (i in 1:nrow(fymonth_table2)){
    if(!is.na(fymonth_table2[i,11]) & fymonth_table2[i,22]=="Active") {fymonth_table2[i,49]<-format(fymonth_table2[i,11], format = "%Y-%m") ##set fymonth to Committed Delivery Date
    }else if(!is.na(fymonth_table2[i,10]) & fymonth_table2[i,22]=="Active") fymonth_table2[i,49]<-format(fymonth_table2[i,10], format = "%Y-%m")  ##set fymonth to DM Est RTEG Date
  }
  fymonth_table4<-fymonth_table2[which(fymonth_table2$fymonth %in% fy16),]
  
  
  
  ##Create month column
  pids3<-subset(fymonth_table4,select=c("EG","DeliveryNumber","ProjectTitle","ProjectCategory","RTEGOTDF","RTEGActualDeliveryDate","DMEstimatedRTEGDate","CommittedDeliveryDate"))
  pids4<-tbl_df(pids3)
  pids5<-mutate(pids4,count_towards_month = format(ymd(RTEGActualDeliveryDate),"%b")) ##takes actual RTEG date as value for count_towards_month by default
  
  ##Set count_towards_month based on DM Est RTEG Date
  for(i in 1:nrow(pids5)){
    if(pids5[i,5]=="Active"){ ##RTEGOTDF="Active" means RTEGActualDeliveryDate is NULL and DMEstRTEG should not be NULL
      pids5[i,9]<-format(pids5[i,7],"%b")  ##use DM Est RTEG to set count towards month
    } 
  }
  
  
  ##Calculate count_towards_month for instances where DM_Est_RTEG is greater than Committed RTEG
  for(i in 1:nrow(pids5)){
    if(pids5[i,5]=="Active"&&!is.na(pids5[i,7])&&!is.na(pids5[i,8])&&(pids5[i,7] > pids5[i,8])){
      pids5[i,9]<-format(pids5[i,7],"%b") ##set count_towards_month 
    }
  }
  
  ##Command line code to check SPO pids
  ##pidsspo<-pids5[which(pids5$EG=="O365 SharePoint"),]
  ##pidsspo2<-pidsspo[which(is.na(pidsspo$RTEGActualDeliveryDate)),]
  
  ##Calculate count_towards_month for instances where DM_Est_RTEG is NULL and Committed RTEG is not NULL
  for(i in 1:nrow(pids5)){
    if(pids5[i,5]=="Active"&&is.na(pids5[i,7])&&!is.na(pids5[i,8]))
      pids5[i,9]<-format(pids5[i,8],"%b")
  }
  
  ##Set PID count
  pids7<-mutate(pids5, OTCount = 0, PIDCount = 1)  ##add columns for OTCount and PIDCount
  
  ##Set OT Count for rows with RTEGOTDF values and value=="Yes"
  for(i in 1:nrow(pids7)){
    if(pids7[i,5]=="Yes") pids7[i,10]<-1     ##if OTDF="Yes then mark as OT
  }
  
  ##Set OT Count for rows with Committed=Null and DM Est RTEG Date is set.  Assume team can still deliver On-Time.
  for(i in 1:nrow(pids7)){
    DM_Est<-NA
    if(pids7[i,5]=="Active" & !is.na(pids7[i,7]) & is.na(pids7[i,8])) {  ##Committed RTEG is NULL, DM Est is set
      DM_Est<-pids7[i,7]
      if(DM_Est > CurrentDay) pids7[i,10]<-1
    } 
    DM_Est<-NA
  }
  
  pids9<-pids7 %>% 
    group_by(EG, count_towards_month) %>% 
    summarize(sumOT=sum(OTCount),sumPIDCount=sum(PIDCount),OT_Projection_for_Month=sumOT/sumPIDCount) %>% 
    arrange(EG, count_towards_month)

  write.csv(pids7,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/predictive_OT_ouput.csv")
  write.csv(pids9,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/predictive_OT_output_summary.csv")
  
  
}