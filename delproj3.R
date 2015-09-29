delproj3<-function(file1, file2){
  
  ###################
  ##
  ##This script is designed to count On-Time projections for the current month and next month
  ##Use in Vish's Tuesday meetings at 1pm
  ##File1 should be the Delivered Capacity report from DERA, exported as CSV.
  ##File2 should be the Delivery Pipeline report from DERA, exported as CSV.
  ##
  ## V3 has a fix for scenarios when the DM Est is greater than the Comm RTEG, the count_towards_month should be set to the DM_Est, starting 
  ## on line 68
  ##
  ###################
  
  library(dplyr)
  library(lubridate)
  
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
  
  ##Read files, create dataframe with only desired variables
  ##dat <- read.csv("MCIOdata/All/DelCap-5-31-alltypes.csv", stringsAsFactors = FALSE)
  ##dot <- read.csv("MCIOdata/All/DelPipe-5-31-alltypes.csv", stringsAsFactors = FALSE)
  dat <- read.csv(file1, stringsAsFactors = FALSE)
  dot <- read.csv(file2, stringsAsFactors = FALSE)
  
  ##Create month column
  dat2<-subset(dat,select=c("EG","DeliveryNumber","ProjectTitle","RTEGOTDF","RTEGActualDeliveryDate"))
  dat2[,5]<-as.character(strptime(dat2[,5],"%m/%d/%Y"))
  dat3<-tbl_df(dat2)
  dat4<-mutate(dat3, count_towards_month = format(ymd(RTEGActualDeliveryDate),"%b"), DM_Estimated_RTEG_Date = 0, CommittedDeliveryDate = 0 ) ##add column for Actual RTEG Month
  
  
  dot2<-subset(dot, select=c("EG1","DeliveryNumber","ProjectTitle","DM_Estimated_RTEG_Date","CommittedDeliveryDate"))
  dot2[,5]<-as.character(strptime(dot2[,5],"%m/%d/%Y"))  ##convert date to  POSIXct
  dot2[,4]<-as.character(strptime(dot2[,4],"%m/%d/%Y"))  ##convert date into POSIXct
  ##dot2[,8]<-mdy(dot2$CommittedDeliveryDate)   convert date into POSIXct  
  ##dot2[,7]<-mdy(dot2$DM_Estimated_RTEG_Date)  convert date into POSIXct 

  dot3<-tbl_df(dot2)
  dot4<-mutate(dot3, EG = EG1, RTEGOTDF = 0, count_towards_month = 0, RTEGActualDeliveryDate = 0)
  
  ##align columns then merge the two tables
  dat5<-select(dat4, EG, DeliveryNumber, ProjectTitle, RTEGOTDF,RTEGActualDeliveryDate, DM_Estimated_RTEG_Date,CommittedDeliveryDate, count_towards_month)
  dot5<-select(dot4, EG, DeliveryNumber, ProjectTitle, RTEGOTDF,RTEGActualDeliveryDate, DM_Estimated_RTEG_Date,CommittedDeliveryDate, count_towards_month)
  new_table<-rbind(dat5, dot5)
  new_table2<-new_table[with(new_table,!is.na(DeliveryNumber)),]
  new_table2$DM_Estimated_RTEG_Date[which(new_table2$DM_Estimated_RTEG_Date == 0)] <- NA ##convert all zero to NA, prevents parsing failures
  new_table2$CommittedDeliveryDate[which(new_table2$CommittedDeliveryDate == 0)] <- NA ##convert all zero to NA, prevents parsing failures
  new_table2<-mutate(new_table2, DM_and_Committed_set = 0)
  ##new_table2[,6]<-mdy(new_table2$DM_Estimated_RTEG_Date)
  ##new_table2[,7]<-mdy(new_table2$CommittedDeliveryDate)
  
  
  ##Calculate month that each line item counts towards
  for(i in 1:nrow(new_table2)){
    if(!is.na(new_table2[i,7])){   ##is the Committed RTEG set
     new_table2[i,8]<-format(ymd(new_table2[i,7]),"%b")     ##then set count_towards_month based on Committed RTEG Date
  }
  else if(!is.na(new_table2[i,6])){   ##is DM EStimated RTEG set
    new_table2[i,8]<-format(ymd(new_table2[i,6]), "%b")     ##then set count_towards_month based on DM Estimated RTEG Date
   }            
  }

  ##Calculate count_towards_month for instances where DM_Est_RTEG is greater than Committed RTEG
  for(i in 1:nrow(new_table2)){
    if(!is.na(new_table2[i,6])&&!is.na(new_table2[i,7]))
      new_table2[i,9]<-1
  }
  
  ##if DM_Est is greater than Committed RTEG, set count towars month to DM Est  
  for(i in 1:nrow(new_table2)){
    DM_Est<-NA
    Commit_RTEG<-NA
    if(new_table2[i,9]==1){
      DM_Est<-ymd(new_table2[i,6])
      Commit_RTEG<-ymd(new_table2[i,7])
      if(DM_Est > Commit_RTEG) new_table2[i,8]<-format(ymd(new_table2[i,6]), "%b")  ##set count_towards_month
    }
  }
  
  ##Set PID count
  new_table3<-mutate(new_table2, OTCount = 0, PIDCount = 1)  ##add columns for OTCount and PIDCount
  
  ##Set OT Count for rows with RTEGOTDF values and value=="Yes"
  for(i in 1:nrow(new_table3)){
    if(!is.na(new_table3[i,4]) && new_table3[i,4]=="Yes") new_table3[i,10]<-1     ##if OTDF="Yes then mark as OT
  }
  

  ##Set OT Count for rows with Committed=Null and DM Est RTEG Date is set.  Assume team can still deliver On-Time.
  for(i in 1:nrow(new_table3)){
    DM_Est<-NA
    if(is.na(new_table3[i,7]) && !is.na(new_table3[i,6])) {
      DM_Est<-ymd(new_table3[i,6])
      if(DM_Est > CurrentDay )
        new_table3[i,10]<-1
    } 
    DM_Est<-NA
  }
  
  ##if DM_Est is greater than Committed RTEG, set count towars month to DM Est  
  for(i in 1:nrow(new_table3)){
    DM_Est<-NA
    Commit_RTEG<-NA
    if(new_table3[i,9]==1){
      DM_Est<-ymd(new_table3[i,6])
      Commit_RTEG<-ymd(new_table3[i,7])
      if(DM_Est <= Commit_RTEG) new_table3[i,10]<-1  ##set OT count
    }
  }
  
  
  
  new_table4<-new_table3 %>% 
    group_by(EG, count_towards_month) %>% 
    summarize(sumOT=sum(OTCount),sumPIDCount=sum(PIDCount),OT_Projection_for_Month=sumOT/sumPIDCount) %>% 
    arrange(EG, count_towards_month)
  

  View(new_table4)

  View(new_table3)
  write.csv(new_table3,file="MCIOdata/All/ouput.csv")
  write.csv(new_table4,file="MCIOdata/All/output_summary.csv")
}