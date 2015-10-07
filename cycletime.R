cycletime <- function(file1){
  
  #######
  ##
  ##this script calculates the following metrics for all EG: OrderConfirm-to-Dock, Dock-to-RTEG, OrderConfirm-to-RTEG
  ##For each metrics, it calculates the mean, median, and 95th percentile
  ##It takes a monthly calculation of the metrics
  ##
  ##Input file is the Delivered Capacity report from DERA
  ##
  #######
  
  
  ##setup and real input file
  library(dplyr)
  library(lubridate)
  ##mydf <- read.csv(file1, stringsAsFactors = FALSE)
  mydf<-read.csv("C:/Users/andrewll/Documents/R/MCIOdata/All/DelCap-Jul1-Sep30-alleg-networkandservers.csv", stringsAsFactors = FALSE)
  dat <- tbl_df(mydf)
  
  ##select desired columns from the dataframe and add some new ones for month and year delivered
  dat2 <- subset(dat, select=c("EG",
                               "DeliveryNumber",
                               "ProjectTitle",
                               "RTEGOTDF",
                               "RTEGActualDeliveryDate", 
                               "ProjectCategory", 
                               "OrderCOnfirmtoDock_Actual", 
                               "DTR1", 
                               "OrderCOnfirmtoRTEG_Actual2",
                               "ProjectCreationDate",
                               "woadPOCreation",
                               "woadDock2"))
  
  ## add calculated variables
  dat3 <- mutate(dat2, Month_Delivered = format(mdy(RTEGActualDeliveryDate),"%b"), 
                 Year_Delivered = format(mdy(RTEGActualDeliveryDate),"%Y"), 
                 PIDCount = 1, 
                 pidcreate = as.Date(ProjectCreationDate, format = "%m/%d/%Y"), 
                 pocreate = as.Date(woadPOCreation, format = "%m/%d/%Y"), 
                 dockdate = as.Date(woadDock2, format = "%m/%d/%Y"))
  
  dat4 <- mutate(dat3, pidcreate_to_pocreate = pocreate - pidcreate, 
                 pocreate_to_dock = dockdate - pocreate)
 
  ##summarize and create report table
  dat5<- dat4 %>% 
    group_by(EG, ProjectCategory, Year_Delivered, Month_Delivered) %>%
    ##summarize(PIDCount = sum(PIDCount), OrderCOnfirmtoDock_Actual_Avg = mean(OrderCOnfirmtoDock_Actual, na.rm=TRUE), OrderCOnfirmtoDock_Actual_Median = median(OrderCOnfirmtoDock_Actual, na.rm=TRUE), OrderCOnfirmtoDock_Actual_95th = quantile(OrderCOnfirmtoDock_Actual, .95, na.rm=TRUE), Dock_to_RTEG_Avg = mean(DTR1, na.rm=TRUE),  Dock_to_RTEG_Median = median(DTR1, na.rm=TRUE), Dock_to_RTEG_95th = quantile(DTR1, .95, na.rm=TRUE), OrderCOnfirmtoRTEG_Avg = mean(OrderCOnfirmtoRTEG_Actual2, na.rm=TRUE), OrderCOnfirmtoRTEG_Median = median(OrderCOnfirmtoRTEG_Actual2,na.rm=TRUE), OrderCOnfirmtoRTEG_95th = quantile(OrderCOnfirmtoRTEG_Actual2, .95,na.rm=TRUE)) %>%
    summarize(PIDCount = sum(PIDCount), 
              pidcreate_to_pocreate_avg = mean(pidcreate_to_pocreate, na.rm = TRUE), 
              pidcreate_to_pocreate_95th = quantile(pidcreate_to_pocreate, .95, na.rm = TRUE),
              pocreate_to_dock_avg = mean(pocreate_to_dock, na.rm = TRUE),
              pocreate_to_dock_95th = quantile(pocreate_to_dock, .95, na.rm = TRUE),
              Dock_to_RTEG_Avg = mean(DTR1, na.rm=TRUE),  
              Dock_to_RTEG_95th = quantile(DTR1, .95, na.rm=TRUE)) %>%
    arrange(EG, ProjectCategory, Year_Delivered, Month_Delivered)
  
  ##print table with individual PID details
  write.csv(dat4,file="C:/Users/andrewll/Documents/R/MCIOdata/All/ouput_cycletime_pid_details.csv")
  
  ##print table with summarized details by EG
  write.csv(dat5,file="C:/Users/andrewll/Documents/R/MCIOdata/All/ouput_cycletime_pid_summaries.csv")
  
  View(dat5)
  
  ## create PRD charts
  dat6<-dat4[which(dat4$ProjectCategory=="PRD"),]
  
  
  ## create discrete charts
  dat11<-dat4[which(dat4$ProjectCategory=="Discrete"),]
  
  
  
  ## create network charts
  dat21<-dat4[which(dat4$ProjectCategory=="Network"),]
  
  
}