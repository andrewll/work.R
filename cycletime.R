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
  mydf <- read.csv(file1, stringsAsFactors = FALSE)
  ##mydf<-read.csv("MCIO/All/DelCap-Jan1-May31-alltypes.csv", stringsAsFactors = FALSE)
  dat <- tbl_df(mydf)
  
  ##select desired columns from the dataframe and add some new ones for month and year delivered
  dat2 <- subset(dat, select=c("EG","DeliveryNumber","ProjectTitle","RTEGOTDF","RTEGActualDeliveryDate", "ProjectCategory", "OrderCOnfirmtoDock_Actual", "DTR1", "OrderCOnfirmtoRTEG_Actual2"))
  ##dat2[,5]<-as.character(strptime(dat2[,5],"%m/%d/%Y"))
  dat3 <- mutate(dat2, Month_Delivered = format(mdy(RTEGActualDeliveryDate),"%b"), Year_Delivered = format(mdy(RTEGActualDeliveryDate),"%Y"), PIDCount = 1)
  
  
  dat4<- dat3 %>% 
    group_by(EG, ProjectCategory, Year_Delivered, Month_Delivered) %>%
    ##summarize(PIDCount = sum(PIDCount), OrderCOnfirmtoDock_Actual_Avg = mean(OrderCOnfirmtoDock_Actual, na.rm=TRUE), OrderCOnfirmtoDock_Actual_Median = median(OrderCOnfirmtoDock_Actual, na.rm=TRUE), OrderCOnfirmtoDock_Actual_95th = quantile(OrderCOnfirmtoDock_Actual, .95, na.rm=TRUE), Dock_to_RTEG_Avg = mean(DTR1, na.rm=TRUE),  Dock_to_RTEG_Median = median(DTR1, na.rm=TRUE), Dock_to_RTEG_95th = quantile(DTR1, .95, na.rm=TRUE), OrderCOnfirmtoRTEG_Avg = mean(OrderCOnfirmtoRTEG_Actual2, na.rm=TRUE), OrderCOnfirmtoRTEG_Median = median(OrderCOnfirmtoRTEG_Actual2,na.rm=TRUE), OrderCOnfirmtoRTEG_95th = quantile(OrderCOnfirmtoRTEG_Actual2, .95,na.rm=TRUE)) %>%
    summarize(PIDCount = sum(PIDCount), OrderCOnfirmtoDock_Actual_Avg = mean(OrderCOnfirmtoDock_Actual, na.rm=TRUE), OrderCOnfirmtoDock_Actual_95th = quantile(OrderCOnfirmtoDock_Actual, .95, na.rm=TRUE), Dock_to_RTEG_Avg = mean(DTR1, na.rm=TRUE),  Dock_to_RTEG_95th = quantile(DTR1, .95, na.rm=TRUE), OrderCOnfirmtoRTEG_Avg = mean(OrderCOnfirmtoRTEG_Actual2, na.rm=TRUE), OrderCOnfirmtoRTEG_95th = quantile(OrderCOnfirmtoRTEG_Actual2, .95,na.rm=TRUE)) %>%
    arrange(EG, ProjectCategory, Year_Delivered, Month_Delivered)
    
  write.csv(dat4,file="ouput_cycletime.csv")
  View(dat4)
  
}