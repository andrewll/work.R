cycletime5<-function(){
  
  #######
  ##
  ##this script calculates the following metrics for all EG: OrderConfirm-to-Dock, Dock-to-RTEG, OrderConfirm-to-RTEG
  ##For each metrics, it calculates the mean, median, and 95th percentile
  ##It takes a monthly calculation of the metrics
  ##
  ##Input file is SQL query
  ##
  #######
  
  
  library(ggplot2)
  library(dplyr)
  library(sqldf)
  library(scales)
  library(reshape2)
  library(lubridate)
  
  # basic set up clear all existing variables 
  rm(list = ls(all=T))
  
  #setup variables
  desired_project_category<-c("PRD")
  desired_eg<-c("O365 SharePoint")
  itar<-c("443779","443780","455430","455254")
  
  ##set the path to DeploymentPerformance file
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")
  
  
  ##define the deloyments file
  file1 <- "DeliveryPerformance.csv"
  
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)

  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  

  ##convert dates to date format for pids table
  pids$RTEGActualDeliveryDate <- as.Date(pids$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  pids$ReceivingDate <- as.Date(pids$ReceivingDate, format = "%m/%d/%Y")
  pids$DemandCreatedDate<- as.Date(pids$DemandCreatedDate, format = "%m/%d/%Y")
  pids$woadDock <- as.Date(pids$woadDock, format = "%m/%d/%Y")
  pids$DM.Estimated.RTEG.Date <- as.Date(pids$DM.Estimated.RTEG.Date, format = "%m/%d/%Y")
  pids$RequestedDeliveryDate <- as.Date(pids$RequestedDeliveryDate, format = "%m/%d/%Y")
  pids$ProjectCreationDate <- as.Date(pids$ProjectCreationDate, format = "%m/%d/%Y")
  pids$PO.Confirmed.Dock.Date <- as.Date(pids$PO.Confirmed.Dock.Date, format = "%m/%d/%Y")
  pids$Current.Committed.Dock.Date <- as.Date(pids$Current.Committed.Dock.Date, format = "%m/%d/%Y")
  pids$rtegActualMonth <- as.Date(pids$rtegActualMonth, format = "%m/%d/%Y")
  
  ##remove dots in header names in pids table
  pidsnames <- gsub("\\.","",names(pids))
  colnames(pids) <- c(pidsnames)

  pids03 <- pids[which(pids$RTEGActualDeliveryDate > '2016-01-01'),]
  pids05 <- pids03[which(pids03$EG %in% desired_eg),]
  pids07 <- pids05[which(pids05$ProjectCategory %in% desired_project_category),]
  pids08 <- pids07[which(!pids07$DeliveryNumber %in% itar),]
  
  pids09 <- mutate(pids08, Month_Delivered = format(RTEGActualDeliveryDate, "%m"),
                   pid_to_rteg = RTEGActualDeliveryDate - ProjectCreationDate,
                   PIDCount = 1)
  
  ##summarize
  pids11 <- pids09 %>% 
    group_by(Month_Delivered) %>%
    summarize(pid_to_rteg_avg = mean(pid_to_rteg), 
              pid_to_rteg_95th = quantile(pid_to_rteg,.95, na.rm = TRUE), 
              dock_to_rteg_mean = mean(DTR, na.rm = TRUE),
              dock_to_rteg_95th = quantile(DTR, .95, na.rm = TRUE),
              PIDCount = sum(PIDCount))

  write.csv(pids11,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/SPO-PRD-CT_test.csv")
  
  ##plot box and whisker
  png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_boxplot_prd_spo.png", 
      width = 960, height = 480, units = "px")
  g<-ggplot(pids11,aes(x=Month_Delivered, y=dock_to_rteg_95th))
  g+geom_bar(stat="identity")+labs(title="SPO PRD Dock-to-RTEG Cycle Times", x="Month of RTEG", y="Cycle Time in days")
  dev.off()
  
  
  
  
  
  
}