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
  desired_project_category<-c("Network", "PRD")
  desired_eg<-c("O365 SharePoint")
  deployment_class<-c("New Deployment")
  
  ##set the path to DeploymentPerformance file
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")
  
  
  ##define the deloyments file
  file1 <- "DeliveryProjectContentReport.csv"
  
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)

  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = FALSE)
  
  ##convert dates to date format for pids table
  pids$ProjectDelivered <- as.Date.character(pids$ProjectDelivered, format = "%m/%d/%Y")
  pids$EstimatedRTEGDate <- as.Date.character(pids$EstimatedRTEGDate)
  pids$RequestedDelivery <- as.Date.character(pids$RequestedDelivery,, format = "%m/%d/%Y")
  pids$CommittedDelivery <- as.Date.character(pids$CommittedDelivery, format = "%m/%d/%Y")
  pids$CreationDate <- as.Date.character(pids$CreationDate, format = "%m/%d/%Y")
  
  pids$ActualDockMax <- lubridate::ymd(pids$ActualDockMax)
  pids$CurrentCommittedDockMax <-lubridate::ymd(pids$CurrentCommittedDockMax)

  
  ##remove dots in header names in pids table
  pidsnames <- gsub("\\.","",names(pids))
  colnames(pids) <- c(pidsnames)

  pids03 <- pids[which(pids$ProjectDelivered > '2017-07-01'),]
  pids05 <- pids03[which(pids03$EngineeringGroup %in% desired_eg),]
  pids07 <- pids05[which(pids05$ProjectCategory %in% desired_project_category),]
  pids08 <- pids07[which(pids07$DeploymentClass %in% deployment_class),]
  
  
  ##add new calculated columns
  pids09 <- mutate(pids08, Month_Delivered = format(ProjectDelivered, "%y-%m"),
                   pid_to_rteg = ProjectDelivered - CreationDate, DTR = ProjectDelivered - ActualDockMax, 
                   PIDCount = 1)
  
  pids_network_rawdata <- pids09[which(pids09$ProjectCategory=="Network"),]
  pids_prd_rawdata <- pids09[which(pids09$ProjectCategory=="PRD"),]
  
  ##summarize network
  pids_network_summary <- pids_network_rawdata %>% 
    group_by(Month_Delivered) %>%
    summarize(pid_to_rteg_mean = mean(pid_to_rteg), 
              pid_to_rteg_95th = quantile(pid_to_rteg,.95, na.rm = TRUE), 
              dock_to_rteg_mean = mean(DTR, na.rm = TRUE),
              dock_to_rteg_95th = quantile(DTR, .95, na.rm = TRUE),
              PIDCount = sum(PIDCount))
  
  pids_prd_summary <- pids_prd_rawdata %>% 
    group_by(Month_Delivered) %>%
    summarize(pid_to_rteg_mean = mean(pid_to_rteg), 
              pid_to_rteg_95th = quantile(pid_to_rteg,.95, na.rm = TRUE), 
              dock_to_rteg_mean = mean(DTR, na.rm = TRUE),
              dock_to_rteg_95th = quantile(DTR, .95, na.rm = TRUE),
              PIDCount = sum(PIDCount))
  
  ##write to file - including ITAR
  write.csv(pids_network_rawdata,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/SPO_network_rawdata.csv")
  write.csv(pids_network_summary,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/SPO_network_summary.csv")
  write.csv(pids_prd_rawdata,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/SPO_prd_rawdata.csv")
  write.csv(pids_prd_summary,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/SPO_prd_summary.csv")
  
  
  
  
  
  ##plot box and whisker
  png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_boxplot_prd_spo.png", 
      width = 960, height = 480, units = "px")
  g<-ggplot(pids11,aes(x=Month_Delivered, y=dock_to_rteg_95th))
  g+geom_bar(stat="identity")+labs(title="SPO PRD Dock-to-RTEG Cycle Times", x="Month of RTEG", y="Cycle Time in days")
  dev.off()
  
  
  
  
  
  
}