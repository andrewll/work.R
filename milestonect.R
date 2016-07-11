milestonect<-function(){
  
  #############################
  ##
  ##  I want to know the cycle times of the milestones for PIDs of my EG...
  ##
  ##  ...so I wrote this script.
  ##
  ##
  #############################
  
  library(ggplot2)
  library(dplyr)
  library(sqldf)
  library(scales)
  library(reshape2)
  library(lubridate)
  
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")
  
  ##define the deloyments file
  file1 <- "DeliveryPerformanceWithMilestone.csv"
  ##define SPO waves
  file2 <- "spowaves.csv"
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  ##define the spo waves file path
  file_loc2 <- file.path(path, file2)
  
  desired_project_category<-c("PRD","Network")
  
  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  spowaves <- read.csv(file_loc2,
                       header = TRUE, colClasses = NA, na.strings = "N/A", stringsAsFactors = TRUE)
  
  
  
  ##remove dots in header names in pids table
  pidsnames <- gsub("\\.","",names(pids))
  colnames(pids) <- c(pidsnames)
  
  spowavenames <-gsub("\\.","",names(spowaves))
  colnames(spowaves) <-c(spowavenames)
  
  ##convert dates to date format for pids table
  pids$RTEGActualDeliveryDate <- as.Date(pids$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  
  ##subsetting to correct data
  pids03<-pids[which(pids$EG=="O365 SharePoint"),]
  pids05<-pids03[which(pids03$RTEGActualDeliveryDate>"2016-01-01"),]
  pids07<-pids05[which(pids05$ProjectCategory %in% desired_project_category),]
  pids09<-pids07[which(pids07$DeploymentClass=="New Deployment"),]

  ##join the merge table with the pids table
  SQLQuery1 <- "SELECT p.DeliveryNumber
  ,p.RTEGActualDeliveryDate
  ,p.EG
  ,p.ProjectCategory
  ,p.DeploymentClass
  ,p.WorkOrderName
  ,p.WorkOrderCycleTime
  ,p.MilestoneName
  ,p.ProjectReadinessValue
  ,p.NetworkReadinessValue
  ,p.CablingReadinessValue
  ,p.ProcurementValue
  ,p.DeliveryReadinessValue
  ,p.ReceivingValue
  ,p.BoltandEnergizeValue
  ,p.CableValidationValue
  ,p.ConfigureVerifyNetworkValue
  ,p.OperationalAcceptanceValue
  ,w.WaveCategory
  FROM pids09 p
  LEFT JOIN spowaves w 
  ON p.DeliveryNumber = w.DeliveryNumber"
  
  pids12 <- sqldf(SQLQuery1)
  
  ##subset down to unique values per row
  pids14<-unique(pids12)
  
  pids15<-mutate(pids14, month_delivered = format(ymd(RTEGActualDeliveryDate),"%Y-%m"))
  
  ##split into network and PRD pids
  pids16<-pids15[which(pids15$ProjectCategory=="Network"),]
  pids28<-pids15[which(pids15$ProjectCategory=="PRD"),]

  ##generate charts for network pids for Configure & Verify milestone
  pids36<-pids16[which(pids16$MilestoneName=="Configure & Verify Network"),]
    png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_boxplot_network_configverifymilestone_GFSD_data.png", 
      width = 960, height = 480, units = "px")
  g<-ggplot(pids36, aes(x=month_delivered, y=WorkOrderCycleTime, fill=WorkOrderName))
  g+geom_boxplot()+labs(title="SPO Network PIDs Config&Verify Milestone Cycle Times by Month of RTEG", x="Month of RTEG", y="Cycle Time in days")+
      geom_hline(yintercept = 29)
  dev.off()
  
  ##generate charts for PRD pids for Configure & Verify milestone
  pids48<-pids28[which(pids28$MilestoneName=="Configure & Verify Network"),]
  png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_boxplot_prd_configverifymilestone_GFSD_data.png", 
      width = 960, height = 480, units = "px")
  g<-ggplot(pids48, aes(x=month_delivered, y=WorkOrderCycleTime, fill=WorkOrderName))
  g+geom_boxplot()+labs(title="SPO PRD PIDs Config&Verify Milestone Cycle Times by Month of RTEG", x="Month of RTEG", y="Cycle Time in days")+
    geom_hline(yintercept = 7)
  dev.off()
  
  
  
  #generate data sheet for network pids
  pids26<-pids16 %>%
    arrange(month_delivered)
  
  ##plot for network pids for Procurement milestone
  pids37<-pids16[which(pids16$MilestoneName=="Procurement"),]
  png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_boxplot_network_procurementvalue_GFSD_data.png", 
      width = 960, height = 480, units = "px")
  g<-ggplot(pids37, aes(x=month_delivered, y=WorkOrderCycleTime, fill=WorkOrderName))
  g+geom_boxplot()+labs(title="SPO Network PIDs Procurement Milestone", x="Month of RTEG", y="Cycle Time in days")+
    geom_hline(yintercept = 10)
  dev.off()
  
  
  ##plot for PRD pids for Procurement milestone
  pids49<-pids28[which(pids28$MilestoneName=="Procurement"),]
  png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_boxplot_prd_procurementvalue_GFSD_data.png", 
      width = 960, height = 480, units = "px")
  g<-ggplot(pids49, aes(x=month_delivered, y=WorkOrderCycleTime, fill=WorkOrderName))
  g+geom_boxplot()+labs(title="SPO PRD PIDs Procurement Milestone", x="Month of RTEG", y="Cycle Time in days")+
    geom_hline(yintercept = 10)
  dev.off()
  
  ##plot for Network pids for Receiving milestone
  pids38<-pids16[which(pids16$MilestoneName=="Receiving"),]
  png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_boxplot_network_receivingvalue_GFSD_data.png", 
      width = 960, height = 480, units = "px")
  g<-ggplot(pids38, aes(x=month_delivered, y=WorkOrderCycleTime, fill=WorkOrderName))
  g+geom_boxplot()+labs(title="SPO Network PIDs Receiving Milestone", x="Month of RTEG", y="Cycle Time in days")+
    geom_hline(yintercept = 60)
  dev.off()
  
  ##plot for PRD pids for Receiving milestone
  pids50<-pids28[which(pids28$MilestoneName=="Receiving"),]
  png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_boxplot_prd_receivingvalue_GFSD_data.png", 
      width = 960, height = 480, units = "px")
  g<-ggplot(pids50, aes(x=month_delivered, y=WorkOrderCycleTime, fill=WorkOrderName))
  g+geom_boxplot()+labs(title="SPO PRD PIDs Receiving Milestone", x="Month of RTEG", y="Cycle Time in days")+
    geom_hline(yintercept = 60)
  dev.off()
  

}
