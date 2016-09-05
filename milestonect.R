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
  desired_eg<-c("O365 SharePoint")
  
  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  ##spowaves <- read.csv(file_loc2,
    ##                   header = TRUE, colClasses = NA, na.strings = "N/A", stringsAsFactors = TRUE)
  
  
  
  ##remove dots in header names in pids table
  pidsnames <- gsub("\\.","",names(pids))
  colnames(pids) <- c(pidsnames)
  
  ##spowavenames <-gsub("\\.","",names(spowaves))
  ##colnames(spowaves) <-c(spowavenames)
  
  ##convert dates to date format for pids table
  pids$RTEGActualDeliveryDate <- as.Date(pids$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  
  ##subsetting to correct data
  pids02<-subset(pids, select=c("DeliveryNumber"
                    ,"EG"
                    ,"CommittedDeliveryDate"
                    ,"RTEGActualDeliveryDate"
                    ,"DataCenter"
                    ,"DeploymentClass"
                    ,"ProjectCategory"
                    ,"MilestoneName"
                    ,"WorkOrderName"
                    ,"WorkOrderCycleTime"
                    ,"WorkOrderPlannedCycletime"
                    ,"WoadDock"))
  pids03<-pids02[which(pids02$EG %in% desired_eg),]
  pids05<-pids03[which(pids03$RTEGActualDeliveryDate>"2016-01-01"),]
  pids07<-pids05[which(pids05$ProjectCategory %in% desired_project_category),]
  pids09<-pids07[which(pids07$DeploymentClass=="New Deployment"),]

  ##join the merge table with the pids table
  ##SQLQuery1 <- "SELECT p.DeliveryNumber
  ##,p.RTEGActualDeliveryDate
  ##,p.EG
  ##,p.ProjectCategory
  ##,p.DeploymentClass
  ##,p.WorkOrderName
  ##,p.WorkOrderCycleTime
  ##,p.MilestoneName
  ##,p.ProjectReadinessValue
  ##,p.NetworkReadinessValue
  ##,p.CablingReadinessValue
  ##,p.ProcurementValue
  ##,p.DeliveryReadinessValue
  ##,p.ReceivingValue
  ##,p.BoltandEnergizeValue
  ##,p.CableValidationValue
  ##,p.ConfigureVerifyNetworkValue
  ##,p.OperationalAcceptanceValue
  ##,w.WaveCategory
  ##FROM pids09 p
  ##LEFT JOIN spowaves w 
  ##ON p.DeliveryNumber = w.DeliveryNumber"
  
  ##pids12 <- sqldf(SQLQuery1)
  
  ##subset down to unique values per row
  pids14<-unique(pids09)
  
  pids15<-mutate(pids14, month_delivered = format(RTEGActualDeliveryDate, "%m"), workordervariance = WorkOrderCycleTime/WorkOrderPlannedCycletime)
  
  ##split into network and PRD pids
  pids16<-pids15[which(pids15$ProjectCategory=="Network"),]
  pids28<-pids15[which(pids15$ProjectCategory=="PRD"),]

  ##subset by milestone
  targetmilestone1 <- c("Project Readiness")
  targetmilestone2 <- c("Network Readiness")
  targetmilestone3 <- c("DC Readiness")
  targetmilestone4 <- c("Procurement")
  targetmilestone5 <- c("Delivery Readiness")
  targetmilestone6 <- c("Receiving")
  targetmilestone7 <- c("Bolt and Energize")
  targetmilestone8 <- c("Physical Cabling")
  targetmilestone9 <- c("Operational Acceptance")
  targetmilestone10<- c("Configure & Verify Network")
  pids31<-pids28[which(pids28$MilestoneName %in% targetmilestone1),]
  pids32<-pids28[which(pids28$MilestoneName %in% targetmilestone2),]
  pids33<-pids28[which(pids28$MilestoneName %in% targetmilestone3),]
  pids34<-pids28[which(pids28$MilestoneName %in% targetmilestone4),]
  pids35<-pids28[which(pids28$MilestoneName %in% targetmilestone5),]
  pids36<-pids28[which(pids28$MilestoneName %in% targetmilestone6),]
  pids37<-pids28[which(pids28$MilestoneName %in% targetmilestone7),]
  pids38<-pids28[which(pids28$MilestoneName %in% targetmilestone8),]
  pids39<-pids28[which(pids28$MilestoneName %in% targetmilestone9),]
  pids40<-pids28[which(pids28$MilestoneName %in% targetmilestone10),]
  
  ##plot1
  pids41<-pids31 %>%
    group_by(WorkOrderName,month_delivered) %>%
    summarize(WorkOrder_CycleTime_95th=quantile(WorkOrderCycleTime,.95),Milestone_Name = "Project Readiness") %>%
    arrange(month_delivered) 
  png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_milestone_gfsd_data_projectreadiness.png", 
      width = 960, height = 480, units = "px")
  g<-ggplot(pids41, aes(x=month_delivered, y=WorkOrder_CycleTime_95th, fill=WorkOrderName))
  g+geom_bar(stat="identity")+facet_wrap(~WorkOrderName)+labs(title="Project Readiness Milestone - 95th% CT per WO")
  dev.off()
  
  ##plot2
  png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_milestone_gfsd_data_NWreadiness.png", 
      width = 960, height = 480, units = "px")
  g<-ggplot(pids32, aes(x=WorkOrderName, y=WorkOrderCycleTime, fill=WorkOrderName))
  g+geom_bar(stat="identity") + facet_wrap(~month_delivered)+labs(title="Network Readiness Milestone - Accumulated Days Spent per WO")
  dev.off()
  
  ##plot3
  png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_milestone_gfsd_data_DCreadiness.png", 
      width = 960, height = 480, units = "px")
  g<-ggplot(pids33, aes(x=WorkOrderName, y=WorkOrderCycleTime, fill=WorkOrderName))
  g+geom_bar(stat="identity") + facet_wrap(~month_delivered)+labs(title="DC Readiness Milestone - Accumulated Days Spent per WO")
  dev.off()
  
  ##plot4
  png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_milestone_gfsd_data_Procurement.png", 
      width = 960, height = 480, units = "px")
  g<-ggplot(pids34, aes(x=WorkOrderName, y=WorkOrderCycleTime, fill=WorkOrderName))
  g+geom_bar(stat="identity") + facet_wrap(~month_delivered)+labs(title="Procurement Milestone - Accumulated Days Spent per WO")
  dev.off()
  
  ##plot5
  png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_milestone_gfsd_data_delivery_readiness.png", 
      width = 960, height = 480, units = "px")
  g<-ggplot(pids35, aes(x=WorkOrderName, y=WorkOrderCycleTime, fill=WorkOrderName))
  g+geom_bar(stat="identity") + facet_wrap(~month_delivered)+labs(title="Delivery Readiness Milestone - Accumulated Days Spent per WO")
  dev.off()
  
  ##plot6 Receiving
  pids46<-pids36 %>%
    group_by(WorkOrderName,month_delivered) %>%
    summarize(WorkOrder_CycleTime_95th=quantile(WorkOrderCycleTime,.95),Milestone_Name = "Receiving") %>%
    arrange(month_delivered) 
  png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_milestone_gfsd_data_receiving.png", 
      width = 960, height = 480, units = "px")
  g<-ggplot(pids46, aes(x=month_delivered, y=WorkOrder_CycleTime_95th, fill=WorkOrderName))
  g+geom_bar(stat="identity") + facet_wrap(~WorkOrderName)+labs(title="Receiving Milestone - 95th% CT per WO")
  dev.off()
  
  ##plot7 Bolt & Energize
  pids47<-pids37 %>%
    group_by(WorkOrderName,month_delivered) %>%
    summarize(WorkOrder_CycleTime_95th=quantile(WorkOrderCycleTime,.95),Milestone_Name = "Bolt and Energize") %>%
    arrange(month_delivered) 
  png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_milestone_gfsd_data_bolt_energize.png", 
      width = 960, height = 480, units = "px")
  g<-ggplot(pids47, aes(x=month_delivered, y=WorkOrder_CycleTime_95th, fill=WorkOrderName))
  g+geom_bar(stat="identity") + facet_wrap(~WorkOrderName)+labs(title="Bolt and Energize Milestone - 95th% CT per WO")
  dev.off()
  
  ##plot8 Physical Cabling
  pids48<-pids38 %>%
    group_by(WorkOrderName,month_delivered) %>%
    summarize(WorkOrder_CycleTime_95th=quantile(WorkOrderCycleTime,.95),Milestone_Name = "Physical Cabling") %>%
    arrange(month_delivered) 
  png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_milestone_gfsd_data_phys_cabling.png", 
      width = 960, height = 480, units = "px")
  g<-ggplot(pids48, aes(x=month_delivered, y=WorkOrder_CycleTime_95th, fill=WorkOrderName))
  g+geom_bar(stat="identity") + facet_wrap(~WorkOrderName)+labs(title="Physical Cabling Milestone - 95th% CT per WO")
  dev.off()
  
  ##plot9 Ops Accept
  pids49<-pids39 %>%
    group_by(WorkOrderName,month_delivered) %>%
    summarize(WorkOrder_CycleTime_95th=quantile(WorkOrderCycleTime,.95),Milestone_Name = "Operational Acceptance") %>%
    arrange(month_delivered) 
  png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_milestone_gfsd_data_ops_accept.png", 
      width = 960, height = 480, units = "px")
  g<-ggplot(pids49, aes(x=month_delivered, y=WorkOrder_CycleTime_95th, fill=WorkOrderName))
  g+geom_bar(stat="identity") + facet_wrap(~WorkOrderName)+labs(title="Ops Accept Milestone - 95th% CT per WO")
  dev.off()
  
  ##plot10 Configure & Verify milestone
  pids50<-pids40 %>%
    group_by(WorkOrderName,month_delivered) %>%
    summarize(WorkOrder_CycleTime_95th=quantile(WorkOrderCycleTime,.95),Milestone_Name = "Configure & Verify Network") %>%
    arrange(month_delivered) 
  png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_milestone_gfsd_data_confignetwork.png", 
      width = 960, height = 480, units = "px")
  g<-ggplot(pids50, aes(x=month_delivered, y=WorkOrder_CycleTime_95th, fill=WorkOrderName))
  g+geom_bar(stat="identity") + facet_wrap(~WorkOrderName)+labs(title="Configure Verify Network Milestone - 95th% CT per WO")
  dev.off()
  
  ##generate charts for PRD pids for Configure & Verify milestone
  pids48<-pids28[which(pids28$MilestoneName=="Configure & Verify Network"),]
  ##png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_boxplot_prd_configverifymilestone_GFSD_data.png", 
  ##    width = 960, height = 480, units = "px")
  ##g<-ggplot(pids48, aes(x=month_delivered, y=WorkOrderCycleTime, fill=WorkOrderName))
  ##g+geom_boxplot()+labs(title="SPO PRD PIDs Config&Verify Milestone Cycle Times by Month of RTEG", x="Month of RTEG", y="Cycle Time in days")+
  ##  geom_hline(yintercept = 7)
  ##dev.off()
  
  
  
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
