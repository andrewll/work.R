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
  pids05<-pids03[which(pids03$RTEGActualDeliveryDate>"2015-07-01"),]
  pids07<-pids05[which(pids05$ProjectCategory %in% desired_project_category),]
  pids09<-pids07[which(pids07$DeploymentClass=="New Deployment"),]

  ##join the merge table with the pids table
  SQLQuery1 <- "SELECT p.DeliveryNumber
  ,p.RTEGActualDeliveryDate
  ,p.EG
  ,p.ProjectCategory
  ,p.DeploymentClass
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
  
  ##split into network and PRD pids
  pids16<-pids14[which(pids14$ProjectCategory=="Network"),]
  pids36<-pids14[which(pids14$ProjectCategory=="PRD"),]

  ##generate charts for network pids
  g<-ggplot(pids16, aes(x=WavesCategory, y=ConfigureVerifyNetworkValue))
  g+geom_boxplot()
  
  
    
}
