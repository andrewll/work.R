simulaterteg<-function(projcategory){
  
  #########################
  ##
  ## purpose: to run monte carlo simulations of the milestone data for PRD deployments, 
  ## to ultimately help forecast the DM Estimated RTEG date, and to determine which milestones to target first to get down
  ## to a target DTR cycletime
  ##
  #########################
  
  # basic set up clear all existing variables 
  rm(list = ls(all=T))
  
  library(dplyr)
  library(ggplot2)
  library(sqldf)
  
  ##Set variables
  ##EGList <- c("O365 Exchange", "AP", "O365 SharePoint","CRM","XBOX","ISSD (Azure AAD)")
  EGList <- c("O365 SharePoint")
  
  
  ##set the path to DeploymentPerformance file
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")
  ##define the deloyments file
  file1 <- "DeliveryPerformance.csv"
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  
  ## read data
  dat1 <- read.csv("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in/DeliveryPerformanceWithMilestone.csv", stringsAsFactors = TRUE)
  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)


  
  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  
  ##convert dates into correct format
  dat1$RTEGActualDeliveryDate <- as.Date(dat1$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  dat1$Woad.Dock<- as.Date(dat1$Woad.Dock, format = "%m/%d/%Y")
  
  ##convert Delivery Number to correct format
  dat1$DeliveryNumber<-as.character(dat1$DeliveryNumber)
  

  ##remove dots in header names in pids table
  pidsnames <- gsub("\\.","",names(dat1))
  colnames(dat1) <- c(pidsnames)
  
  ##remove dots in header names in pids table
  pidsnames <- gsub("\\.","",names(pids))
  colnames(pids) <- c(pidsnames)
  
  
  ## select desired variables
  dat2<- subset(dat1, select = c("DeliveryNumber"
                                 ,"DeploymentClass"
                                 ,"EG"
                                 ,"ProjectCategory"
                                 ,"DataCenter"
                                 ,"RTEGActualDeliveryDate"
                                 ,"NetworkReadinessValue"
                                 ,"DCReadinessValue"
                                 ,"ProcurementValue"
                                 ,"ReceivingValue"
                                 ,"BoltandEnergizeValue"
                                 ,"PhysicalCablingValue" 
                                 ,"ConfigureVerifyNetworkValue"
                                 ,"OperationalAcceptanceValue"
                                 ,"WoadDock"))
  
  #remove duplicate entries
  dat3<-unique(dat2)
  
  ##extract PRD objects
  ##dat4<-dat3[which(dat3$ProjectCategory==projcategory),]
  dat4<-dat3[which(dat3$ProjectCategory=="PRD"),]
  dat5<-dat4[which(as.character(dat4$EG) %in% EGList),]
  
  ##calculate dock-to-rteg
  dat6<-mutate(dat5, DTR = as.numeric(RTEGActualDeliveryDate - WoadDock), rtegmonthname = format(as.Date(RTEGActualDeliveryDate), "%Y-%m"))
  
  ##mark negative values in DTR as NA
  for(i in 1:nrow(dat6)){
    if(is.na(dat6[i,]$DTR)){
      dat6[i,]$DTR<-c(NA)
    }else if(dat6[i,]$DTR<0){
      dat6[i,]$DTR<-c(NA)
      }
  }
  
  ##convert dates to date format for pids table
  pids$ProjectCreationDate <- as.Date(pids$ProjectCreationDate, format = "%m/%d/%Y")
  pids$DMEstimatedRTEGDate <- as.Date(pids$DMEstimatedRTEGDate, format = "%m/%d/%Y")
  pids$RTEGActualDeliveryDate <- as.Date(pids$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  
  ##merge with input file that contains ProjectCreate Date
  SQLQuery1 <- "SELECT p.DeliveryNumber
  ,p.DeploymentClass
  ,p.EG
  ,p.ProjectCategory
  ,p.DataCenter
  ,w.RTEGActualDeliveryDate
  ,p.NetworkReadinessValue
  ,p.DCReadinessValue
  ,p.ProcurementValue
  ,p.ReceivingValue
  ,p.BoltandEnergizeValue
  ,p.PhysicalCablingValue
  ,p.ConfigureVerifyNetworkValue
  ,p.OperationalAcceptanceValue
  ,p.WoadDock
  ,p.DTR
  ,p.rtegmonthname
  ,w.ProjectCreationDate
  ,w.DMEstimatedRTEGDate
  FROM dat6 p
  LEFT JOIN pids w 
  ON p.DeliveryNumber = w.DeliveryNumber"
  
  pids2 <- sqldf(SQLQuery1)
  
  ##add a column to calculate PIDCreate-to-RTEG
  pids4 <- pids2%>%
           mutate(PIDCreateToRTEG = RTEGActualDeliveryDate - ProjectCreationDate)%>%
           arrange(rtegmonthname)

  #write output file
  write.csv(pids4,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/simulaterteg-input-file.csv")
  
  ##chart boxplots of server pids - cycle time for PID lifecycle
  png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_boxplot_servers.png", width = 960, height = 480, units = "px")
  g<-ggplot(pids15, aes(rtegmonthname, PIDcreateToRTEG))
  g + geom_boxplot()+labs(title="SPO Server PID LifeCycle - PIDcreate-to-RTEG")
  dev.off()
  
  
}