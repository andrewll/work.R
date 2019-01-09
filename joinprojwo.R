joinprojwo<--function(){
  
  #######
  ##
  ##the purpose of this script is to create a table from which we can generate cycle time metrics for the FPN team
  ##by joining the project table and the work order table to extract data for their performance in closing network configuration tickets
  ##and create trends for analysis and decision making
  ##
  ##
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
  desired_pg<-c("FOPE"
                ,"PSO MSODS PPE"
                ,"PSO MSODS PROD"
                ,"Core Platform Services"
                ,"Azure DNS Global Services (COGS)"
                ,"Data Center Services"
                ,"BOSG - Federal SharePoint"
                ,"BOSG - Network Infrastructure" 
                ,"BOSG - SharePoint"
                ,"BOSG - SPO-S"
                ,"FOPE - GSGO"
                )
  desired_milestone <- c("Artifact Generation"
                         ,"Cable and Configure Network"
                         ,"Cable and Configure PRD"
                         ,"Cable Validation"
                         ,"Configure & Verify Network"
                         ,"Upstream Device Config")
  deployment_class<-c("New Deployment")
  
  ##set the path to DeploymentPerformance file
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")
  
  
  ##define the deloyments file
  file1 <- "projects2018.csv"
  file2 <- "WO2018.csv"
  
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  file_loc2 <- file.path(path, file2)
  
  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = FALSE)
  worders <- read.csv(file_loc2, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = FALSE)
  
  ##remove dots in header names in pids table
  pidsnames <- gsub("\\.","",names(pids))
  colnames(pids) <- c(pidsnames)
  
  ##remove dots in header names in pids table
  wordersnames <- gsub("\\.","",names(worders))
  colnames(worders) <- c(wordersnames)
  
  pids03 <- pids[which(pids$ProjectDelivered > '2018-07-01'),]
  pids05 <- pids03[which(pids03$PropertyGroup %in% desired_pg),]
  ##pids07 <- pids05[which(pids05$ProjectCategory %in% desired_project_category),]
  ##pids08 <- pids07[which(pids07$DeploymentClass %in% deployment_class),]
  
  SQLQuery1 <- "SELECT d.DeliveryNumber
  ,d.ProjectTitle
  ,d.EngineeringGroup
  ,d.ActualDockMax
  ,d.PropertyGroup
  ,e.FixedSLA
  ,e.TicketNumber
  ,e.TicketID
  ,e.WorkOrderName
  ,e.MilestoneName
  ,e.WorkOrderStatusName
  ,e.StartDate
  ,e.WOActualCycletimewithBlockage
  ,e.WOActualCycletimewithoutBlockage
  ,e.EndDate
  
  FROM pids05 d
  LEFT JOIN worders e
  ON d.DeliveryNumber = e.DeliveryNumber"
  
  worders03 <- sqldf(SQLQuery1) 
  
  worders05 <- worders03[which(worders03$MilestoneName %in% desired_milestone),]
  
  worders05$StartDate <- as.Date.character(worders05$StartDate, format = "%m/%d/%Y")
  worders05$EndDate <- as.Date.character(worders05$EndDate, format = "%m/%d/%Y")
  worders07 <- mutate(worders05, calc_ticket_ct = EndDate - StartDate)
  
  ##print output
  write.csv(worders07,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/O365projects_workorders_network_Nov8.csv")
  
  
  
}