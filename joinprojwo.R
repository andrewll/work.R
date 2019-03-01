joinprojwo<-function(){
  
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
  desired_pg<-c("BOSG - Federal SharePoint "
                ,"BOSG - SPO-S"
                ,"BOSG - SharePoint"
                ,"FASTSearch"
                ,"FOPE"
                ,"FOPE - GSGO"
                ,"PSO KMS"
                ,"PSO MSODS PROD"
                ,"PSO Phone Factor"
                ,"SharePoint Shared MT"
                ,"Skype Platform"
                ,"Skype Plus"
                ,"USNat EOP"
                ,"USNat MSODS"
                ,"USSec EOP"
                ,"USSec MSODS" )
  desired_milestone <- c("Artifact Generation"
                         ,"Cable and Configure Network"
                         ,"Cable and Configure PRD"
                         ,"Cable Validation"
                         ,"Configure & Verify Network"
                         ,"Upstream Device Config")
  deployment_class<-c("New Deployment")
  desired_project_category<-c("PRD","EngineeringGroupNetwork","DiscreteServer")
  desired_WorkOrderStatusName<-c("Closed","Resolved")
  desired_milestonename<-c("Artifact Generation"
                           ,"Cable and Configure Discrete"
                           ,"Cable and Configure Network"
                           ,"Cable and Configure PRD"
                           ,"Cable Validation"
                           ,"Configure & Verify Network")
  desired_WorkOrderStatusName<-c("AGG Device Configuration"
                                 ,"Configure Agg"
                                 ,"Configure Host Leaf"                                
                                 ,"Configure Load Balancer"                            
                                 ,"Configure Network"                                  
                                 ,"Configure Network (LOAD Balancer)"                  
                                 ,"Configure Network (TOR)"                            
                                 ,"Configure T0"
                                 ,"Create Full NDT"                                    
                                 ,"Create NDT"                                         
                                 ,"Create New VLAN"
                                 ,"Host Leaf Device Configuration"
                                 ,"L2 Host Device Configuration"                       
                                 ,"L3 AGG Device Configuration"
                                 ,"Load Balancer Device Configuration"
                                 ,"Network Device Configuration"                       
                                 ,"Network Other - Configuration"
                                 ,"Network Validation"
                                 ,"TOR Switch  Configuration"                          
                                 ,"Validate Agg Cabling"                               
                                 ,"Validate Cabling "                                  
                                 ,"Validate Host Leaf Cabling"                         
                                 ,"Validate Load Balancer Cabling"                     
                                 ,"Validate Network"                                   
                                 ,"Validate PRD Cabling"                               
                                 ,"Verify M0/C0 Config"
                                 ,"Verify Network Configuration"                       
                                 ,"Verify PRD Configuration"                           
                                 ,"Verify T0 Config")
  
  ##set the path to DeploymentPerformance file
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")
  
  
  ##define the deloyments file
  file1 <- "vwprojects2019.csv"
  file2 <- "vwworkorder2019.csv"
  
  
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
  
  #filter work orders
  ##worders02 <- worders[which(worders$WorkOrderStatusName %in% desired_WorkOrderStatusName),]
  worders03 <- worders[which(worders$MilestoneName %in% desired_milestonename),]
  worders05 <- worders03[which(worders03$WorkOrderName %in% desired_WorkOrderStatusName),]
  
  pids$ProjectDelivered <- as.Date.character(pids$ProjectDelivered, format = "%m/%d/%Y")
  
  pids03 <- pids[which(pids$ProjectDelivered > '2018-05-01'),]
  pids05 <- pids03[which(pids03$PropertyGroup %in% desired_pg),]
  pids07 <- pids05[which(pids05$ProjectCategory %in% desired_project_category),]
  pids08 <- pids07[which(pids07$DeploymentClass %in% deployment_class),]
  
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
  ,e.EndDate
  
  FROM pids08 d
  LEFT JOIN worders05 e
  ON d.DeliveryNumber = e.DeliveryNumber"
  
  worders07 <- sqldf(SQLQuery1) 
  
  ##worders09 <- worders07[which(worders03$MilestoneName %in% desired_milestone),]
  
  worders07$StartDate <- as.Date.character(worders07$StartDate, format = "%m/%d/%Y")
  worders07$EndDate <- as.Date.character(worders07$EndDate, format = "%m/%d/%Y")
  worders07 <- mutate(worders07, calc_ticket_ct = EndDate - StartDate)
  
  ##print output
  write.csv(pids08,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/Nonpnaasprojects_projects.csv")
  write.csv(worders07,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/Nonpnaasprojects_workorders_network.csv")
  
  
  
}