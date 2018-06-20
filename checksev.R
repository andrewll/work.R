checksev<-function(){
  
  library(ggplot2)
  library(dplyr)
  library(sqldf)
  library(scales)
  library(reshape2)
  library(lubridate)
  
  # basic set up clear all existing variables 
  rm(list = ls(all=T))
  
  ##set the path to DeploymentPerformance file
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")
  
  ##define the deloyments file
  file1 <- "DeliveryProjectContentReport.csv"
  file5 <- "Incidents.csv"
  
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  ##define the spo pairs file path
  file_loc5 <- file.path(path, file5)
  
  ##define variables
  desired_eg<-c("O365 SharePoint", "O365 Exchange","FOPE","OneDrive")
  
  
  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  incidents <- read.csv(file_loc5, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  
  ##convert field formatting
  pids$ProjectDelivered <- as.Date.character(pids$ProjectDelivered, format = "%m/%d/%Y")
  
  pids3<-pids[which(pids$ProjectDelivered>'1/1/2018'),]
  pids5<-pids3[which(pids3$EngineeringGroup %in% desired_eg),]
  
  ##join the merge table with the pids table
  SQLQuery1 <- "SELECT p.DeliveryNumber
  ,p.EngineeringGroup
  ,p.ProjectDelivered
  ,p.DeploymentSeverity
  ,w.SourceSystemID
  ,w.Severity
  ,w.SourceSystemCode
  ,w.Description

  
  FROM pids5 p
  LEFT JOIN incidents w
  ON p.DeliveryNumber = w.DeliveryNumber"
  
  pids7 <- sqldf(SQLQuery1)
  
  
  ##print output
  write.csv(pids7,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/check_severity_output.csv")
  
  
}