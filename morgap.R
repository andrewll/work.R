morgap<--function(){
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

  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)

  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  
  ##convert fields to proper format
  pids$ActualCloseDate <- as.Date(pids$ActualCloseDate, format = "%m/%d/%Y")
  pids$CreationDate <- as.Date(pids$CreationDate, format = "%m/%d/%Y")
  pids$ActualDockMax <- as.Date(pids$ActualDockMax, format = "%m/%d/%Y")
  
  ##filter out the cancelled
  pids2<-pids[which(pids$ProjectStatusName!="Cancelled"),]
  
  ##filter out Shared Networking
  ##pids3<-pids[which(pids$DeploymentClass=="New Deployment"),]
  ##pids5<-pids3[which(pids3$ProjectCategory=="PRD"),]
  pids5<-pids2[which(pids2$EngineeringGroup=="Shared Networking"),]
  
  ##filter out active PIDs
  pids7<-pids5[which(pids5$ActualCloseDate > '2017-01-01'),]
  
  ##filter out only the MOR PIDs
  morpids<-pids7[grepl("^Fabric \\| MOR",pids7$ProjectTitle),]
  
  ##filter out only the BAsebuild PIDs
  basebuildpids<-pids7[grepl("^Fabric \\| Base Build",pids7$ProjectTitle),]
  
  ##change date field names for morpids
  
  ##extract list of O365 PIDs
  
  ##merge with morpids
  
  ##calculate metrics
  
  ##generate graphs
  
  
  
  
  
  
  
  
  
  
  
  
}