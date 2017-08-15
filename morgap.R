morgap<--function(){
  library(ggplot2)
  library(dplyr)
  library(sqldf)
  library(scales)
  library(reshape2)
  library(lubridate)
  library(stringi)
  
  # basic set up clear all existing variables 
  rm(list = ls(all=T))
  
  #setup EG variable for lookup
  EG<-c("FOPE","O365 Exchange","O365 Lync","O365 SharePoint","O365 Other")
  
  ##set the path to DeploymentPerformance file
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")
  

  ##define the deloyments file
  file1 <- "DeliveryProjectContentReport.csv"

  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)

  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = c("","NA"), stringsAsFactors = TRUE)
  
  ##convert fields to proper format
  pids$ActualCloseDate <- as.Date(pids$ActualCloseDate, format = "%m/%d/%Y")
  pids$CreationDate <- as.Date(pids$CreationDate, format = "%m/%d/%Y")
  pids$ActualDockMax <- as.Date(pids$ActualDockMax, format = "%m/%d/%Y")
  
  ##filter out the cancelled
  pids2<-pids[which(pids$ProjectStatusName!="Cancelled"),]
  
  ##filter on Shared Networking
  pids5<-pids2[which(pids2$EngineeringGroup=="Shared Networking"),]
  
  ##filter out PIDs for time frame desired
  pids7<-pids5[which(pids5$CreationDate > '2016-08-01'),]
  
  ##filter out only the MOR PIDs
  morpids<-pids7[grepl("^Fabric \\| M",pids7$ProjectTitle),]
  
  ##change date field names for morpids
  mornames <- gsub("^","mor",names(morpids))
  colnames(morpids) <- c(mornames)
  
  ##filter out only the BAsebuild PIDs
  basebuildpids<-pids7[grepl("^Fabric \\| B",pids7$ProjectTitle),]
  
  ##extract list of O365 PIDs
  pids9<-pids2[which(pids2$DeploymentClass=="New Deployment"),]
  pids11<-pids9[which(pids9$EngineeringGroup %in% EG),]
  
  ##isolate the MOR tags for each O365 PIDs
  pids13<-pids11[which(!is.na(pids11$Tags)),]
  pids15<-pids13[grepl("MORPID",pids13$Tags),]
  pids17<-mutate(pids15, assocmorpid = "")
  pids17$assocmorpid<-stri_sub(unlist(stri_match_first_regex(pids17$Tags, "MORPID=[0-9]{6}")),from=8)
  
  
  ##merge O365 PIDs with morpids
  SQLQuery1 <- "SELECT p.DeliveryNumber
  ,p.EngineeringGroup
  ,p.RequestedDelivery
  ,p.CommittedDelivery
  ,p.ProjectDelivered
  ,p.ActualDockMax
  ,p.assocmorpid
  ,w.morDeliveryNumber
  ,w.morCreationDate
  ,w.morActualDockMax
  ,w.morProjectDelivered

  FROM pids17 p
  LEFT JOIN morpids2 w
  ON p.assocmorpid = w.morDeliveryNumber"
  
  mergedpids <- sqldf(SQLQuery1)
  
  
  
  
  ##calculate metrics
  
  ##generate graphs
  
  
  
  
  
  
  
  
  
  
  
  
}
