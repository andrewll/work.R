decommaudit<-function(){
  
  #####################
  ##
  ##  This script reconciles the logical decom assets to physical assets.  If they match, then
  ##  we are clear to proceed to physical decom.
  ##
  #####################
  
  library(ggplot2)
  library(dplyr)
  library(sqldf)
  library(scales)
  library(reshape2)
  library(lubridate)
  library(stringi)
  library(igraph)
  
  # basic set up clear all existing variables 
  rm(list = ls(all=T))
  
  ##set the path to DeploymentPerformance file
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")
  
  
  ##define the deloyments file
  file1 <- "decommdbassets.csv"
  file2 <- "apdecomreport.csv"
  file3 <- "decommunittable23517.csv"
  projectid <- "23517"
  
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  file_loc2 <- file.path(path, file2)
  file_loc3 <- file.path(path, file3)
  
  ## read the deployment performance file
  decomdbassets <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = FALSE)
  apdecomreport <- read.csv(file_loc2, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = FALSE)
  decompid <- read.csv(file_loc3, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = FALSE)
  
  ##change column names for decomdb
  decomdbnames <- gsub("^","decomdb_",names(decomdbassets))
  colnames(decomdbassets) <- c(decomdbnames)
  
  ##change column names for AP decom report
  apnames <- gsub("^","apdecomreport_",names(apdecomreport))
  colnames(apdecomreport) <- c(apnames)
  
  ##change column names for AP decom report
  decompidnames <- gsub("^","decompid_",names(decompid))
  colnames(decompid) <- c(decompidnames)
  
  ##Extract ProjectID from Apdecommreport (unique)
  ##Get the first value from POD
  ##Extract the DC, colo, and rackname info and reconstruct that into the rackname in asset DF
  ##Subset asset DF using that rackname, extract the value in ProjectID
  
  ##subset assets from decomdb using PID
  decomdbassets3 <- decomdbassets[which(decomdbassets$decomdb_ProjectId %in% projectid),]
  assets_svr01 <-decomdbassets3[which(decomdbassets3$decomdb_ItemType=='Server'),]
  assets_net01 <-decomdbassets3[which(decomdbassets3$decomdb_ItemType=='NetworkDevice'),]
  
  ##subset assets from apdecomreport
  ap_net01 <- apdecomreport[which(apdecomreport$apdecomreport_Type=='MiniSwitch'),]
  
  #join network lists
  SQLQuery1 <- "SELECT d.decomdb_DeviceName
  ,d.decomdb_RackName
  ,d.decomdb_NetworkDeviceType
  ,d.decomdb_ProjectId
  ,e.apdecomreport_Name
  ,e.apdecomreport_Type
  ,e.apdecomreport_SKU
  ,e.apdecomreport_IPAddress

  FROM assets_net01 d
  LEFT JOIN ap_net01 e
  ON d.decomdb_DeviceName = e.apdecomreport_Name"
  
  assets_net03 <- sqldf(SQLQuery1)
  
  ##print sheet
  write.csv(assets_net03,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/decomm_networkswitch_matching.csv")
  
  ##add count column to server DF
  assets_svr03 <- mutate(assets_svr01, ServerCount = 1)
  
  ##summarize server count by rackname
  assets_svr05 <- assets_svr03 %>%
    group_by(decomdb_RackName) %>%
    summarize(ServerCount = sum(ServerCount)) %>%
    arrange(decomdb_RackName)
  
  ##join with decompid
  SQLQuery1 <- "SELECT d.decompid_Name
  ,d.decompid_ServerCount
  ,d.decompid_ProjectId
  ,e.decomdb_RackName
  ,e.ServerCount

  
  FROM decompid d
  LEFT JOIN assets_svr05 e
  ON d.decompid_Name = e.decomdb_RackName"
  
  assets_svr07 <- sqldf(SQLQuery1)
  
  ##print sheet
  write.csv(assets_svr07,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/decomm_servercount.csv")
  
}