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
  projectid <- readline(prompt="Enter a ProjectID:")
  print(c("Thank you, ProjectID is",projectid,"Proceeding with script"))
  file1 <- "decommdbassets.csv"
  file2 <- paste("apdecomreport",projectid,".csv",sep="")
  ##file2 <- "apdecomreport25216.csv"
  file3 <- paste("decommprojectanddetails",projectid,".csv",sep="")
  ##file3 <- "decommprojectanddetails-25216.csv"
  
  
  
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
  
  ##add servercount to decompid
  decompid02 <- mutate(decompid,servercount=0)
  for (j in 1:dim(decompid02)[1]){
    if(decompid02$decompid_ItemType[j]=="Server"){
      decompid02$servercount[j]<-"1"
    }
  }
  decompid02$servercount <- as.numeric(decompid02$servercount)
  
  ##Extract ProjectID from Apdecommreport (unique)
  ##Get the first value from POD
  ##Extract the DC, colo, and rackname info and reconstruct that into the rackname in asset DF
  ##Subset asset DF using that rackname, extract the value in ProjectID
  
  ##subset assets from decomdb using PID
  decomdbassets3 <- decomdbassets[which(decomdbassets$decomdb_ProjectId %in% projectid),]
  assets_svr01 <-decomdbassets3[which(decomdbassets3$decomdb_ItemType=='Server'),]
  assets_net01 <-decomdbassets3[which(decomdbassets3$decomdb_ItemType=='NetworkDevice'),]
  assets_net02 <-mutate_all(assets_net01, funs(toupper))
  
  ##subset assets from apdecomreport
  ap_net01 <- apdecomreport[which(apdecomreport$apdecomreport_Type=='MiniSwitch'),]
  
  #join network lists
  SQLQuery1 <- "SELECT d.decomdb_DeviceName
  ,d.decomdb_RackName
  ,d.decomdb_NetworkDeviceType
  ,d.decomdb_ProjectId
  ,d.decomdb_Model
  ,e.apdecomreport_Name
  ,e.apdecomreport_Type
  ,e.apdecomreport_SKU
  ,e.apdecomreport_IPAddress
  ,e.apdecomreport_Pod

  FROM assets_net02 d
  LEFT JOIN ap_net01 e
  ON d.decomdb_DeviceName = e.apdecomreport_Name"
  
  assets_net03 <- sqldf(SQLQuery1)
  
  ##print sheet
  write.csv(assets_net03,file=paste("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/decomm_networkswitch_byPID",projectid,".csv",sep="_"))
  
  print(c("Comparing network devices.  First output file generated.  Please stand by for more..."))
  
  #join network lists by APDecomReport
  SQLQuery1 <- "SELECT e.apdecomreport_Name
  ,e.apdecomreport_Type
  ,e.apdecomreport_SKU
  ,e.apdecomreport_IPAddress
  ,e.apdecomreport_Pod
  ,d.decomdb_DeviceName
  ,d.decomdb_RackName
  ,d.decomdb_NetworkDeviceType
  ,d.decomdb_ProjectId
  ,d.decomdb_Model
  
  FROM ap_net01 e  
  LEFT JOIN assets_net02 d
  ON e.apdecomreport_Name = d.decomdb_DeviceName"
  
  assets_net05 <- sqldf(SQLQuery1)
  
  ##print sheet
  write.csv(assets_net05,file=paste("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/decomm_networkswitch_byAPdecomReport",projectid,".csv",sep="_"))
  
  print(c("Comparing network devices.  Second output file generated.  Please stand by for more..."))
  
  ##add count column to server DF
  assets_svr03 <- mutate(assets_svr01, ServerCount = 1)
  
  ##summarize server count by rackname
  assets_svr05 <- assets_svr03 %>%
    group_by(decomdb_RackName) %>%
    summarize(ServerCount = sum(ServerCount)) %>%
    arrange(decomdb_RackName)
  
  ##summarize server count by rackname
  decompid03 <- decompid02 %>%
    group_by(decompid_RackName) %>%
    summarize(decompid_ServerCount = sum(servercount)) %>%
    arrange(decompid_RackName)
  
  ##join with decompid
  SQLQuery1 <- "SELECT d.decompid_RackName
  ,d.decompid_Servercount
  ,e.decomdb_RackName
  ,e.ServerCount
  
  FROM decompid03 d
  LEFT JOIN assets_svr05 e
  ON d.decompid_RackName = e.decomdb_RackName"
  
  assets_svr07 <- sqldf(SQLQuery1)
  
  ##count matching
  ##asset_svr09 <- mutate(assets_svr07,countmatch = "FALSE")
  ##for (j in 1:dim(asset_svr09)[1]){
  ##  if(asset_svr09$decompid_ServerCount[j]==asset_svr09$Servercount[j]){
  ##    asset_svr09$countmatch[j]<-c("TRUE")
  ##  }
  ##}
  
  ##summarize APdecomreport on server count, join that with this table to verify servercount matches
  
  ##print sheet
  write.csv(assets_svr07,file=paste("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/decomm_servercount",projectid,".csv",sep="_"))
  
  print(c("Comparing server count.  Done.  Please check your output files."))
  
}