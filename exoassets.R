exoassets<--function(){
  
  ############################
  ##
  ## the purpose of this script is to find the assets (racknames) for EXO PIDs committed in Manganese (or any semester)
  ##
  ############################
  
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
  file1 <- "decommdbassets_exo04092020.csv"
  file2 <- "exodecomnonad.csv"
  file3 <- "decommprojects_exo04032020.csv"
  
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  file_loc2 <- file.path(path, file2)
  file_loc3 <- file.path(path, file3)
  
  ## read the deployment performance file
  decomdbassetsexo <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = FALSE)
  decompid <- read.csv(file_loc2, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = FALSE)
  exostratuspids <- read.csv(file_loc3, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = FALSE)
  
  ##extract PID into list
  ##decompidlist<-decompid[which(decompid$Status=='Execution'),]
  ##decompidlist3 <- unique(decompidlist$PID)
  
  ##extract to just activitystate = decom
  decompid3<-decompid[which(decompid$ActivityState=='Decom'),]
  
  ##create variable with just project id
  desired_pids <- unique(decompid3$ProjectID)
  
  ##filter just on desired_pids
  decompid5<-exostratuspids[which(exostratuspids$ProjectID %in% desired_pids2),]
  
  ##join
  SQLQuery1 <- "SELECT d.ProjectID
  ,d.GFSID
  ,d.Name
  ,d.Type
  ,d.PhysicalRackSku
  ,d.ActivityState
  ,d.Rackname
  ,e.ProjectId
  ,e.ExecutionPhase
  ,e.servercount
  ,e.rackcount

  FROM decompid3 d
  LEFT JOIN exostratuspids e
  ON d.ProjectId = e.ProjectId"
  
  decompid5 <- sqldf(SQLQuery1)
  
  
  
  ##filter list down to just the ones Not complete
  ##decomdbassetsexo3<-decomdbassetsexo[which(decomdbassetsexo$ProjectId %in% decompidlist3),]
  
  ##extract racknames 
  ##exo_racklist_managenese_commit <- unique(decomdbassetsexo3$DecommUnitName)
  
  ##print
  ##print sheet
  write.csv(decompid3,file=paste("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/exo_pidlist.csv",sep="_"))
  
  
  
  
}
