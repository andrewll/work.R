findcluster()<-function{
  
  #################
  ##
  ## one time script to join data from APGold to MSasset details
  ##
  #################
  
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
  file2 <- "apgoldclustersflattened.csv"
  
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  file_loc2 <- file.path(path, file2)
 
  
  ## read the deployment performance file
  decomdbassets <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = FALSE)
  apgoldclusters <- read.csv(file_loc2, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = FALSE)
  
  ##subset ap gold clusters to just hk2
  apgoldclusters03<-apgoldclusters[which(apgoldclusters$DC=="HK2"),]
  decomdbassets03<-decomdbassets[which(decomdbassets$DCCode=="HK2"),]
  
  ##write ap gold flattened HK2 list
  ##write.csv(apgoldclusters03,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/APGoldLogicalClusters_HK2.csv")
  
  apgoldcluster05<-mutate(apgoldclusters03, ap_colo = paste(DC,colo, sep = ""))
  apgoldcluster07<-mutate(apgoldcluster05, ap_colo_rack = paste(ap_colo, Rack.1, sep="-"))
  apgoldcluster09<-subset(apgoldcluster07, select = c("ap_colo_rack", "ApSku"))
  apgoldcluster11<-unique(apgoldcluster09)
  
  ##join data frames to capture decom PID for each rack
  SQLQuery1 <- "SELECT p.ap_colo_rack
  ,p.ApSku
  ,w.DecommUnitName
  ,w.ProjectId
  
  FROM apgoldcluster11 p
  LEFT JOIN decomdbassets03 w 
  ON p.ap_colo_rack = w.DecommUnitName"
  
  apgoldcluster13 <- sqldf(SQLQuery1)
  
  apgoldcluster15<-unique(apgoldcluster13)
  
  ##write ap gold flattened HK2 list
  write.csv(apgoldcluster15,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/APGoldLogicalClusters_HK2.csv")
  
  
}
