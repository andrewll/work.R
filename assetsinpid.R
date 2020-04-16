assetsinpid<-function(){
  
  
  ##########################
  ##
  ##  the purpose of this PID is to lookup what racks and devices are included in a PID.
  ##  the PID may have rack names but sometimes we need to know which devices are included
  ##
  ##########################
  
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
  file1 <- "decommdbassets_bingcosmostechops.csv"
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
  
  ##filter to pid
  decomdbassets3<-decomdbassets[which(decomdbassets$ProjectId %in% projectid),]
  
  ## print file
  write.csv(decomdbassets3,file=paste("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/assetsinpid_",projectid,".csv",sep="_"))
  
  
  
}