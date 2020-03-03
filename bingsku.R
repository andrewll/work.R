bingsku<-function(){
  
  ######################
  ## The purpose of this script is to determine what the active Bing SKUs are
  ##
  ######################
  
  
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
  
  ##set file name
  file1<-"decommprojects.csv"
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  
  ##set variables
  desired_eg<-c("Bing")
  desired_status<-c("Execution","Forecasted","Planned","Planning")
  
  ## download the data
  decomprojects <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = FALSE)
  
  ##filter to desired EG and status
  decomprojects3<-decomprojects[which(decomprojects$EG=="Bing"),]
  decomprojects5<-decomprojects3[which(decomprojects3$Status %in% desired_status),]
  decomprojects7<-decomprojects5[which(decomprojects5$UnitType=="Container"),]
  
  ##get just the skus
  bingsku<-sort(unique(decomprojects7$SKU))
  
  ##print sheet
  write.csv(bingsku,file=paste("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/bingsku.csv"))
  
  
  
}