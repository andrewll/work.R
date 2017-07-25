foodbank<--function(){
  
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
  file1 <- "client_list_hackathon_0724_V2.csv"
  
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  
  ## read the deployment performance file
  clients <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  
  
  
  
  
  
}