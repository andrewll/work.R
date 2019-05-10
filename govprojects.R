govprojects<-function(){
  
  ################################
  ##
  ##  To collect info and stats on Government deployments
  ##
  ################################
  
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
  
  ##setup special variables
  todaysdate<-today()
  desired_eg<-c("Azure AD (MSODS)"
                ,"Azure Key Vault"
                ,"Core Apps"
                ,"FOPE"
                ,"O365 SharePoint"
                ,"Skype")
  desired_pg<-c("BOSG - Federal SharePoint "
                ,"BOSG - SPO-S"
                ,"BOSG - SharePoint"
                ,"FASTSearch"
                ,"FOPE"
                ,"FOPE - GSGO"
                ,"PSO KMS"
                ,"PSO MSODS PROD"
                ,"PSO Phone Factor"
                ,"SharePoint Shared MT"
                ,"Skype Platform"
                ,"Skype Plus"
                ,"USNat EOP"
                ,"USNat MSODS"
                ,"USSec EOP"
                ,"USSec MSODS" )
  desired_deploymentclass<-c("New Deployment")
  desired_projects<-c("PRD","EngineeringGroupNetwork","DiscreteServer")
  
  ##set the path to DeploymentPerformance file
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")
  
  ##define the input files
  file1 <- "vwprojects2019.csv"
  file7 <- "vwsignaltolivetracking2019.csv" ##for priority stack ranking info from CP
  
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  file_loc7 <- file.path(path, file7)
  
  
  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  stltrack <- read.csv(file_loc7, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  
  
  
  
  
  
  
  
  
}