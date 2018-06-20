inc_hyp3<--function(){
  
  
  ###########################
  ##
  ##
  ###########################
  
  
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
  
  ##define the datasources
  file1 <- "DeliveryProjectContentReport.csv"
  file5 <- "incidents_raw.csv"
  
  desired_eg<-c("O365 SharePoint","O365 Exchange","FOPE","OneDrive")
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  #define the incidents file path
  file_loc5 <- file.path(path, file5)
  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  incidents <- read.csv(file_loc5, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  
  pids$ActualDockMax <- as.Date.character(pids$ActualDockMax, format = "%m/%d/%Y")
  pids$ProjectDelivered <- as.Date.character(pids$ProjectDelivered, format = "%m/%d/%Y")
  incidents$EndDate<-as.Date(incidents$EndDate, format = "%m/%d/%Y")
  incidents$StartDate<-as.Date(incidents$StartDate, format = "%m/%d/%Y")
  
  ##fix colum names
  pidsnames <- gsub("\\.","",names(pids))
  colnames(pids) <- c(pidsnames)
  
  ##filter down to desired pids
  pids3<-pids[which(pids$DeploymentClass=="New Deployment"),]
  pids5<-pids3[which(pids3$ProjectCategory=="PRD"),]
  pids7<-pids5[which(pids5$EngineeringGroup %in% desired_eg),]
  pids8<-pids7[which(!is.na(pids7$ProjectDelivered)),]
  pids9<-pids8[which(pids8$ProjectDelivered>'2017-12-31'),]
  
  ##create column for DTR
  pids10<-mutate(pids9, DTR = as.numeric(ProjectDelivered - ActualDockMax), DTRbucket = "", pidscount = as.numeric(1))
  pids11<-pids10[which(!is.na(pids10$DTR)),]
  
  ##create categories for DTR
  for(i in 1:nrow(pids11)){
    if(pids11[i,]$DTR<10)  
      pids11[i,]$DTRbucket<-c("'0-9")
    else if(pids11[i,]$DTR<20)
      pids11[i,]$DTRbucket<-c("'10-19")
    else if(pids11[i,]$DTR<30)
      pids11[i,]$DTRbucket<-c("'20-29")
    else if(pids11[i,]$DTR>=30)
      pids11[i,]$DTRbucket<-c("'30+")
  }
  
  ##convert column names
  pids11names <- gsub("^","proj",names(pids11))
  colnames(pids11) <- c(pids11names)
  
  ##count incidents per project
  incidents2<-mutate(incidents, incidentduration = as.numeric(EndDate - StartDate),incidentcount = 1)
  
  ##filter list to just post-dock incidents
  SQLQuery1 <- "SELECT 
  p.DeliveryNumber
  ,p.StartDate
  ,p.EndDate
  ,p.incidentduration
  ,p.incidentcount
  ,p.SourceSystemID
  ,p.FaultCode
  ,p.CodeDescription
  ,p.Description
  ,w.projDeliveryNumber
  ,w.projEngineeringGroup
  ,w.projProjectDelivered
  ,w.projActualDockMax
  ,w.projDTRbucket
  
  FROM incidents2 p
  LEFT JOIN pids11 w
  ON p.DeliveryNumber = w.projDeliveryNumber"
  
  incidents5 <- sqldf(SQLQuery1)
  
  #filter out only rows that match on Delivery Number
  incidents6<-incidents5[which(!is.na(incidents5$projDeliveryNumber)),]
  
  ##sample data
  ##preloopdelnumber<-c("486920","487112")
  incidents7<-incidents6[which(incidents6$DeliveryNumber=='486920'),]
  

  
  ##start to calc Critical Path Impact (CPI)
  for (i in 1:nrow(incidents7)){
    ##setup variable to loop through projects
    loopdeliverynumber<-incidents7[i,]$DeliveryNumber
    ##create new dataframe
    loopdf1<-incidents7[which(incidents7$DeliveryNumber %in% loopdeliverynumber),]
    ctd<-loopdf1[1,]$projActualDockMax  ##setup a first use variable
    for(p in 1:nrow(loopdf1)){
      while(loopdf1[p,]$DeliveryNumber==loopdeliverynumber){
        loopdf1<-loopdf1[with(loopdf1,order(StartDate,EndDate)),] ##sort the dataframe by StartDate
        ##setup counter variables
        min_startdate<-loopdf1[p,]$StartDate
        max_enddate<-loopdf1[p,]$EndDate
        totalduration<-as.numeric(max_enddate-min_startdate)
        if(loopdf1[p,]$StartDate>max_enddate){
          totalduration<-totalduration+as.numeric(loopdf1[p,]$EndDate-loopdf1[p,]$StartDate)
          max_enddate<-loopdf1[p,]$EndDate
        }
        
      }
      
    }
    
  }
}