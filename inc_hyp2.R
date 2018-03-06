inc_hyp2<-function(){
  
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
  file1 <- "DeliveryProjectContentReport.csv"
  ##define SPO pairs
  file5 <- "Incidents_raw.csv"
  
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
  
  ##create column for DTR
  pids9<-mutate(pids7, DTR = as.numeric(ProjectDelivered - ActualDockMax), DTRbucket = "")
  pids11<-pids9[which(!is.na(pids9$DTR)),]
  
  ##create categories for DTR
  for(i in 1:nrow(pids11)){
    if(pids11[i,]$DTR<10)  
      pids11[i,]$DTRbucket<-c("0-9")
    else if(pids11[i,]$DTR<20)
    pids11[i,]$DTRbucket<-c("'10-19")
    else if(pids11[i,]$DTR<30)
      pids11[i,]$DTRbucket<-c("20-29")
    else if(pids11[i,]$DTR>=30)
      pids11[i,]$DTRbucket<-c("30+")
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
  ,w.projActualDockMax
  ,w.projDTRbucket

  FROM incidents2 p
  LEFT JOIN pids11 w
  ON p.DeliveryNumber = w.projDeliveryNumber"
  
  incidents5 <- sqldf(SQLQuery1)
  
  #filter out only rows that match on Delivery Number
  incidents6<-incidents5[which(!is.na(incidents5$projDeliveryNumber)),]
  
  ##determine if incident is pre-dock or post-dock
  incidents7<-mutate(incidents6, preorpostdockstate = "")
  for(i in 1:nrow(incidents7)){
    if(incidents7[i,]$StartDate>=incidents7[i,]$projActualDockMax)  
      incidents7[i,]$preorpostdockstate<-c("Post-dock")
    else incidents7[i,]$preorpostdockstate<-c("Pre-dock")
  }

  ##calc incident duration by DeliveryNumber
  incidents9<-incidents7 %>%
    group_by(preorpostdockstate,projDTRbucket) %>%
    summarize(incidentcount = sum(incidentcount)
              ,incidentduration = sum(incidentduration)
              ,avg_durations = mean(incidentduration)
              ,stdev_durations = sd(incidentduration)
              ,incidentcount = sum(incidentcount))
  
  ##join pids to incidents to filter down to incidents that started after dock date
  
  #############################
  ##filter incidents for specific faultcodes
  #############################
  incidents_target<-c("125012","125136","115359","125120","115087","125066","125337","115010","115619","115728")
  incidents11<-incidents2[which(incidents2$FaultCode %in% incidents_target),]
  
  incidents13<-incidents11 %>%
    group_by(FaultCode) %>%
    summarize(incidentcount = sum(incidentcount),
              avg_duration = mean(incidentduration, na.rm = TRUE),
              st_Dev_duration = sd(incidentduration, na.rm = TRUE))
  
  
  
  ##SQL Join the incident count to the PIDs dataframe
  SQLQuery1 <- "SELECT p.DeliveryNumber
  ,p.EngineeringGroup
  ,p.DTR
  ,p.DTRbucket
  ,w.DeliveryNumber
  ,w.

  
  FROM pids11 p
  LEFT JOIN incidents2 w
  ON p.DeliveryNumber = w.DeliveryNumber"
  
  pids13 <- sqldf(SQLQuery1)
  
  
  #########
  ##
  ## find incidents in 10-19 bucket
  ##
  #########
  pids15<-pids11[which(pids11$DTRbucket=='10-19'),]  ##create list of PIDs in the desired dtr bucket
  pids15dn<-pids15$DeliveryNumber
  incidents15<-incidents2[which(incidents2$DeliveryNumber %in% pids15dn),]
  write.csv(incidents15,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/incidents_10-19bucket.csv")
  #########
  
  #####generic output statements
  write.csv(pids11,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/O365pidsDTR.csv")
  
  write.csv(incidents7,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/incidents.csv")
  
  write.csv(incidents15,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/incidents_10-19bucket.csv")
  
  
  
}