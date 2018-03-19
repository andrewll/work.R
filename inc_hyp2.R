inc_hyp2<-function(){
  
  ###########################
  ##
  ## script for analyzing incidents in deployments
  ## data source (server):  deraazuresql01.database.windows.net
  ## database name:  derareport
  ## view name:  l3view.vwIncidents
  ##
  ## filters:  
  ##   FY18, New Deployments, PRD, RTEG'd, post-dock, pre-RTEG, only for EG = "O365 SharePoint","O365 Exchange","FOPE","OneDrive"
  ##
  ## output: CSV
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
  
  ##determine if incident is pre-dock or post-dock
  incidents7<-mutate(incidents6, preorpostdockstate = "")
  for(i in 1:nrow(incidents7)){
    if(incidents7[i,]$StartDate>=incidents7[i,]$projActualDockMax)  
      incidents7[i,]$preorpostdockstate<-c("Post-dock")
    else incidents7[i,]$preorpostdockstate<-c("Pre-dock")
  }
  
  ##join pids to incidents to filter down to incidents that started after dock date
  incidents9<-incidents7[which(incidents7$preorpostdockstate=="Post-dock"),]
  
  ##exclude incidents with StartDate greater than ProjectDelivered date
  incidents11<-incidents9[which(incidents9$StartDate<incidents9$projProjectDelivered),]
  incidents12<-incidents11[which(!is.na(incidents11$incidentduration)),]
  

  ##calc incident duration by DeliveryNumber
  incidents13<-incidents12 %>%
    group_by(projEngineeringGroup) %>%
    summarize(projectcount=count(unique(projDeliveryNumber))
              ,incidentcount = sum(incidentcount)
              ,avg_duration = mean(incidentduration, na.rm = TRUE)
              ,stdev = sd(incidentduration, na.rm = TRUE)
              ,sumduration = sum(incidentduration)
              
  pids13<-pids11 %>%
    group_by(projEngineeringGroup) %>%
    summarize(projectcount=sum(projpidscount))
  
  #############################
  ##filter incidents for specific faultcodes
  #############################
  ##incidents_target<-c("125012","125136","115359","125120","115087","125066","125337","115010","115619","115728")
  ##incidents11<-incidents2[which(incidents2$FaultCode %in% incidents_target),]
  
  ##incidents13<-incidents11 %>%
  ##  group_by(FaultCode) %>%
  ##  summarize(incidentcount = sum(incidentcount),
  ##            avg_duration = mean(incidentduration, na.rm = TRUE),
  ##            st_Dev_duration = sd(incidentduration, na.rm = TRUE))
  
  
  #########
  ##
  ## find incidents in 10-19 bucket
  ##
  #########
  ##pids15<-pids11[which(pids11$DTRbucket=='10-19'),]  ##create list of PIDs in the desired dtr bucket
  ##pids15dn<-pids15$DeliveryNumber
  ##incidents15<-incidents2[which(incidents2$DeliveryNumber %in% pids15dn),]
  ##write.csv(incidents15,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/incidents_10-19bucket.csv")
  #########
  
  #####generic output statements
  ##  raw pids data
  write.csv(pids11,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/O365pidsDTR.csv")
  ##  raw incidents data
  write.csv(incidents11,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/incidents.csv")
  ## summarized incidents data
  write.csv(incidents13,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/incidents_summarized.csv")
  
  
  
}