clusterpair<-function(){
  
  library(ggplot2)
  library(dplyr)
  library(sqldf)
  library(scales)
  library(reshape2)
  library(lubridate)
  
  
  # basic set up clear all existing variables 
  rm(list = ls(all=T))
  
  ##set the path to DeploymentPerformance file
  ##path <- paste0("C:/Users/answami/Documents",
  ##               "/WindowsPowerShell/Scripts/Deployments")
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")
  
  ##setup special variables
  ##go_locals<-c("457404","457405","458216","458217","458967","458968","458964","458965","458966")
  go_locals<-c("458216","458217")
  todaysdate<-today()
  desired_eg<-c("O365 SharePoint", "O365 Exchange","FOPE","OneDrive")
  
  
  ##define the deloyments file
  file1 <- "DeliveryProjectContentReport.csv"
  ##define SPO pairs
  file5 <- "spo-pairing-fy16.csv"
  
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  ##define the spo pairs file path
  file_loc5 <- file.path(path, file5)

  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  
  ## read the waves table
  spopairs <- read.csv(file_loc5,
                       header = TRUE, colClasses = NA, na.strings = "N/A", stringsAsFactors = TRUE)
  
  ##convert spopairs DeliveryNumber to character
  spopairs$DeliveryNumber<-as.character(spopairs$DeliveryNumber)
  pids$DeliveryNumber<-as.character(pids$DeliveryNumber)
  
  ##remove dots in header names in pids table
  pidsnames <- gsub("\\.","",names(pids))
  colnames(pids) <- c(pidsnames)
  
  spopairnames <-gsub("\\.","",names(spopairs))
  colnames(spopairs) <-c(spopairnames)
  
  pids3<-pids[which(pids$DeploymentClass=="New Deployment"),]
  pids5<-pids3[which(pids3$ProjectCategory=="PRD"),]
  pids7<-pids5[which(pids5$EngineeringGroup %in% desired_eg),]
  
  ##join the merge table with the pids table
  SQLQuery1 <- "SELECT p.DeliveryNumber
  ,p.Pair
  ,p.Region
  ,p.Intent
  ,p.fiscalyear
  ,p.wave
  ,w.ProjectTitle
  ,w.ActualDockMax
  ,w.CurrentCommittedDockMax
  ,w.RequestedDelivery
  ,w.EstimatedRTEGDate
  ,w.CommittedDelivery
  ,w.ProjectDelivered
  ,w.DataCenter
  ,w.PropertyGroup
  ,W.DemandID

  FROM spopairs p
  LEFT JOIN pids7 w
  ON p.DeliveryNumber = w.DeliveryNumber"
  
  pids9 <- sqldf(SQLQuery1)
  
  ##convert dates to date format for pids table
  pids9$ProjectDelivered <- as.Date.character(pids9$ProjectDelivered, format = "%m/%d/%Y")
  pids9$ActualDockMax <- as.Date.character(pids9$ActualDockMax, format = "%m/%d/%Y")
  pids9$CurrentCommittedDockMax <-as.Date.character(pids9$CurrentCommittedDockMax, format = "%m/%d/%Y")
  pids9$EstimatedRTEGDate <- as.Date.character(pids9$EstimatedRTEGDate, format = "%m/%d/%Y")
  pids9$RequestedDelivery <- as.Date.character(pids9$RequestedDelivery,, format = "%m/%d/%Y")
  pids9$CommittedDelivery <- as.Date.character(pids9$CommittedDelivery, format = "%m/%d/%Y")
  
  
  ##subset to just the PIDs in the pairing list
  pids11<-pids9[which(!is.na(pids9$Pair)),]
  pids12<-pids11[which(!is.na(pids11$ProjectTitle)),]
  
  ##format dataframe for output - Region variable and RTEG variable
  pids13<-mutate(pids12, RTEG = as.Date(NA), Status = " ")
  for(i in 1:nrow(pids13)){
    if(is.na(pids13[i,]$ProjectDelivered)) 
      pids13[i,]$RTEG<-pids13[i,]$EstimatedRTEGDate
    else pids13[i,]$RTEG<-pids13[i,]$ProjectDelivered
  }
  
  ##calculate Live date and RTEG month
  pids15<-mutate(pids13, RTGM = RTEG + 14, Live = RTGM + 14, crteg_month = format(CommittedDelivery, "%Y-%m"), 
                 dm_rteg_month = format(EstimatedRTEGDate,"%Y-%m"), wipdays="")
  
  ## set Live and Buildout status 
  for(i in 1:nrow(pids15)){
    if(pids15[i,]$Live<=todaysdate)
      pids15[i,]$Status<-c("Live")
    else if(pids15[i,]$RTGM<=todaysdate)
      pids15[i,]$Status<-c("Buildout")
    else if(pids15[i,]$RTEG<=todaysdate)
      pids15[i,]$Status<-c("Buildout")
    else pids15[i,]$Status<-c("")
  }
  
  ##calculate correct Live date for go_locals
  ##for(i in 1:nrow(pids15)){
  ##  if(pids15[i,]$DeliveryNumber %in% go_locals)
  ##    pids15[i,]$Live<-pids15[i,]$RTEG+30
  ##}
  
  ##calculate Dock-to-RTEG for WIP
  for(i in 1:nrow(pids15)){
    if(!is.na(pids15[i,]$ActualDockMax)) currentwoadDock<-pids15[i,]$ActualDockMax
      else currentwoadDock<-pids15[i,]$CurrentCommittedDockMax
    if(pids15[i,]$RTEG<todaysdate) pids15[i,]$wipdays<-"RTEG'd"
      else
      if(currentwoadDock>todaysdate) pids15[i,]$wipdays<-"Pending Dock"
        else 
          pids15[i,]$wipdays<-as.integer(todaysdate-currentwoadDock)
      }
  
  pids17<-subset(pids15,select = c("fiscalyear","DeliveryNumber","PropertyGroup","DemandID", "Region"
                                  ,"Pair","Status","Intent","RequestedDelivery","RTEG","RTGM","Live", "CommittedDelivery"
                                  ,"ActualDockMax","CurrentCommittedDockMax","DataCenter","wave","wipdays"))
  pids19<-arrange(pids17,fiscalyear,Pair,RTEG)
  pids21<-mutate(pids19, quickhits = "", lagging = "")
                 

  ##print output
  write.csv(pids21,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/output_cluster_pairs.csv")
  
  ##web scraping test
  ##con = url("http://spdeployment.azurewebsites.net/#/deployment/list/4")
  ##htmlCode = readLines(con)
  ##close(con)
  ##htmlCode
  
  
  
  
}
