cycletimemor<-function(){
  
  #######################
  ##
  ##  given a list of MORs, find the cycle time for 4 milestones PidCreate-to-PO, PO-to-POApproval, POapproval-to-dock, dock-to-rteg
  ##  find cycle time overall
  ##  calculate correlations of milestones to overall cycle time
  ##
  #######################
  
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
  file1 <- "DeliveryPerformance.csv"
  ##define abbreviations file
  file2 <- "spo-pairing-fy16.csv"
  ##define input with PO Create and PO Approve dates
  file5 <- "ProjectDetailsbyMilestone.csv"
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  #define spo abbreviation file path
  file_loc2 <-file.path(path, file2)
  ##define the spo pairs file path
  file_loc5 <- file.path(path, file5)
  
  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  
  ##read mor list
  morlist <- read.csv(file_loc2, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  
  ##read pids with PO data
  dat <- read.csv(file_loc5, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  
  
  ##remove dots in header names in pids table
  pidsnames <- gsub("\\.","",names(pids))
  colnames(pids) <- c(pidsnames)
  
  morlistnames <-gsub("\\.","",names(morlist))
  colnames(morlist) <-c(morlistnames)
  
  datnames <- gsub("\\.","",names(dat))
  colnames(dat) <- c(datnames)
  
  ##remove duplicate column
  dat$WoadDock <- NULL
  
  morlist2<-unique(morlist)


  
  ##join the merge table with the pids table
  SQLQuery1 <- "SELECT w.DeliveryNumber
  ,p.EG
  ,p.PropertyGroup
  ,p.DPM
  ,p.ProjectTitle
  ,p.DeploymentClass
  ,p.ProjectCategory
  ,p.DemandCreatedDate
  ,p.ProjectCreationDate
  ,p.RTEGActualDeliveryDate
  ,p.woadPOCreation
  ,p.woadDock
  ,p.RequestedDeliveryDate
  ,p.ReceivingDate
  ,p.DMEstimatedRTEGDate
  ,p.DTR
  FROM morlist2 w
  LEFT JOIN pids p
  ON w.DeliveryNumber = p.DeliveryNumber"
  
  pids3 <- sqldf(SQLQuery1)
  
  ##subset to just the PIDs that were RTEG'ed
  pids5<-pids3[which(!is.na(pids3$RTEGActualDeliveryDate)),]
  
  ##second SQL join to get the Max.PO.Create.Date and Max.PO.Approve.Date 
  SQLQuery2 <- "SELECT p.DeliveryNumber
  ,p.EG
  ,p.PropertyGroup
  ,p.DPM
  ,p.ProjectTitle
  ,p.DeploymentClass
  ,p.ProjectCategory
  ,p.DemandCreatedDate
  ,p.ProjectCreationDate
  ,p.RTEGActualDeliveryDate
  ,p.woadPOCreation
  ,p.woadDock
  ,p.RequestedDeliveryDate
  ,p.ReceivingDate
  ,p.DMEstimatedRTEGDate
  ,p.DTR
  ,w.MaxPOCreateDate
  ,w.MaxPOApproveDate
  ,w.ReceivingValue
  ,w.BoltandEnergizeValue
  ,w.PhysicalCablingValue
  ,w.ConfigureVerifyNetworkValue
  ,w.OperationalAcceptanceValue
  FROM pids5 p
  LEFT JOIN dat w
  ON p.DeliveryNumber = w.DeliveryNumber"
  
  pids7 <- sqldf(SQLQuery2)
  
  ##remove duplicate rows
  pids9<-unique(pids7)
  
  ##convert dates to date format for pids table
  pids9$RTEGActualDeliveryDate <- as.Date(pids9$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  pids9$ReceivingDate <- as.Date(pids9$ReceivingDate, format = "%m/%d/%Y")
  pids9$DemandCreatedDate<- as.Date(pids9$DemandCreatedDate, format = "%m/%d/%Y")
  pids9$woadDock <- as.Date(pids9$woadDock, format = "%m/%d/%Y")
  pids9$woadPOCreation<- as.Date(pids9$woadPOCreation, format = "%m/%d/%Y")
  pids9$DMEstimatedRTEGDate <- as.Date(pids9$DMEstimatedRTEGDate, format = "%m/%d/%Y")
  pids9$RequestedDeliveryDate <- as.Date(pids9$RequestedDeliveryDate, format = "%m/%d/%Y")
  pids9$ProjectCreationDate <- as.Date(pids9$ProjectCreationDate, format = "%m/%d/%Y")
  pids9$MaxPOCreateDate<- as.Date(pids9$MaxPOCreateDate, format = "%m/%d/%Y")
  pids9$MaxPOApproveDate<- as.Date(pids9$MaxPOApproveDate, format = "%m/%d/%Y")
  
  ## add calculated variables
  pids11 <- mutate(pids9, Year_Created = format(ProjectCreationDate,"%Y"), 
                  Month_Created = format(ProjectCreationDate, "%Y-%m"),
                  Month_Docked = format(woadDock, "%Y-%m"),
                  PIDCount = 1)

  pids13 <- mutate(pids11, demandcreate_to_pidcreate = as.numeric(ProjectCreationDate - DemandCreatedDate),
                  pidcreate_to_pocreate = as.numeric(MaxPOCreateDate - ProjectCreationDate), 
                  pocreate_to_poapprove = as.numeric(MaxPOApproveDate - MaxPOCreateDate),
                  poapprove_to_dock = as.numeric(woadDock - MaxPOApproveDate))
  
  ##rearranging columns
  pids15 <- subset(pids13, select = c("EG"
                                    ,"DeliveryNumber"
                                    ,"ProjectTitle"
                                    ,"ProjectCategory"
                                    ,"DeploymentClass"
                                    ,"DemandCreatedDate"
                                    ,"ProjectCreationDate"
                                    ,"MaxPOCreateDate"
                                    ,"MaxPOApproveDate"
                                    ,"woadDock"
                                    ,"RTEGActualDeliveryDate"
                                    ,"PIDCount"
                                    ,"Month_Docked"
                                    ,"Year_Created"
                                    ,"Month_Created"
                                    ,"pidcreate_to_pocreate"
                                    ,"pocreate_to_poapprove"
                                    ,"poapprove_to_dock"
                                    ,"DTR"
                                    ,"ReceivingValue"
                                    ,"BoltandEnergizeValue"
                                    ,"PhysicalCablingValue"
                                    ,"ConfigureVerifyNetworkValue"
                                    ,"OperationalAcceptanceValue"
  ))
  
  #remove NA values
  pids17<-pids15[which(!is.na(pids15$pidcreate_to_pocreate)),]
  pids19<-pids17[which(!is.na(pids17$pocreate_to_poapprove)),]
  
  #calculate pid_to_rteg and dock_to_rteg
  pids19$RTEGActualDeliveryDate <- as.Date(pids19$RTEGActualDeliveryDate, origin=lubridate::origin, format = "%m/%d/%Y")
  pids21<-mutate(pids19, pid_to_rteg=RTEGActualDeliveryDate-ProjectCreationDate)
  
  ##group together for printing
  pids23<-pids21%>%
    group_by(Month_Created)%>%
    summarize(pid_to_rteg_avg=mean(DTR, na.rm=TRUE), pid_to_rteg_95th=quantile(pid_to_rteg, .95, na.rm=TRUE), PIDCount = sum(PIDCount))%>%
    arrange(Month_Created)
  
  ##print output
  write.csv(pids19,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/output_mor_cycletimes.csv")
  write.csv(pids23,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/output_mor_cycletimes_summarized.csv")
  
  
  }