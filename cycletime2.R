cycletime2<-function(){
  
  #################
  ##
  ##  Purpose: calculate cycle time for 3 major phases: PIDCreate-to-POCreate, POCreate-to-POApprove, POApprove-to-Dock, Dock-to-RTEG
  ##  This is a derivative of both the simulaterteg script and the cycletime script
  ##
  #################
  
  
  library(ggplot2)
  library(dplyr)
  library(sqldf)
  library(scales)
  library(reshape2)
  library(lubridate)
  
  ##EGList <- c("O365 Exchange", "AP", "O365 SharePoint","CRM","XBOX","ISSD (Azure AAD)")
  ##EGList <- c("O365 SharePoint","O365 Exchange")
  ##EGList <- c("O365 Exchange")
  EGList <- c("Networking")
  
  ## read data containing milestone and PO information
  dat <- read.csv("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in/DeliveryPerformanceWithMilestone.csv", stringsAsFactors = TRUE)
  
  ##read data containing pidcreate information
  mydf<-read.csv("C:/Users/andrewll/Documents/R/MCIOdata/All/DelCap-Jul1-Oct26-alleg-networkandservers.csv", stringsAsFactors = FALSE)
  
  ##convert Delivery Number to correct format
  dat$DeliveryNumber<-as.character(dat$DeliveryNumber)
  mydf$DeliveryNumber<-as.character(mydf$DeliveryNumber)
  
  ##remove dots in header names in pids table
  pidsnames <- gsub("\\.","",names(dat))
  colnames(dat) <- c(pidsnames)
  
  ##merge the two dataframes
  SQLQuery1 <- "SELECT p.DeliveryNumber
  ,p.ProjectTitle
  ,p.RTEGActualDeliveryDate
  ,p.RequestedDeliveryDate
  ,p.EG
  ,p.ProjectCategory
  ,p.DeploymentClass
  ,w.DemandCreatedDate
  ,w.ProjectCreationDate
  ,w.woadDock2
  ,p.MaxPOCreateDate
  ,p.MaxPOApproveDate
  ,w.DTR1
  FROM dat p
  LEFT JOIN mydf w 
  ON p.DeliveryNumber = w.DeliveryNumber"
  
  pids <- sqldf(SQLQuery1)
  
  ##convert dates into correct format
  pids$RTEGActualDeliveryDate <- as.Date(pids$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  pids$RequestedDeliveryDate<- as.Date(pids$RequestedDeliveryDate, format = "%m/%d/%Y")
  pids$DemandCreatedDate<- as.Date(pids$DemandCreatedDate, format = "%m/%d/%Y")
  pids$ProjectCreationDate<- as.Date(pids$ProjectCreationDate, format = "%m/%d/%Y")
  pids$woadDock2<- as.Date(pids$woadDock2, format = "%m/%d/%Y")
  pids$MaxPOCreateDate<- as.Date(pids$MaxPOCreateDate, format = "%m/%d/%Y")
  pids$MaxPOApproveDate<- as.Date(pids$MaxPOApproveDate, format = "%m/%d/%Y")
  
  ##extract delivered pids from July 1st onwards
  pids2<-pids[which(pids$ProjectCategory=="PRD"),]
  pids3<-pids2[which(pids2$RTEGActualDeliveryDate>'2015-06-30'),]
  
  ##extract for desired EG
  pids4<-pids3[which(pids3$EG %in% EGList),]
  
  ##remove duplicate rows
  pids5<-unique(pids4)
  
  ##remove rows with NA 
  pids6<-pids5[which(!is.na(pids5$woadDock2)),]
  
  ## add calculated variables
  pids7 <- mutate(pids6, Year_Delivered = format(RTEGActualDeliveryDate,"%Y"), 
                  Month_Delivered = format(RTEGActualDeliveryDate, "%Y-%m"),
                  Month_Docked = format(woadDock2, "%Y-%m"),
                  PIDCount = 1)
  
  pids8 <- mutate(pids7, pidcreate_to_pocreate = as.numeric(MaxPOCreateDate - ProjectCreationDate), 
                 pocreate_to_poapprove = as.numeric(MaxPOApproveDate - MaxPOCreateDate),
                 poapprove_to_dock = as.numeric(woadDock2 - MaxPOApproveDate))
  
  ##rearrange variables for easy Excel work
  pids9 <- subset(pids8, select = c("EG"
                                    ,"DeliveryNumber"
                                    ,"ProjectTitle"
                                    ,"ProjectCategory"
                                    ,"DeploymentClass"
                                    ,"ProjectCreationDate"
                                    ,"MaxPOCreateDate"
                                    ,"MaxPOApproveDate"
                                    ,"woadDock2"
                                    ,"RTEGActualDeliveryDate"
                                    ,"PIDCount"
                                    ,"Month_Docked"
                                    ,"Year_Delivered"
                                    ,"Month_Delivered"
                                    ,"pidcreate_to_pocreate"
                                    ,"pocreate_to_poapprove"
                                    ,"poapprove_to_dock"
                                    ,"DTR1"
                                    ))
  pids10 <- arrange(pids9,EG, Year_Delivered, Month_Delivered)
  
  ##remove pids with negative values
  pids11<-pids10[which(pids10$poapprove_to_dock>0),]
  
  ##count the number of instances in each month
  instancecount<-count(pids11, vars = Month_Docked)
  names(instancecount) <- c("Month_Docked","instancecount")
  pids12<-merge(pids11, instancecount, by.x= "Month_Docked", by.y ="Month_Docked")
  
  ##summarize
  pids13<- pids9 %>% 
    group_by(EG, ProjectCategory, Year_Delivered, Month_Delivered) %>%
    summarize(PIDCount = sum(PIDCount), 
              pidcreate_to_pocreate_avg = mean(pidcreate_to_pocreate, na.rm = TRUE), 
              pidcreate_to_pocreate_95th = quantile(pidcreate_to_pocreate, .95, na.rm = TRUE),
              pocreate_to_poapprove_avg = mean(pocreate_to_poapprove, na.rm = TRUE),
              pocreate_to_poapprove_95th = quantile(pocreate_to_poapprove, .95, na.rm = TRUE),
              poapprove_to_dock_avg = mean(poapprove_to_dock, na.rm = TRUE),
              poapprove_to_dock_95th = quantile(poapprove_to_dock, .95, na.rm = TRUE),
              Dock_to_RTEG_Avg = mean(DTR1, na.rm=TRUE),  
              Dock_to_RTEG_95th = quantile(DTR1, .95, na.rm=TRUE)) %>%
    arrange(EG, ProjectCategory, Year_Delivered, Month_Delivered)
  
  
  ##assign output data
  boxplotdata<-pids12
  printtabledata<-pids13
  
  
  ##print table with individual PID details
  write.csv(boxplotdata,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/ouput_cycletime_pid_details.csv")
  
  ##print table with summarized details by EG
  write.csv(printtabledata,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/ouput_cycletime_pid_summaries.csv")
  
  ##plot the box and whisker
  g<-ggplot(data = boxplotdata, aes(x=Month_Docked, y=poapprove_to_dock, fill=EG))
  g+geom_boxplot()+labs(title="POApprove-to-Dock Cycle Time of Pre-Rack Deliveries")
  ##g+geom_boxplot()+labs(title="POApprove-to-Dock Cycle Time of Pre-Rack Deliveries")+geom_text(aes(x = Month_Docked, y = -5, label = instancecount, size = 1),show_guide = FALSE)
  
  
}