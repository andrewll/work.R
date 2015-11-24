cycletime3<-function(){
  
  #################
  ##
  ##  Purpose: calculate cycle time for 4 major phases: PIDCreate-to-POCreate, POCreate-to-POApprove, POApprove-to-Dock, Dock-to-RTEG
  ##  This is a derivative of both the simulaterteg script and the cycletime script.
  ##
  ##  This script is a derivative of cycletime2, but with active pids.
  ##
  ##  Input file is the Dera ProjectDeliveries extended spreadsheet
  ##
  #################
  
  
  library(ggplot2)
  library(dplyr)
  library(sqldf)
  library(scales)
  library(reshape2)
  library(lubridate)
  
  ##EGList <- c("O365 Exchange", "AP", "O365 SharePoint","CRM","XBOX","ISSD (Azure AAD)")
  EGList <- c("O365 SharePoint")
  ##EGList <- c("O365 Exchange")
  ##EGList <- c("Networking")
  
  ##read data 
  mydf<-read.csv("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in/DeliveryProjectStatusReport.csv", stringsAsFactors = FALSE)
  
  ##remove dots in header names in pids table
  pidsnames <- gsub("\\.","",names(mydf))
  colnames(mydf) <- c(pidsnames)
  
  ##extract only active pids
  mydf$RTEGActualDeliveryDate <- as.Date(mydf$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  myactive<-mydf[which(is.na(mydf$RTEGActualDeliveryDate)),]
  
  
  ##convert Delivery Number to correct format
  myactive$DeliveryNumber<-as.character(myactive$DeliveryNumber)
  
  ##select desired variables
  pids<-subset(myactive, select = c("DeliveryNumber"
                                    ,"EG"
                                    ,"DeploymentClass"
                                    ,"ProjectCategory"  
                                    ,"DemandCreatedDate"
                                    ,"ProjectCreationDate"
                                    ,"RTEGActualDeliveryDate"
                                    ,"POCreatedDate"
                                    ,"POApprovedDate" 
                                    ,"WorkOrderActualDockDate"
                                    ,"ReceivingDate"
                                    ,"DMEstimatedRTEGDate"))
  

  
  ##convert dates into correct format
  pids$RTEGActualDeliveryDate <- as.Date(pids$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  pids$DMEstimatedRTEGDate <- as.Date(pids$DMEstimatedRTEGDate, format = "%m/%d/%Y")
  pids$DemandCreatedDate<- as.Date(pids$DemandCreatedDate, format = "%m/%d/%Y")
  pids$ProjectCreationDate<- as.Date(pids$ProjectCreationDate)
  pids$ReceivingDate<- as.Date(pids$ReceivingDate, format = "%m/%d/%Y")
  pids$WorkOrderActualDockDate<- as.Date(pids$WorkOrderActualDockDate, format = "%m/%d/%Y")
  pids$POCreatedDate<- as.Date(pids$POCreatedDate, format = "%m/%d/%Y")
  pids$POApprovedDate<- as.Date(pids$POApprovedDate, format = "%m/%d/%Y")
  
  ##extract delivered pids from July 1st onwards
  pids2<-pids[which(pids$ProjectCategory=="PRD"),]
  pids3<-pids2[which(pids2$WorkOrderActualDockDate>'2015-01-01'),]
  ##pids3<-pids2[which(pids2$RTEGActualDeliveryDate>'2015-06-30'),]
  
  ##extract for desired EG
  pids4<-pids3[which(pids3$EG %in% EGList),]
  
  ##remove duplicate rows
  pids5<-unique(pids4)
  
  ##remove rows with NA 
  pids6<-pids5[which(!is.na(pids5$WorkOrderActualDockDate)),]
  
  ## add calculated variables
  pids7 <- mutate(pids6, Year_Delivery_Est = format(DMEstimatedRTEGDate,"%Y"), 
                  Month_Delivery_Est = format(DMEstimatedRTEGDate, "%Y-%m"),
                  Month_Docked = format(WorkOrderActualDockDate, "%Y-%m"),
                  PIDCount = 1)
  
  pids8 <- mutate(pids7, demandcreate_to_pidcreate = as.numeric(ProjectCreationDate - DemandCreatedDate),
                  pidcreate_to_pocreate = as.numeric(POCreatedDate - ProjectCreationDate), 
                  pocreate_to_poapprove = as.numeric(POApprovedDate - POCreatedDate),
                  poapprove_to_dock = as.numeric(WorkOrderActualDockDate - POApprovedDate),
                  dock_to_rteg_est = as.numeric(DMEstimatedRTEGDate - WorkOrderActualDockDate))
  
  ##rearrange variables for easy Excel work
   pids10 <- arrange(pids8,EG, Year_Delivery_Est, Month_Delivery_Est)
  
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
              Dock_to_RTEG_Avg = mean(DockToRTEG, na.rm=TRUE),  
              Dock_to_RTEG_95th = quantile(DockToRTEG, .95, na.rm=TRUE)) %>%
    arrange(EG, ProjectCategory, Year_Delivered, Month_Delivered)
  
  
  ##assign output data
  boxplotdata<-pids12
  printtabledata<-pids13
  
  
  ##print table with individual PID details
  write.csv(boxplotdata,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/ouput_cycletime_activepid_details.csv")
  
  ##print table with summarized details by EG
  write.csv(printtabledata,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/ouput_cycletime_activepid_summaries.csv")
  
  ##plot the box and whisker
  g<-ggplot(data = boxplotdata, aes(x=Month_Docked, y=poapprove_to_dock, fill=EG))
  g+geom_boxplot()+labs(title="POApprove-to-Dock Cycle Time of Pre-Rack Deliveries")
  ##g+geom_boxplot()+labs(title="POApprove-to-Dock Cycle Time of Pre-Rack Deliveries")+geom_text(aes(x = Month_Docked, y = -5, label = instancecount, size = 1),show_guide = FALSE)
  
  

  
}