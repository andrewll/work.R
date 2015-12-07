lorindalist<-function(){
  
  ##to generate date file for Lorinda's delivered pids
  
  ##load libraries
  library(ggplot2)
  library(dplyr)
  library(sqldf)
  library(scales)
  library(reshape2)
  library(lubridate)
  
  ##read data file
  pids<-read.csv("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in/DeliveryProjectStatusReport.csv", stringsAsFactors = FALSE)
  
  ##read Lorinda's list of pids
  ##lorindalist1<-read.csv("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in/lorindalistofpids-20151204.csv", stringsAsFactors = FALSE)
  lorindalist3<-c("435856", "435857", "435858", "435859", "435860", "435863", "429215", "429218", "429732", "429736", "430773", "439726")
  
  pids$RTEGActualDeliveryDate <- as.Date(pids$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  pids$DMEstimatedRTEGDate <- as.Date(pids$DMEstimatedRTEGDate, format = "%m/%d/%Y")
  pids$DemandCreatedDate<- as.Date(pids$DemandCreatedDate, format = "%m/%d/%Y")
  pids$ProjectCreationDate<- as.Date(pids$ProjectCreationDate, format = "%m/%d/%Y")
  pids$ReceivingDate<- as.Date(pids$ReceivingDate, format = "%m/%d/%Y")
  pids$WorkOrderActualDockDate<- as.Date(pids$WorkOrderActualDockDate, format = "%m/%d/%Y")
  pids$POCreatedDate<- as.Date(pids$POCreatedDate, format = "%m/%d/%Y")
  pids$POApprovedDate<- as.Date(pids$POApprovedDate, format = "%m/%d/%Y")
  
  pids5<-subset(pids, select = c("DeliveryNumber"
                                 ,"EG"
                                 ,"PropertyGroup"
                                 ,"DPM"
                                 ,"ProjectTitle" 
                                 ,"DeploymentClass"
                                 ,"ProjectCategory"  
                                 ,"DemandCreatedDate"
                                 ,"ProjectCreationDate"
                                 ,"RTEGActualDeliveryDate"
                                 ,"POCreatedDate"
                                 ,"POApprovedDate" 
                                 ,"WorkOrderActualDockDate"
                                 ,"ReceivingDate"
                                 ,"DMEstimatedRTEGDate"
                                 ,"DockToRTEG"))
  
  ##extract only the pids from lorinda's list
  pidslorinda<-pids5[which(pids5$DeliveryNumber %in% lorindalist3),]
  
  ##calculate new fields
  pidslorinda3 <- mutate(pidslorinda, Year_Delivered = format(RTEGActualDeliveryDate,"%Y"), 
                  Month_Delivered = format(RTEGActualDeliveryDate, "%Y-%m"))
  
  pidslorinda5 <- mutate(pidslorinda3, demandcreate_to_pidcreate = as.numeric(ProjectCreationDate - DemandCreatedDate),
                   pidcreate_to_pocreate = as.numeric(POCreatedDate - ProjectCreationDate), 
                   pocreate_to_poapprove = as.numeric(POApprovedDate - POCreatedDate),
                   poapprove_to_dock = as.numeric(WorkOrderActualDockDate - POApprovedDate),
                   dock_to_rteg = as.numeric(DockToRTEG))
  
  
  ##write to CSV file
  write.csv(pidslorinda5,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/pidslorinda.csv")
  
  
  
}