simulaterteg<-function(){
  
  #########################
  ##
  ## purpose: to run monte carlo simulations of the milestone data for PRD deployments, 
  ## to ultimately help forecast the DM Estimated RTEG date, and to determine which milestones to target first to get down
  ## to a target DTR cycletime
  ##
  #########################
  
  library(dplyr)
  
  EGList <- c("O365 Exchange", "AP", "O365 SharePoint","CRM","XBOX","ISSD (Azure AAD)")
  
  ## read data
  dat1 <- read.csv("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in/DeliveryPerformanceWithMilestone.csv", stringsAsFactors = TRUE)
  
  ##convert dates into correct format
  dat1$RTEGActualDeliveryDate <- as.Date(dat1$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  dat1$Woad.Dock<- as.Date(dat1$Woad.Dock, format = "%m/%d/%Y")
  
  ##convert Delivery Number to correct format
  dat1$DeliveryNumber<-as.character(dat1$DeliveryNumber)
  

  ##remove dots in header names in pids table
  pidsnames <- gsub("\\.","",names(dat1))
  colnames(dat1) <- c(pidsnames)
  
  
  ## select desired variables
  dat2<- subset(dat1, select = c("DeliveryNumber"
                                 ,"DeploymentClass"
                                 ,"EG"
                                 ,"ProjectCategory"
                                 ,"RTEGActualDeliveryDate"
                                 ,"NetworkReadinessValue"
                                 ,"DCReadinessValue"
                                 ,"ProcurementValue"
                                 ,"ReceivingValue"
                                 ,"BoltandEnergizeValue"
                                 ,"PhysicalCablingValue" 
                                 ,"ConfigureVerifyNetworkValue"
                                 ,"OperationalAcceptanceValue"
                                 ,"WoadDock"))
  
  #remove duplicate entries
  dat3<-unique(dat2)
  
  ##extract PRD objects
  dat4<-dat3[which(dat3$ProjectCategory=="PRD"),]
  dat5<-dat4[which(as.character(dat4$EG) %in% EGList),]
  
  ##calculate dock-to-rteg
  dat6<-mutate(dat5, DTR = as.numeric(RTEGActualDeliveryDate - WoadDock), rtegmonthname = format(as.Date(RTEGActualDeliveryDate), "%Y-%m"))
  
  ##mark negative values in DTR as NA
  for(i in 1:nrow(dat6)){
    if(dat6[i,]$DTR<0){
      dat6[i,]$DTR<-c(NA) }   
  }
  
  #write output file
  write.csv(dat6,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/simulaterteg-input-file.csv")
  
  
  
  
}