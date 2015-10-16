simulaterteg<-function(){
  
  #########################
  ##
  ## purpose: to run monte carlo simulations of the milestone data for PRD deployments, 
  ## to ultimately help forecast the DM Estimated RTEG date, and to determine which milestones to target first to get down
  ## to a target DTR cycletime
  ##
  #########################
  
  ## read data
  dat1 <- read.csv("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in/DeliveryPerformanceWithMilestone.csv", stringsAsFactors = TRUE)
  
  ##convert dates into correct format
  dat1$RTEGActualDeliveryDate <- as.Date(dat1$RTEGActualDeliveryDate, format = "%m/%d/%Y")
  
  ## select desired variables
  dat2<- subset(dat1, select = c("DeliveryNumber","DeploymentClass","EG","ProjectCategory","RTEGActualDeliveryDate"))
  
  
  
}