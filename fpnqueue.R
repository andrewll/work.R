fpnqueue<-function(){
  
  ####################
  ##
  ## To count the number of active and incoming projects in the FPN queue by EG, and list them in detail
  ##
  ####################
  
  
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
  
  ##setup special variables
  todaysdate<-today()
  desired_eg<-c("Azure AD (MSODS)"
                ,"Azure Key Vault"
                ,"Core Apps"
                ,"FOPE"
                ,"O365 SharePoint"
                ,"Skype")
  desired_pg<-c("BOSG - Federal SharePoint "
                ,"BOSG - SPO-S"
                ,"BOSG - SharePoint"
                ,"FASTSearch"
                ,"FOPE"
                ,"FOPE - GSGO"
                ,"PSO KMS"
                ,"PSO MSODS PROD"
                ,"PSO Phone Factor"
                ,"SharePoint Shared MT"
                ,"Skype Platform"
                ,"Skype Plus"
                ,"USNat EOP"
                ,"USNat MSODS"
                ,"USSec EOP"
                ,"USSec MSODS" )
  desired_deploymentclass<-c("New Deployment")
  desired_projects<-c("PRD","EngineeringGroupNetwork","DiscreteServer")
  
  ##set the path to DeploymentPerformance file
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")

  ##define the input files
  file1 <- "projects20190215.csv"
  file5 <- "discovered_dock_date.csv"
  file6 <- "manual_dockdate.csv"
  
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  file_loc5 <- file.path(path, file5)
  file_loc6 <- file.path(path, file6)
  
  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  ddd <- read.csv(file_loc5, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  mdd <- read.csv(file_loc6, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  
  
  ##select only desired columns
  pids03<-subset(pids, select = c("DeliveryNumber"
                                  ,"ProjectStatusName"
                                  ,"EngineeringGroup"
                                  ,"PropertyGroup"
                                  ,"DataCenter"
                                  ,"EstimatedRTEGDate"
                                  ,"DeploymentClass"
                                  ,"ProjectCategory"
                                  ,"CurrentCommittedDockMax"
                                  ,"ActualDockMax"
                                  ,"CreationDate"
                                  ,"DemandID"))
  
  ##filter desired engineering groups
  pids04 <- pids03[which(pids03$EngineeringGroup %in% desired_eg),]
  pids05 <- pids04[which(pids04$PropertyGroup %in% desired_pg),]
  
  ##filter for only active projects
  pids07 <- pids05[which(pids05$ProjectStatusName=="InProgress"),]
  
  ##Rename PropertyGroup values to simplify
  pids07$PropertyGroup<-stri_replace_all_fixed(pids07$PropertyGroup," ","")  ##remove all whitespace
  pids07$PropertyGroup<-gsub("BOSG-FederalSharePoint","SharePoint-GSGO",pids07$PropertyGroup)
  pids07$PropertyGroup<-gsub("SharePointSharedMT","SharePoint",pids07$PropertyGroup)
  pids07$PropertyGroup<-gsub("BOSG-SPO-S","SharePoint",pids07$PropertyGroup)
  pids07$PropertyGroup<-gsub("USNat EOP","FOPE - GSGO",pids07$PropertyGroup)
  pids07$PropertyGroup<-gsub("USSec EOP","FOPE - GSGO",pids07$PropertyGroup)
  pids07$PropertyGroup<-gsub("PSOPhoneFactor","MFA",pids07$PropertyGroup)
  pids07$PropertyGroup<-gsub("PSOMSODSPROD","MSODS",pids07$PropertyGroup)
  pids07$PropertyGroup<-gsub("USNat MSODS","MSODS - GSGO",pids07$PropertyGroup)
  pids07$PropertyGroup<-gsub("USSec MSODS","MSODS - GSGO",pids07$PropertyGroup)
  pids07$PropertyGroup<-gsub("PSOKMS","AKV",pids07$PropertyGroup)


  ##filter for new deployments
  pids09<-pids07[which(pids07$DeploymentClass %in% desired_deploymentclass),]
  pids11<-pids09[which(pids09$ProjectCategory %in% desired_projects),]
  
  ##filter on Hardware Docked Network Equipment work order
  ddd_hdne<-ddd[which(ddd$WorkOrderName=="Hardware Docked Network Equipment"),]
  ddd_hdne$EndDate<-as.Date(ddd_hdne$EndDate, format="%m/%d/%Y")
  ddd_hdne03<-ddd_hdne[which(!is.na(ddd_hdne$EndDate)),]
  ddd_hdne05<-subset(ddd_hdne03,select=c("DeliveryNumber"
                                         ,"WorkOrderName"      
                                         ,"WorkOrderStatusName"
                                         ,"StartDate"
                                         ,"EndDate"))
  
  
  ##join data frames to capture discovered dock date
  SQLQuery1 <- "SELECT p.DeliveryNumber
  ,p.EngineeringGroup
  ,p.PropertyGroup
  ,p.DataCenter
  ,p.DeploymentClass
  ,p.ProjectCategory
  ,p.CurrentCommittedDockMax
  ,p.ActualDockMax
  ,p.CreationDate
  ,p.DemandID
  ,w.EndDate
  FROM pids11 p
  LEFT JOIN ddd_hdne05 w 
  ON p.DeliveryNumber = w.DeliveryNumber"
  
  pids14 <- sqldf(SQLQuery1)
  
  ##create single dock date column
  pids14<-mutate(pids14,DockDate=NA)
  pids14$DockDate<-as.Date(pids14$DockDate, format="%m/%d/%Y")
  pids14$ActualDockMax<-as.Date(pids14$ActualDockMax, format="%m/%d/%Y")
  pids14$CurrentCommittedDockMax<-as.Date(pids14$CurrentCommittedDockMax, format="%m/%d/%Y")
  
  ##set manual dock date
  SQLQuery1 <- "SELECT p.DeliveryNumber
  ,p.EngineeringGroup
  ,p.PropertyGroup
  ,p.DataCenter
  ,p.DeploymentClass
  ,p.ProjectCategory
  ,p.CurrentCommittedDockMax
  ,p.ActualDockMax
  ,p.CreationDate
  ,p.DemandID
  ,p.EndDate
  ,p.DockDate
  ,w.ManualDockDate
  FROM pids14 p
  LEFT JOIN mdd w 
  ON p.DeliveryNumber = w.DeliveryNumber"
  
  pids16 <- sqldf(SQLQuery1)
  
  #change manual dock date to date format
  pids16$ManualDockDate<-as.Date(pids16$ManualDockDate, format="%m/%d/%Y")
  
  ##merge manual dock date, and replace Actual dock
  for (j in 1:dim(pids16)[1]){
    if(!is.na(pids16$ManualDockDate[j])){
      pids16$ActualDockMax[j]<-pids16$ManualDockDate[j]
    }
  }
  
  pids18<-subset(pids16,select=c("DeliveryNumber"
                                 ,"EngineeringGroup"
                                 ,"PropertyGroup"
                                 ,"DataCenter"
                                 ,"ProjectCategory"
                                 ,"CurrentCommittedDockMax"
                                 ,"ActualDockMax"
                                 ,"CreationDate"
                                 ,"DemandID"
                                 ,"EndDate"
                                 ,"DockDate"))
  
  ##set dock date for EG Network
  for (j in 1:dim(pids18)[1]){
    if(pids18$ProjectCategory[j]=="EngineeringGroupNetwork"){
      if(!is.na(pids18$EndDate[j])){
        pids18$DockDate[j]<-pids18$EndDate[j]
      }else if(!is.na(pids18$ActualDockMax[j])){
        pids18$DockDate[j]<-pids18$ActualDockMax[j]
      }else pids18$DockDate[j]<-pids18$CurrentCommittedDockMax[j]
    }
  }
  
  #Set dock date for PRD
  for (j in 1:dim(pids18)[1]){
    if(pids18$ProjectCategory[j]=="PRD"){
      if(!is.na(pids18$ActualDockMax[j])){
        pids18$DockDate[j]<-pids18$ActualDockMax[j]
      }else pids18$DockDate[j]<-pids18$CurrentCommittedDockMax[j]
    }
  }
  
  
  ##create pre-dock vs post-dock column
  pids20<-mutate(pids18, Pre_Post_Dock=NA)
  for(j in 1:dim(pids20)[1]){
    if(is.na(pids20$DockDate[j])){
      pids20$Pre_Post_Dock[j]<-c("Unknown Dock Date")
    }else if(pids20$DockDate[j]<todaysdate){
      pids20$Pre_Post_Dock[j]<-c("Post Dock")
    }else pids20$Pre_Post_Dock[j]<-c("Pre Dock")
  }
  
  ##calculate age of wip
  pids22<-mutate(pids20,AgeofWIP=as.numeric(NA))
  for (j in 1:dim(pids22)[1]){
    if(pids22$Pre_Post_Dock[j]=="Unknown Dock Date"){
      pids22$AgeofWIP[j]<-NA
    }else pids22$AgeofWIP[j]<-as.numeric(todaysdate-pids22$DockDate[j])
  }
  
  pids24<-subset(pids22, select=c("DeliveryNumber"
                                  ,"DemandID"
                                  ,"Pre_Post_Dock"
                                  ,"AgeofWIP"
                                  ,"EngineeringGroup"
                                  ,"PropertyGroup"
                                  ,"ProjectCategory"
                                  ,"DataCenter"
                                  ,"DockDate"
                                  ,"EndDate"
                                  ,"CurrentCommittedDockMax"
                                  ,"ActualDockMax"))
  
  ##print sheet
  write.csv(pids24,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/FPN_FIFO_queue.csv")
  
  
 
  
  
  
}
