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
  ##path2<- paste0("https://microsoft.sharepoint.com/teams/MCIO-CDE/_layouts/15/Doc.aspx?OR=teams&action=edit&sourcedoc={CAA043EF-A908-4A60-811E-4A4074287235}")

  ##define the input files
  file1 <- "vwprojects2019.csv"
  file5 <- "vwworkorder2019.csv" ## for network equipment orders that have blank dock dates
  file6 <- "manual_dockdate.csv" ##for discrete servers where i determine dock dates manually through email
  file7 <- "vwsignaltolivetracking2019.csv" ##for priority stack ranking info from CP
  file8 <- "EGDemandbyMonth-EngGroup.csv"  ##EG demand for non-PNaaS properties
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  file_loc5 <- file.path(path, file5)
  file_loc6 <- file.path(path, file6)
  file_loc7 <- file.path(path, file7)
  file_loc8 <- file.path(path, file8)
  
  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  ddd <- read.csv(file_loc5, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  mdd <- read.csv(file_loc6, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  stltrack <- read.csv(file_loc7, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  egdemandbymonth <- read.csv(file_loc8, header = TRUE,colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  
  ##filter down the prioritystackranking
  stltrack03<-stltrack[which(!is.na(stltrack$PriorityStackRank)),] ##remove null values 
  
  ##change header name to differentiate from pids headers
  egdemandbymonth_names <- gsub("X2019.","",names(egdemandbymonth))
  colnames(egdemandbymonth) <- c(egdemandbymonth_names)
  
  ##cleanup headers
  stltrack_names <- gsub("DeliveryNumber","stl_DeliveryNumber",names(stltrack03))
  colnames(stltrack03) <- c(stltrack_names)
  
    ##select only desired columns
  pids03<-subset(pids, select = c("DeliveryNumber"
                                  ,"MDMID"
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
                                  ,"DemandID"
                                  ,"RequestedDelivery"))
  
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
  ,p.MDMID
  ,p.EngineeringGroup
  ,p.PropertyGroup
  ,p.DataCenter
  ,p.DeploymentClass
  ,p.ProjectCategory
  ,p.CurrentCommittedDockMax
  ,p.ActualDockMax
  ,p.CreationDate
  ,p.DemandID
  ,p.RequestedDelivery
  ,w.EndDate
  FROM pids11 p
  LEFT JOIN ddd_hdne05 w 
  ON p.DeliveryNumber = w.DeliveryNumber"
  
  pids14 <- sqldf(SQLQuery1)
  
  ##create single dock date column and fix formatting
  pids14<-mutate(pids14,DockDate=NA)
  pids14$DockDate<-as.Date(pids14$DockDate, format="%m/%d/%Y")
  pids14$ActualDockMax<-as.Date(pids14$ActualDockMax, format="%m/%d/%Y")
  pids14$CurrentCommittedDockMax<-as.Date(pids14$CurrentCommittedDockMax, format="%m/%d/%Y")
  stltrack03$ETADockDate<-as.Date(stltrack03$ETADockDate, format="%m/%d/%Y")
  
    ##set manual dock date
  SQLQuery1 <- "SELECT p.DeliveryNumber
  ,p.MDMID
  ,p.EngineeringGroup
  ,p.PropertyGroup
  ,p.DataCenter
  ,p.DeploymentClass
  ,p.ProjectCategory
  ,p.CurrentCommittedDockMax
  ,p.ActualDockMax
  ,p.CreationDate
  ,p.DemandID
  ,p.RequestedDelivery
  ,p.EndDate
  ,p.DockDate
  ,w.ManualDockDate

  FROM pids14 p
  LEFT JOIN mdd w 
  ON p.DeliveryNumber = w.DeliveryNumber"
  
  pids16 <- sqldf(SQLQuery1)
  
  #change manual dock date to date format
  pids16$ManualDockDate<-as.Date(pids16$ManualDockDate, format="%m/%d/%Y")
  
  ##manual dock date is when I manually search for a dock date for values that are NULL in vwProjects
  ##projects not managed in MDM will not be in SignalToLiveTracking, so NULL ETADockDate
  ##merge manual dock date, and replace Actual dock
  for (j in 1:dim(pids16)[1]){
    if(!is.na(pids16$ManualDockDate[j])){
      pids16$ActualDockMax[j]<-pids16$ManualDockDate[j]
    }
  }
  
  ##merge stltrack with pids
  ##merge prioritystackranking with main DF
  SQLQuery1 <- "SELECT p.DeliveryNumber
  ,p.MDMID
  ,p.DemandID
  ,p.EngineeringGroup
  ,p.PropertyGroup
  ,p.ProjectCategory
  ,p.DataCenter
  ,p.DockDate
  ,p.EndDate
  ,p.CurrentCommittedDockMax
  ,p.ActualDockMax
  ,p.RequestedDelivery
  ,w.stl_DeliveryNumber
  ,w.ETADockDate
  ,w.PriorityStackRank
  ,w.ProjectTitle
  ,w.NeedByRTEGDate
  ,w.PlanRTEGDate
  ,w.NewTechFlag
  ,w.IsNPI
  
  FROM pids16 p
  LEFT JOIN stltrack03 w 
  ON p.DeliveryNumber = w.stl_DeliveryNumber"
  
  pids17 <- sqldf(SQLQuery1)
  
  
  pids18<-subset(pids17,select=c("DeliveryNumber"
                                 ,"MDMID"
                                 ,"PriorityStackRank"
                                 ,"EngineeringGroup"
                                 ,"PropertyGroup"
                                 ,"DataCenter"
                                 ,"ProjectCategory"
                                 ,"CurrentCommittedDockMax"
                                 ,"ActualDockMax"
                                 ,"DemandID"
                                 ,"EndDate"
                                 ,"ETADockDate"
                                 ,"DockDate"
                                 ,"RequestedDelivery"
                                 ,"NeedByRTEGDate"
                                 ,"PlanRTEGDate"
                                 ,"NewTechFlag"
                                 ,"IsNPI"))
  
  ##set dock date for EG Network
  for (j in 1:dim(pids18)[1]){
    if(pids18$ProjectCategory[j]=="EngineeringGroupNetwork"){
      if(!is.na(pids18$EndDate[j])){
        pids18$DockDate[j]<-pids18$EndDate[j]
      }else if(!is.na(pids18$ActualDockMax[j])){
        pids18$DockDate[j]<-pids18$ActualDockMax[j]
      }else if(!is.na(pids18$ETADockDate[j])){
        pids18$DockDate[j]<-pids18$ETADockDate[j]
        }else pids18$DockDate[j]<-pids18$CurrentCommittedDockMax[j]
    }
  }
  
  #Set dock date for PRD
  for (j in 1:dim(pids18)[1]){
    if(pids18$ProjectCategory[j]=="PRD"){
      if(!is.na(pids18$ActualDockMax[j])){
        pids18$DockDate[j]<-pids18$ActualDockMax[j]
      }else if(!is.na(pids18$ETADockDate[j])){
        pids18$DockDate[j]<-pids18$ETADockDate[j]
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
  
  pids24<-subset(pids22, select=c("PriorityStackRank"
                                  ,"AgeofWIP"
                                  ,"DeliveryNumber"
                                  ,"DemandID"
                                  ,"MDMID"
                                  ,"Pre_Post_Dock"
                                  ,"EngineeringGroup"
                                  ,"PropertyGroup"
                                  ,"ProjectCategory"
                                  ,"DataCenter"
                                  ,"DockDate"
                                  ,"RequestedDelivery"
                                  ,"NeedByRTEGDate"
                                  ,"PlanRTEGDate"
                                  ,"NewTechFlag"
                                  ,"IsNPI"))
  
  ##set NPIflag for null to FALSE
  for(j in 1:dim(pids24)[1]){
    if(is.na(pids24$IsNPI[j])){
      pids24$IsNPI[j]<-c("FALSE")
    }
  }
  
  
  ##remove NPI
  pids26<-pids24[which(pids24$IsNPI=="FALSE"),]
  

  
  
  ##print sheet
  write.csv(pids26,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/FPN_FIFO_queue.csv")
  
  ##clear dataframe of unwanted rows: egdemandbymonth
  ##egdbm<-egdemandbymonth[-c(1,3,4,6,8,9,10,12,13,14,15),]
  ##egdbm03<-subset(egdbm, select=c("EngineeringGroup","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  ##egdbm05<-egdbm03 %>%
  ##  mutate(Total = select(.,Jan:Dec) %>% colSums(na.rm = TRUE))
 
  
  ##print sheet
  ##write.csv(egdbm03,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/egdbm.csv")
  
  
}
