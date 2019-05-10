aggorder<-function(){
  
  ########################################
  ##
  ## To put together the list of projects that go in an aggregated order such as
  ## Go Local initiatives and Government deployments
  ##
  ########################################
  
  
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
  desired_eg<-c("Azure"
                ,"Azure AD (MSODS)"
                ,"Azure Data Lake"
                ,"CloudBuild"
                ,"Core Apps"
                ,"FOPE"
                ,"O365 SharePoint"
                ,"O365 Exchange"
                ,"Search"
                ,"Shared Networking")
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
  desired_plangeo<-c("USDoD","USGov","USNat","USSec")
  
  ##set the path to DeploymentPerformance file
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")
  
  ##define the input files
  file1 <- "vwprojects2019.csv"
  file7 <- "vwsignaltolivetracking2019.csv" ##for priority stack ranking info from CP
  
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  file_loc7 <- file.path(path, file7)
  
  
  ## read the deployment performance file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  stltrack <- read.csv(file_loc7, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  
  ##get only delivered FIDs
  stltrack2<-stltrack[which(stltrack$OrderStatusName=="DELIVERED"),]
  
  ##reduce variables in stltrack
  stltrack3<-subset(stltrack2,select = c("MDMID"
                               ,"DeliveryNumber"
                               ,"EngineeringGroup"
                               ,"PlanResourceTypeName"
                               ,"PlanPropertyGroupName"
                               ,"PlanSKUName"
                               ,"PlanGEO"
                               ,"PlanDockDate"
                               ,"PlanRTEGDate"
                               ,"DataCenter"
                               ,"Region"
                               ,"RegionGroup"
                               ,"PlanOrderPriorityName"
                               ,"PriorityStackRank"
                               ,"ProjectCategory"
                               ,"PlanCAPEXApprovalMonth"
                               ,"NeedbyTAMApprovalDate"
                               ,"NeedByCapacityHandOffDate"
                               ,"NewRegionFlag"
                               ,"TAMAwardedDate"
                               ,"DeploymentSignalDate"
                               ,"DeploymentSeverity"
                               ,"DeploymentGroupIDName"
                               ,"SKUCategoryName"
                               ,"ActualCTDDate"
                               ,"CurrentActualCTDDate"
                               ,"ActualDockDate"
                               ,"ActualRTEGDate"
                               ,"ActualLiveDate"
                               ,"OrderStatusName"))
  
  ##filter to only the desired EG
  stltrack4<-stltrack3[which(stltrack3$EngineeringGroup %in% desired_eg),]
  
  ##Dates to correct format
  stltrack5<-mutate(stltrack4,calc_CTD=NA,CTDTR=NA,DTR=NA, OnTimeToRTEG=NA)
  
  stltrack5$PlanDockDate<-as.Date(stltrack5$PlanDockDate, format="%m/%d/%Y")
  stltrack5$PlanRTEGDate<-as.Date(stltrack5$PlanRTEGDate, format="%m/%d/%Y")
  stltrack5$PlanCAPEXApprovalMonth<-as.Date(stltrack5$PlanCAPEXApprovalMonth, format="%m/%d/%Y")
  stltrack5$NeedByCapacityHandOffDate<-as.Date(stltrack5$NeedByCapacityHandOffDate, format="%m/%d/%Y")
  stltrack5$TAMAwardedDate<-as.Date(stltrack5$TAMAwardedDate, format="%m/%d/%Y")
  stltrack5$DeploymentSignalDate<-as.Date(stltrack5$DeploymentSignalDate, format="%m/%d/%Y")
  stltrack5$ActualCTDDate<-as.Date(stltrack5$ActualCTDDate, format="%m/%d/%Y")
  stltrack5$ActualDockDate<-as.Date(stltrack5$ActualDockDate, format="%m/%d/%Y")
  stltrack5$CurrentActualCTDDate<-as.Date(stltrack5$CurrentActualCTDDate, format="%m/%d/%Y")
  stltrack5$ActualCTDDate<-as.Date(stltrack5$ActualCTDDate, format="%m/%d/%Y")
  stltrack5$ActualRTEGDate<-as.Date(stltrack5$ActualRTEGDate, format="%m/%d/%Y")
  stltrack5$ActualLiveDate<-as.Date(stltrack5$ActualLiveDate, format="%m/%d/%Y")
  stltrack5$ActualRTEGDate<-as.Date(stltrack5$ActualRTEGDate, format="%m/%d/%Y")
  
  ##calculate CTD-to-RTEG
  for(j in 1:dim(stltrack5)[1]){
    if(!is.na(stltrack5$CurrentActualCTDDate[j])){
      stltrack5$calc_CTD[j]<-stltrack5$CurrentActualCTDDate[j]
    }else stltrack5$calc_CTD[j]<-stltrack5$ActualCTDDate[j]
  }
  stltrack5$CTDTR<-as.numeric(stltrack5$CTDTR)
  stltrack5$DTR<-as.numeric(stltrack5$DTR)
  stltrack5$OnTimeToRTEG<-as.numeric(stltrack5$OnTimeToRTEG)
  
  ##calculate ctd-to-rteg or dock-to-rteg
  for(j in 1:dim(stltrack5)[1]){
    if(!is.na(stltrack5$ActualRTEGDate[j])){
      stltrack5$CTDTR[j]<-stltrack5$ActualRTEGDate[j]-stltrack5$calc_CTD[j]
    }
  }
  for(j in 1:dim(stltrack5)[1]){
    if(!is.na(stltrack5$ActualRTEGDate[j])){
      stltrack5$DTR[j]<-stltrack5$ActualRTEGDate[j]-stltrack5$ActualDockDate[j]
    }
  }
  
  ##calculate on time to requested RTEG
  for(j in 1:dim(stltrack5)[1]){
    if(!is.na(stltrack5$ActualRTEGDate[j])){
      stltrack5$OnTimeToRTEG[j]<-stltrack5$ActualRTEGDate[j]-stltrack5$PlanRTEGDate[j]
    }
  }
  
  ##print sheet raw data
  write.csv(stltrack5,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/stltrack_fids_raw_data.csv")
  
  
  ###############
  ##
  ##  Look at Group by: RegionGroup, PlanGEO, Region, DataCenter
  ##
  ###############
  
  ##summary level 1 - RegionGroup and EG
  stltrack7_P50<-stltrack5 %>%
    group_by(RegionGroup,EngineeringGroup) %>%
    summarize(P50_CTD_to_RTEG=median(CTDTR,na.rm = TRUE),
              P50_Dock_to_RTEG=median(DTR,na.rm = TRUE),
              P50_Time_to_PlannedRTEG=median(OnTimeToRTEG,na.rm = TRUE)
              )
  
  stltrack7_P90<-stltrack5 %>%
    group_by(RegionGroup,EngineeringGroup) %>%
    summarize(P90_CTD_to_RTEG=quantile(CTDTR, .90, na.rm = TRUE),
              P90_Dock_to_RTEG=quantile(DTR, .90, na.rm = TRUE),
              P90_Time_to_PlannedRTEG=quantile(OnTimeToRTEG, .90, na.rm = TRUE))
  
  
  ##print sheet summary1
  write.csv(stltrack7_P50,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/stltrack_fids_regiongrp_EG_summary_P50.csv")
  write.csv(stltrack7_P90,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/stltrack_fids_regiongrp_EG_summary_P90.csv")
  
  
  
  
  ##Extract South Africa FIDs
  soafrica <- stltrack3[which(stltrack$PlanGEO=="South Africa"),]
  
  
  
  ##convert date fields
  soafrica$PlanRTEGDate <- as.Date.character(soafrica$PlanRTEGDate, format = "%m/%d/%Y")
  soafrica$ActualRTEGDate <- as.Date.character(soafrica$ActualRTEGDate, format = "%m/%d/%Y")
  
  
  ##days late from PlanRTEGDate
  soafrica3<-mutate(soafrica, days_late_on_planrteg=as.numeric(soafrica$ActualRTEGDate-soafrica$PlanRTEGDate))
  
  ##print sheet
  write.csv(soafrica3,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/SouthAfricaFids.csv")
  
  ##extract UAE
  uae <- stltrack3[which(stltrack3$PlanGEO=="UAE"),]
  
  
  ##convert date fields
  uae$PlanRTEGDate <- as.Date.character(uae$PlanRTEGDate, format = "%m/%d/%Y")
  uae$ActualRTEGDate <- as.Date.character(uae$ActualRTEGDate, format = "%m/%d/%Y")
  
  
  ##days late from PlanRTEGDate
  uae3<-mutate(uae, days_late_on_planrteg=as.numeric(uae$ActualRTEGDate-uae$PlanRTEGDate))
  
  ##print sheet
  write.csv(soafrica3,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/UAEFids.csv")
  
  ##gov deployments
  gov1<-stltrack3[which(stltrack3$RegionGroup=="Restricted"),]
  gov3<-gov1[which(gov1$PlanGEO %in% desired_plangeo),]
  gov5<-gov3[which(gov3$OrderStatusName=="DELIVERED"),]
  
  
  
  
  ##group by Region
  
  
  ##group by datacenter
  
  
  
  
  
  #print sheet
  write.csv(gov3,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/GovFids.csv")
  
}