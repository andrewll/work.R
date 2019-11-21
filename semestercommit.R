semestercommit<-function(){
  
  #####################
  ##
  ##  This script marks decom projects into high, medium, low confidence of completing
  ##  within the following semester.  
  ##
  #####################
  
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
  
  ##set variables
  desired_eg<-c("Bing","Cosmos")
  desired_status<-c("Execution","Planned")
  desired_execphase<-c("Physical Decomm","Signal to Dock","Workload Migration","Dock to RC")
  semester_end<-mdy('6/30/2020')
  container_dc<-c("BN2","CO2","CO3","CO4")
  pd_start_target<-mdy('3/1/2020')
  
  ##set the path to DeploymentPerformance file
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")
  
  
  ##define the deloyments file
  file1 <- "decommprojects.csv"
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  
  ## read the deployment performance file
  decompids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = FALSE)
  
  ##filter to status and execution phase variables
  decompids03<-decompids[which(decompids$Status %in% desired_status),]
  decompids05<-decompids03[which(decompids03$ExecutionPhase %in% desired_execphase),]
  decompids07<-decompids05[which(decompids05$EG %in% desired_eg),]
  
  #subset to just the desired columns
  decompids09<-subset(decompids07,select=c("ProjectId"
                                           ,"EG"
                                           ,"UnitType"
                                           ,"DcCode"
                                           ,"ActualWMStart"
                                           ,"ActualWMEnd"
                                           ,"ActualPDStart"
                                           ,"ProjectedPDStart"
                                           ,"ProjectedPDEnd"
                                           ,"ExecutionPhase"
                                           ,"servercount"))
  
  ##convert date fields to date format
  decompids09$ActualWMStart<-as.Date(decompids09$ActualWMStart, format="%m/%d/%Y")
  decompids09$ActualWMEnd<-as.Date(decompids09$ActualWMEnd, format="%m/%d/%Y")
  decompids09$ActualPDStart<-as.Date(decompids09$ActualPDStart, format="%m/%d/%Y")
  decompids09$ProjectedPDStart<-as.Date(decompids09$ProjectedPDStart, format="%m/%d/%Y")
  decompids09$ProjectedPDEnd<-as.Date(decompids09$ProjectedPDEnd, format="%m/%d/%Y")
  
  ##filter down to dates before semester end
  decompids11<-decompids09[which(decompids09$ProjectedPDEnd<semester_end),]
  
  ##add columns for part_container, confidencelevel
  decompids13<-mutate(decompids11, part_container = 0, confidencelevel = "3-Low")
  
  ##set part_container
  for (j in 1:dim(decompids13)[1]){
    if(decompids13$DcCode[j] %in% container_dc){
      decompids13$part_container[j]<-1
    }
  }
  
  ##set confidencelevel
  for (j in 1:dim(decompids13)[1]){
   if(!is.na(decompids13$ActualPDStart[j])&&
      (decompids13$ActualPDStart[j]<pd_start_target)&&
      (decompids13$part_container[j]==0) )
      decompids13$confidencelevel[j]<-c("1-High")
   else
     if(!is.na(decompids13$ActualPDStart[j])&&
        (decompids13$ActualPDStart[j]<pd_start_target)&&
        (decompids13$part_container[j]==1) )
       decompids13$confidencelevel[j]<-c("2-Medium")
  }
  
  ##resort columns
  decompids15<-subset(decompids13,select=c("EG"
                                           ,"confidencelevel"
                                           ,"ProjectedPDEnd"
                                           ,"servercount"
                                           ,"ProjectId"
                                           ,"UnitType"
                                           ,"DcCode"
                                           ,"ExecutionPhase"
                                           ,"ActualWMStart"
                                           ,"ActualWMEnd"
                                           ,"ActualPDStart"
                                           ,"ProjectedPDStart"
                                           ,"part_container"))
  
  decompids17<-decompids15[order(decompids15$EG, decompids15$confidencelevel,decompids15$ProjectedPDEnd),]
  
  ##print sheet
  write.csv(decompids17,file=paste("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/manganese_bingcosmos_analysis.csv",sep="_"))
  
  
}