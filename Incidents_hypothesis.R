

library(ggplot2)
library(dplyr)
library(sqldf)
library(scales)
library(reshape2)
library(lubridate)
library(stringi)
library(igraph)

desired_eg<-c("O365 SharePoint","O365 Exchange","FOPE","OneDrive")
  
 # basic set up clear all existing variables 
rm(list = ls(all=T))

##set the path to DeploymentPerformance file
path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")

##define the deloyments file
file1 <- "DeliveryProjectContentReport.csv"
##define SPO pairs
file5 <- "Incidents_raw.csv"
 
##define the Deployments file path
file_loc1 <- file.path(path, file1)
#define the incidents file path
file_loc5 <- file.path(path, file5)
## read the deployment performance file
pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)

##fix colum names
pidsnames <- gsub("\\.","",names(pids))
colnames(pids) <- c(pidsnames)

##filter down to desired pids
pids3<-pids[which(pids$DeploymentClass=="New Deployment"),]
pids5<-pids3[which(pids3$ProjectCategory=="PRD"),]
pids7<-pids5[which(pids5$EngineeringGroup %in% desired_eg),]

##filter to only RTEG'd projects
pids9<-pids7[which(!is.na(pids7$ProjectDelivered)),]

##create list of DeliveryNumbers
pids_fy18_rtegd<-as.factor(unique(pids9$DeliveryNumber))

##filter out only the MOR PIDs
morpids<-pids3[grepl("^Fabric \\| M",pids3$ProjectTitle),]
mornames <- gsub("^","mor",names(morpids))
colnames(morpids) <- c(mornames)
morpids$morTags<-stri_replace_all_fixed(morpids$morTags," ","")  ##remove all whitespace
morpids$Tags<-stri_trans_toupper(morpids$morTags)             ##change strings to upper case to simplify grep later
morpids$morDeliveryNumber<-as.integer(morpids$morDeliveryNumber)

##read incidents file
incidents<-read.csv(file_loc5, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
incidentpids<-as.factor(unique(incidents$DeliveryNumber))

##get PIDs that are not in the incidents list
pidsrtegd2<-pids9[!(pids9$DeliveryNumber %in% incidentpids),]

##convert date format
pidsrtegd2$ActualDockMax <- as.Date.character(pidsrtegd2$ActualDockMax, format = "%m/%d/%Y")
pidsrtegd2$ProjectDelivered <- as.Date.character(pidsrtegd2$ProjectDelivered, format = "%m/%d/%Y")

#Calculate DTR on pids without incidents
pidsrtegd4<-mutate(pidsrtegd2, DTR = ProjectDelivered - ActualDockMax, withincidents = 0)
pidsrtegd6<-pidsrtegd4[which(!is.na(pidsrtegd4$ProjectDelivered)),]
quantile_incidentsno<-quantile(pidsrtegd4$DTR,.75)
incidents_no<-pidsrtegd4[which(pidsrtegd4$DTR<quantile_incidentsno),]
incidents_no2<-mutate(incidents_no, assocmorpid = "")
incidents_no2$Tags<-stri_replace_all_fixed(incidents_no2$Tags," ","")  ##remove all whitespace
incidents_no2$Tags<-stri_trans_toupper(incidents_no2$Tags)             ##change strings to upper case to simplify grep later
incidents_no2$assocmorpid<-stri_sub(unlist(stri_match_first_regex(incidents_no2$Tags, "MORPID=[0-9]{6}")),from=8)  ##MOR PID tag
incidents_no3<-incidents_no2[which(!is.na(incidents_no2$assocmorpid)),]
incidents_no3$assocmorpid<-as.integer(incidents_no3$assocmorpid)

##merge mor pids dataframe with pids22 to get MOR rteg dates
SQLQuery1 <- "SELECT d.DeliveryNumber
,d.ProjectTitle
,d.EngineeringGroup
,d.ActualDockMax
,d.ProjectDelivered
,d.DTR
,d.withincidents
,d.assocmorpid
,e.morDeliveryNumber
,e.morActualDockMax
,e.morProjectDelivered

FROM incidents_no3 d
LEFT JOIN morpids e
ON d.assocmorpid = e.morDeliveryNumber"

incidents_no4 <- sqldf(SQLQuery1)

##get list of pids with incidents
incidentpids4<-pids[which(pids$DeliveryNumber %in% incidentpids),]


##Incidents YES

#calculate DTR on pids with incidents
incidentpids4$ActualDockMax <- as.Date.character(incidentpids4$ActualDockMax, format = "%m/%d/%Y")
incidentpids4$ProjectDelivered <-as.Date(incidentpids4$ProjectDelivered, format = "%m/%d/%Y")
incidentpids6<-incidentpids4[which(!is.na(incidentpids4$ProjectDelivered)),]
incidentpids8<-incidentpids6[which(!is.na(incidentpids6$ActualDockMax)),]
incidentpids10<-mutate(incidentpids8, DTR = ProjectDelivered - ActualDockMax, withincidents = 1)
quantile_incidentsyes<-quantile(incidentpids10$DTR,.75)
incidents_yes<-incidentpids10[which(incidentpids10$DTR<quantile_incidentsyes),]

##lookup mor pids
incidents_yes2<-mutate(incidents_yes, assocmorpid = "")
incidents_yes2$Tags<-stri_replace_all_fixed(incidents_yes2$Tags," ","")  ##remove all whitespace
incidents_yes2$Tags<-stri_trans_toupper(incidents_yes2$Tags)             ##change strings to upper case to simplify grep later
incidents_yes2$assocmorpid<-stri_sub(unlist(stri_match_first_regex(incidents_yes2$Tags, "MORPID=[0-9]{6}")),from=8)  ##MOR PID tag



allpids<-rbind(incidents_no,incidents_yes2)



write.csv(allpids,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/O365pidsDTR.csv")

write.csv(incidents_no3,file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/incidents_no3.csv")


