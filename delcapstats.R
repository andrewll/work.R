delcapstats<-function(file){
  
  ################
  ##
  ##This script is for updating Jay Brickey's shiproom deck 
  ##input files are output from Delivered Capacity report
  ##
  ################
  
  
##files_list<-list.files(directory,full.names=TRUE)
delcap<-data.frame()
delcap<-read.csv(file,stringsAsFactors = FALSE)


extract_columns<-c("EG","PropertyGroup","DeliveryNumber","ProjectTitle","DataCenter","CommittedDeliveryDate1","RTEGActualDeliveryDate","DTR","DEL","RTEGOTDF","TotalServerCount","DPM","ProjectCategory","DeploymentClass","DemandCreatedDate","ProjectCreationDate","woadPOCreation","ReceivingDate2")
delcap2<-delcap[,extract_columns]
delcap2[,11]<-suppressWarnings(as.numeric(delcap2[,11]))

#Subsetting just the rows
delcap3_prd<-delcap2[which(delcap2$ProjectCategory=="PRD"),]
delcap3_discrete<-delcap2[which(delcap2$ProjectCategory=="Discrete"),]

#count
delcap3_prd_count<-nrow(delcap3_prd)
delcap3_discrete_count<-nrow(delcap3_discrete)

#average
delcap3_prd_mean<-mean(delcap3_prd$DTR, na.rm=T)
delcap3_discrete_mean<-mean(delcap3_discrete$DTR, na.rm=T)

#median
delcap3_prd_median<-median(delcap3_prd$DTR, na.rm=T)
delcap3_discrete_median<-median(delcap3_discrete$DTR, na.rm=T)

#95th Percentile
delcap3_prd_95<-quantile(delcap3_prd$DTR,probs=.95, na.rm=TRUE)
delcap3_discrete_95<-quantile(delcap3_discrete$DTR,probs=.95, na.rm=TRUE)

#Standard Deviation
delcap3_prd_sd<-sd(delcap3_prd$DTR)
delcap3_discrete_sd<-sd(delcap3_discrete$DTR)

#Total Server Count
delcap3_prd_tsc<-sum(delcap3_prd$TotalServerCount)
delcap3_discrete_tsc<-sum(delcap3_discrete$TotalServerCount)

#print results
message("PRD PID count: ",delcap3_prd_count)
message("PRD DTR Average: ",delcap3_prd_mean)
message("PRD DTR Median: ",delcap3_prd_median)
message("PRD DTR 95th%: ",delcap3_prd_95)
message("PRD DTR St Dev: ",delcap3_prd_sd)
message("PRD Total Server count: ",delcap3_prd_tsc)
message("Discrete PID count: ",delcap3_discrete_count)
message("Discrete DTR Average: ",delcap3_discrete_mean)
message("Discrete DTR Median: ",delcap3_discrete_median)
message("Discrete DTR 95th%: ",delcap3_discrete_95)
message("Discrete DTR St Dev: ",delcap3_discrete_sd)
message("Discrete Total Server count: ",delcap3_discrete_tsc)



}