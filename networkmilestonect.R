networkmilestonect<-function(){
  
  ########################
  ##
  ## function to calculate CT from Vinayak's Scrum pre-read spreadsheet
  ##
  ########################
  
  library(ggplot2)
  library(dplyr)
  library(sqldf)
  library(scales)
  library(reshape2)
  library(lubridate)
  
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")
  
  ##define the deloyments file
  file1 <- "SPO_network_scrum_preread_2016_10_06.csv"

  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)

  ##read input file
  pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A", stringsAsFactors = TRUE)
  
  ##convert dates to date format for pids table
  pids$Date.Entering.CVN <- as.Date(pids$Date.Entering.CVN,, format = "%m/%d/%Y")
  pids$NW.Milestone.completion.date <- as.Date(pids$NW.Milestone.completion.date, format = "%m/%d/%Y")
  pids$CVN.SLA <- as.character(pids$CVN.SLA)
  
  ##calculate month completion date
  pids2<-mutate(pids, month_delivered = format(ymd(NW.Milestone.completion.date),"%Y-%m"))
  
  ##subset
  pids5<-subset(pids2, select = c("PID","CVN.SLA","month_delivered","CVN.CT"))
  pids7<-pids5[which(!is.na(pids5$month_delivered)),]
  pids9<-pids7[which(!is.na(pids7$CVN.SLA)),]
  
  ##replace SLA string with better description
  pids9$CVN.SLA<-gsub("7","7 days-PRD",pids9$CVN.SLA)
  pids9$CVN.SLA<-gsub("14","14 days-Core Agg/Exp MT",pids9$CVN.SLA)
  pids9$CVN.SLA<-gsub("21","21 days-Core Agg/Collapsed Agg MT",pids9$CVN.SLA)
  pids9$CVN.SLA<-gsub("29","29 days-Core Agg/Exp DonMT",pids9$CVN.SLA)
  
  ##plot
  png("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/cycletime_boxplot_network_configverifymilestone.png", 
      width = 960, height = 480, units = "px")
  par(mfrow=c(2,2), mar=c(10,8,2,1))
  g<-ggplot(pids9,aes(x=month_delivered, y=CVN.CT, fill=CVN.SLA))
  g+geom_boxplot()+facet_wrap(~CVN.SLA)+labs(title="SPO PIDs - Config&Verify Milestone Cycle Times", x="Month of Milestone Exit", y="Cycle Time in days")
  dev.off()
  
  #output to excel
  ##print table with summarized details by EG
  write.csv(pids9,file="C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/CSV_cycletime_boxplot_network_configverifymilestone.csv")
  
  
  
  
}