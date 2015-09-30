## basic set up clear all existing variables 
rm(list = ls(all=T))

library(ggplot2)
library(plyr)
library(sqldf)
library(scales)
library(reshape2)


##set the path to DeploymentPerformance file
path <- paste0("C:/Users/answami/Documents",
               "/WindowsPowerShell/Scripts/Deployments")

##define the deloyments file
file1 <- "DeliveryPerformance.csv"
##define Azure deliveries file
file2 <- "AzureDeliveries.csv"
##define the milestone file
file3 <- "MilestonePerformance.csv"
##define the milestone sequence file
file4 <- "MilestoneSeq.csv"

##define the Deployments file path
file_loc1 <- file.path(path, file1)
##define Azure deliveries file path
file_loc2 <- file.path(path, file2)
##define the Milestone file path
file_loc3 <- file.path(path, file3)
##define the Milestone sequence file path
file_loc4 <- file.path(path, file4)


## read the deployment performance file
pids <- read.csv(file_loc1, header = TRUE, colClasses = NA, na.strings = "#N/A")

## read the Azure deliveries file
azurepids <- read.csv(file_loc2, 
                      header = TRUE, colClasses = NA, na.strings = "#N/A")

## read the Milestone CT file
milestones <- read.csv(file_loc3, 
                       header = TRUE, colClasses = NA, na.strings = "#N/A")

## read the Milestone Seq file
milestone_seq <- read.csv(file_loc4, 
                          header = TRUE, colClasses = NA, na.strings = "#N/A")

#select only some fields for azurepids
azurepids <- azurepids[,which(grepl("SC|Deploy|Plan", names(azurepids)) == TRUE)]

## convert PID# number to character
azurepids$Deploy_GFSDProjectID = as.character(azurepids$Deploy_GFSDProjectID)


##convert dates to date format
pids$RTEGActualDeliveryDate <- as.Date(pids$RTEGActualDeliveryDate, format = "%m/%d/%Y")
pids$ReceivingDate <- as.Date(pids$ReceivingDate, format = "%m/%d/%Y")
pids$woadDock <- as.Date(pids$woadDock, format = "%m/%d/%Y")

milestones$woadDock <- as.Date(milestones$woadDock, format = "%m/%d/%Y")

##remove spaces from the milestone name in the milestone seq file 
milestone_seq$MilestoneName <- gsub(" ", "", milestone_seq$MilestoneName)
##remove the & from Configure&VerifyNetwork
milestone_seq$MilestoneName <- gsub("&", "", milestone_seq$MilestoneName)

## select deployment class = New Deployment
pids <- pids[which(as.character(pids$DeploymentClass) == "New Deployment"),]
milestones <- milestones[which(as.character(milestones$DeploymentClass) == "New Deployment"),]

##define the EGs we want to evaluate
EGList <- c("Azure", "O365 Exchange", "AP", "O365 SharePoint")

##select only Azure and EXO 
pids <- pids[which(as.character(pids$EG) %in% EGList),]
milestones <- milestones[which(as.character(milestones$EG) %in% EGList),]


## define deployment types to evaluate
Dtype <- c("PRD")

pids <- pids[which(as.character(pids$ProjectCategory) %in% Dtype), ]
milestones <- milestones[which(as.character(milestones$ProjectCategory) %in% Dtype), ]


##from milestones remove all remaining NA actual dock dates to make the box plots use the same samples
milestones <- milestones[which(is.na(milestones$woadDock) == FALSE),]


## now join azurepids and pids 

SQLQuery1 <- "SELECT p.DeliveryNumber
,p.RTEGActualDeliveryDate
,p.ReceivingDate
,p.EG
,p.rtegActualMonth
,p.woadDock
,p.PreRackCount
,q.SC_ActualDockDate as AzureActualDockDate
FROM pids p
LEFT JOIN azurepids q 
ON p.DeliveryNumber = q.Deploy_GFSDProjectID"


result <- sqldf(SQLQuery1)

##convert ActualDock date to date format
result$AzureActualDockDate <- as.Date(result$AzureActualDockDate, format = "%m/%d/%Y")
##convert rtegactual month to date format
result$rtegActualMonth <- as.Date(result$rtegActualMonth, format = "%m/%d/%Y")


##Add a monthname column
result$rtegmonthname <- format(result$rtegActualMonth, format = "%Y-%m")
milestones$rtegmonthname <- format(as.Date(milestones$RTEGActualDeliveryDate, format = "%m/%d/%Y"), "%Y-%m")

##create a blank TrueDockDate field

result$TrueDockDate <- as.Date("1/0/1900", format = "%m/%d/%Y")

## merge the two dockdate fields to get a consolidated dock date field

for (i in 1:dim(result)[1]){
  if (!is.na(result$AzureActualDockDate[i])){result$TrueDockDate[i] <- result$AzureActualDockDate[i]}
  else {result$TrueDockDate[i] <- result$woadDock[i]}
  
}

##convert TrueDockDate to date format
result$TrueDockDate <- as.Date(result$TrueDockDate, format = "%m/%d/%Y")

##calculate DTR 
result$DTR2 <- as.numeric(result$RTEGActualDeliveryDate - result$TrueDockDate)

## define months to include
fullyear <- c("2014-07","2014-08","2014-09",
              "2014-10","2014-11","2014-12",
              "2015-01","2015-02","2015-03",
              "2015-04","2015-05","2015-06",
              "2015-07", "2015-08", "2015-09")

halfyear <- c("2015-01","2015-02","2015-03",
              "2015-04","2015-05","2015-06",
              "2015-07", "2015-08", "2015-09")

##take the time period in question
result <- result[which(result$rtegmonthname %in% halfyear),]
milestones <- milestones[which(milestones$rtegmonthname %in% halfyear),]


##there are some absurd PIDs. drop it while we investigate what's going on
result <- result[-(which(result$DeliveryNumber %in% c("435509", "406997"))),]
milestones <- milestones[-(which(milestones$DeliveryNumber %in% c("435509", "406997"))),]

##calculate pidcount
pidcount <- count(result, vars = c("EG", "as.factor(rtegmonthname)"))
names(pidcount) <- c("EG", "rtegmonthname", "pidcount")


##merge pidcount into result

result <- merge(result, pidcount, by.x = c("EG", "rtegmonthname"), 
                by.y = c("EG", "rtegmonthname"))

##copy over the pidcount as numeric for later summarization if needed
result$pidcount_numeric <- result$pidcount
## convert pidcount to character and add descriptor label to the first value
result$pidcount <- as.character(result$pidcount)
result$pidcount[1] <- paste0("\n\nPIDs")


##trim down milestones to just what's needed for box plotting
milestones2 <- NULL
milestones2$DeliveryNumber <- milestones$DeliveryNumber
milestones2$rtegmonthname <- milestones$rtegmonthname
milestones2$EG <- milestones$EG
milestones2 <- cbind(milestones2, milestones[,which(grepl("Value", names(milestones)) == TRUE)])

##now melt the dataset. We will use this for box plotting
milestones3 <- melt(milestones2, id = c("DeliveryNumber", "rtegmonthname", "EG"))
##name the columns appropriately
names(milestones3) <- c("DeliveryNumber", "rtegmonthname", "EG", "Milestone", "CT")

##remove text 'values' from milestone names for ease of readibility
milestones3$Milestone <- gsub("Value", "", milestones3$Milestone)

##define which milestones to include for milestone plots
milestonelist <- c("Receiving", "BoltandEnergize", "HWValidation",
                   "PhysicalCabling", "CableValidation",
                   "ConfigureVerifyNetwork", "OperationalAcceptance")



##define which EG milestone box plots are needed for
#eglist <- c("Azure")

#select milestone rows based on eglist
milestones3 <- milestones3[which(milestones3$EG %in% EGList),]

##select only milestones in milestonelist
milestones3 <- milestones3[which(milestones3$Milestone %in% milestonelist),]


##tack on sequence numbers to Milestone names

##first join the milestone sequence number

SQLQuery2 <- "SELECT p.DeliveryNumber
,p.rtegmonthname
,p.EG
,p.Milestone
,p.CT
,q.MilestoneSequenceNumber
FROM milestones3 p
LEFT JOIN milestone_seq q 
ON p.Milestone = q.MilestoneName"

#now overwrite the milestone3 frame with the joined dataset
milestones3 <- sqldf(SQLQuery2)

##append the sequence number with the milestone
for (j in 1:dim(milestones3)[1]){
  if (milestones3$MilestoneSequenceNumber[j] < 1000){
    milestones3$Milestone[j] <- paste0("00",milestones3$MilestoneSequenceNumber[j], " ", milestones3$Milestone[j])  
  }
  else{ 
    if (milestones3$MilestoneSequenceNumber[j] < 10000 && milestones3$MilestoneSequenceNumber[j] >= 1000){
      milestones3$Milestone[j] <- paste0("0",milestones3$MilestoneSequenceNumber[j], " " , milestones3$Milestone[j])  
    }
    else {milestones3$Milestone[j] <- paste0(milestones3$MilestoneSequenceNumber[j], " " , milestones3$Milestone[j])}
  }
}

##create plots
## plot cycle time
plot1 <- ggplot(data = result, aes(x = EG, 
                                   y = DTR2, fill = EG, label = PreRackCount))

##add facets by month
plot1 <- plot1 + geom_boxplot() + facet_grid(.~ rtegmonthname)
##adjust font size etc. of x,y axes
plot1 <- plot1 + theme(axis.text.x=element_text(colour = "black",angle=90,
                                                hjust = 0.1,vjust=0.1,
                                                size = 20, face = "bold"),
                       axis.title.y = element_text(colour = "black", size = 20, face = "bold"),
                       axis.text.y = element_text(colour = "black", size = 10, face = "bold"))

##adjust font size etc. of facet labels
plot1 <- plot1 + theme(strip.text.x = element_text(size = 20, colour = "black", face = "bold"))

##remove legend because x-labels are self-evident
plot1 <- plot1 + theme(legend.position = "none")
plot1 <- plot1 + scale_x_discrete(name="") 
#plot1 <- plot1 + scale_y_continuous(name="") 
#plot1 <- plot1 + ggtitle("Dock-to-RTEG cycle time spread - Azure and EXO")

##add number of points
#plot1 <- plot1 + geom_jitter()

##add pidcount labels
plot1 <- plot1 + geom_text(aes(x = EG, y = -5, 
                               label = pidcount, size = 1, face = "bold"),
                           show_guide = FALSE)
plot1 <- plot1 + ylab("Dock-To-RTEG cycle time (days)")


## plot pid count
#plot2 <- ggplot(data = pidcount, 
#                aes(x = EG, y = pidcount, fill = EG, label = pidcount))
#plot2 <- plot2 + geom_bar(stat = "identity") + facet_grid(.~rtegmonthname)
#plot2 <- plot2 + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,
#                                                size = 16, face = "bold"))
#plot2 <- plot2 + geom_text(aes(x = EG, y = pidcount, label = pidcount), size = 3)
#plot2 <- plot2 + scale_x_discrete(name="") 
#plot2 <- plot2 + scale_y_continuous(name="") 
#plot2 <- plot2 + theme(legend.position = "none")
#plot2 <- plot2 + ggtitle("PIDs delivered - Azure and EXO (Discrete, PRD, Network)")
#plot2 <- plot2 + ylab("PIDs Delivered")

##plot delivery scatter plot
plot3 <- ggplot(data = result,
                aes(x = RTEGActualDeliveryDate, y = DTR2,
                    color = EG, size = PreRackCount, show_guide = FALSE))

## add an upper control limit to plot3. 3*sd(dataset)
sigma3 <- 3*sd(result$DTR2, na.rm = TRUE)
UCL <- data.frame( yintercept = sigma3, UCL = factor(sigma3))
plot3 <- plot3 + geom_hline(aes(yintercept = yintercept, 
                                linetype = UCL),
                            data = UCL, show_guide = FALSE)

## add another horizontal line for a 7 day DTR target
target <- 7
tar <- data.frame( yintercept = target, UCL = factor(target))
plot3 <- plot3 + geom_hline(aes(yintercept = yintercept, 
                                linetype = tar),
                            data = tar, color = "red", show_guide = FALSE)



## add another horizontal line for median of dataset
med <- median(result$DTR2, na.rm = TRUE)
med <- data.frame( yintercept = med, med = factor(med))
plot3 <- plot3 + geom_hline(aes(yintercept = yintercept, 
                                linetype = med),
                            data = med, show_guide = FALSE)


plot3 <- plot3 + geom_point(alpha = 0.5, show_guide = FALSE)
#plot3 <- plot3 + geom_density(alpha = 0.005, guide = "none")

##plot full color legend
dummyData <- result[1, ]
dummyData$DTR2 <- NaN
plot3 <- plot3 + geom_point(data=dummyData, aes(RTEGActualDeliveryDate, DTR2, 
                                                colour = EG), alpha=1.0, na.rm=TRUE)


##adjust the relative sizes of bubble3s
plot3 <- plot3 + scale_size_continuous(range = c(3,8))

##plot x axis in weeks
plot3 <- plot3 + scale_x_date(labels = date_format("%W"), 
                              breaks = date_breaks("week"))

##add x, y labels
plot3 <- plot3 + ylab("Dock to RTEG Cycle time (Days)")
plot3 <- plot3 + xlab("RTEG week")

#hide minor grid lines
plot3 <- plot3 + theme(panel.grid.minor=element_blank())

##annotate text for lines, y axis calculations have to redone while annotating
plot3 <- plot3 + annotate("text", x = as.Date("30", format = "%W"),
                          y = 3*sd(result$DTR2, na.rm = TRUE),
                          label = "UCL")
plot3 <- plot3 + annotate("text", x = as.Date("30", format = "%W"), y = 7, label = "Target")

plot3 <- plot3 + annotate("text", x = as.Date("30", format = "%W"), 
                          y = median(result$DTR2, na.rm = TRUE), 
                          label = "Median")


##adjust legend font size
plot3 <- plot3 + theme(legend.title=element_text(size=20), 
                       legend.text = element_text(size = 20, face = "bold"))

##adjust x,y axes font sizes
plot3 <- plot3 + theme(axis.text.x=element_text(colour = "black", size = 10, face = "bold"),
                       axis.title.x = element_text(colour = "black", size = 20, face = "bold"),
                       axis.title.y = element_text(colour = "black", size = 20, face = "bold"),
                       axis.text.y = element_text(colour = "black", size = 10, face = "bold"))


##make a milestone plot just like the DTR CT box plot
plot4 <- ggplot(data = milestones3, aes(x = Milestone, 
                                        y = CT, fill = EG))

##add facets by month
#plot4 <- plot4 + geom_boxplot() + facet_grid(.~ rtegmonthname)
plot4 <- plot4 + geom_boxplot()

##adjust font size etc. of x,y axes
plot4 <- plot4 + theme(axis.text.x=element_text(colour = "black",angle=90,
                                                hjust = 0.1,vjust=0.1,
                                                size = 20, face = "bold"),
                       axis.title.y = element_text(colour = "black", size = 20, face = "bold"),
                       axis.text.y = element_text(colour = "black", size = 10, face = "bold"))

##adjust font size etc. of facet labels
plot4 <- plot4 + theme(strip.text.x = element_text(size = 20, colour = "black", face = "bold"))

##remove legend because x-labels are self-evident
#plot4 <- plot4 + theme(legend.position = "none")
plot4 <- plot4 + scale_x_discrete(name="") 

##print graphs to PNG

ggsave( "./DeliveryPerformance/ControlChart.png",
        plot3,
        width = 20,
        height = 10,
        dpi = 1200)


ggsave( "./DeliveryPerformance/Boxplots.png",
        plot1,
        width = 20,
        height = 10,
        dpi = 1200)


ggsave( "./DeliveryPerformance/MilestoneBoxplot.png",
        plot4,
        width = 20,
        height = 10,
        dpi = 1200)

##make a table showing stats
stats <- ddply(result, 
               ~ EG + rtegmonthname, 
               summarize, median = median(DTR2, na.rm = TRUE), 
               sd = sd(DTR2, na.rm = TRUE), 
               mean = mean(DTR2, na.rm = TRUE))



##wtite stats to csv file
write.csv(stats, "C:/Users/answami/Documents/R/Code/DeliveryPerformance/stats.csv")

#reshape milestone data for writing to file
##dump sequence number; we don't need it any more
milestones3$MilestoneSequenceNumber <- NULL
milestones_wide <- dcast(milestones3, DeliveryNumber + rtegmonthname + EG ~ Milestone)

##write data files to csv files
write.csv(result, "C:/Users/answami/Documents/R/Code/DeliveryPerformance/DockToRTEGPlotData.csv")
write.csv(milestones_wide, "C:/Users/answami/Documents/R/Code/DeliveryPerformance/MilestoneCTPlotData.csv")


