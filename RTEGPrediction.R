

#-----------------------------------------------------------------------------------------------------------------------------
# Predict RTEG Date

# basic set up clear all existing variables 
rm(list = ls(all=T))

#options(warn = -1)

library(ggplot2)
library(plyr)
library(sqldf)
library(reshape2)
library(MASS)
library(car)
library(nortest)
library(Hmisc)
library(pastecs)
library(modeest)
library(lubridate)

TZ <- "UTC"

#------------------------------------------------------------------------------------------------------------
# Import and Initialize Data

# Get PID Data
thisData <- read.csv("C:/aaProjects/aaStats/aaaWhitePaper/Andrew/POC1/RScripts/PIDData.csv")

# Get SLAs for each Milestone
# This is currently based on SLAs from Laura Riley, Non-Azure
SLAData <- read.csv("C:/aaProjects/aaStats/aaaWhitePaper/Andrew/POC1/RScripts/SLA.csv")

# Get Data defining the distributions we want to model, plus parameters for exporting in specific format (may not need here)
distrData <- read.csv("C:/aaProjects/aaStats/aaaWhitePaper/Andrew/POC1/RScripts/DistributionTypes.csv")
distrDataTemp <- distrData
nbrDistrType <- as.numeric( length(distrData$DistrNbr) )

# Get list of distinct PIDs (of interest, not the associated data vector)
SQLQuery <- "SELECT distinct(OrigPID) FROM thisData ORDER BY OrigPID"
distinctPID <- sqldf(SQLQuery)
nbrPID <- as.numeric( length(distinctPID$OrigPID) )


# Set Constants
nbrDigits       <- 2
percVariability <- .2 # percent variability to add for Sum of SLAs method
iSummMin        <- 1 # col for Min from Summary stats
iSumm25th       <- 2 # col for 75th Percentile from Summary stats
iSumm50th       <- 3 # col for Median from Summary stats
iSummMean       <- 4 # col for Mean from Summary stats
iSumm75th       <- 5 # col for 75th Percentile from Summary stats
ActualHrPerDay  <- 24
iColCT <- as.numeric(grep("CycleTimeHr", colnames(thisData)))
iColPIDTimeInMilestone  <- as.numeric(grep("PIDTimeInMilestoneHr", colnames(thisData)))
iColCurrDate <- as.numeric(grep("CurrentDate", colnames(thisData)))
iColPIDStart <- as.numeric(grep("PIDMilestoneStartTime", colnames(thisData)))
iColPID  <- as.numeric(grep("OrigPID", colnames(thisData)))
iColEG  <- as.numeric(grep("EG", colnames(thisData)))
iColDeploymentClass  <- as.numeric(grep("DeploymentClass", colnames(thisData)))
iColProjectCategory  <- as.numeric(grep("ProjectCategory", colnames(thisData)))
iColDataCenterLocation  <- as.numeric(grep("DataCenterLocation", colnames(thisData)))
iColDataCenterRegion  <- as.numeric(grep("DataCenterRegion", colnames(thisData)))
iColCurrentMileStoneName  <- as.numeric(grep("OrigMilestone", colnames(thisData)))
iColMileStoneSequenceNUmber  <- as.numeric(grep("OrigMSNbr", colnames(thisData)))
iColDPMType  <- as.numeric(grep("DPMType", colnames(thisData)))

iColParam1      <- as.numeric(grep("R_Param1Value", colnames(distrData)))
iColParam2      <- as.numeric(grep("R_Param2Value", colnames(distrData)))
iColPVal        <- as.numeric(grep("R_pValue", colnames(distrData)))
iColLoglik      <- as.numeric(grep("R_loglik", colnames(distrData)))
iColPMParam1    <- as.numeric(grep("ProM_Param1Value", colnames(distrData)))
iColPMParam2    <- as.numeric(grep("ProM_Param2Value", colnames(distrData)))
iColDistrStr1   <- as.numeric(grep("ProModelStr1", colnames(distrData)))
iColDistrStr2   <- as.numeric(grep("ProModelStr2", colnames(distrData)))
iColDistrName   <- as.numeric(grep("DistrName", colnames(distrData)))
iColDistrGenName   <- as.numeric(grep("FitDistrGenName", colnames(distrData)))
iColHypTest     <- as.numeric(grep("DistrHypTestName", colnames(distrData)))
strDiagnostics  <- ""
strPrimaryEstimator <- ""

# Initialize dataframe for Output
dfOutput <- data.frame( 
  DateRun = character(),
  DeliveryNumber = integer(),
  PIDState = character(),
  BestDistribution = character(),
  BestEstimator = character(),
  EstimatedRTEGDate = character(),
  LowerDate = character(),
  UpperDate = character(),
  EG = character(),
  DeploymentClass = character(),
  ProjectCategory = character(), 
  DataCenterLocation = character(), 
  DataCenterRegion = character(), 
  CurrentMileStoneName = character(), 
  MileStoneSequenceNUmber = integer(), 
  DPMType = character(),
  PIDMilestoneStartTime = character(),
  PIDTimeInMilestoneHr = integer(),
  PIDTimeInMilestoneDay = numeric(),
  TimeToComplete = numeric(), 
  LowerBound = character(),
  UpperBound = character(),
  Mean = numeric(),
  Median = numeric(),
  LB_Mean = numeric(),
  UB_Mean = numeric(),
  LB_Median = numeric(),
  UB_Median = numeric(),
  LB_SLA = numeric(),
  UB_SLA = numeric(),
  StandardDeviation = numeric(),
  InterquartileRange = numeric(),
  FirstQuartile = numeric(),
  ThirdQuartile = numeric(),
  LowerWhisker = numeric(),
  UpperWhisker = numeric(),
  LengthOrigDataset = integer(),
  LengthFinalDataset = integer(),
  NumberOutliersRemoved = integer()
)

# Initialize dataframe for Diagnostic output 
dfDiagnostics <- data.frame( 
  PIDCount = integer(),
  DistributionType = character(),
  Diagnostics = character()
)

# Initialize dataframe for NewCT days output 
dfNewCTDays <- data.frame( 
  PIDCount = integer(),
  OrigPID = integer(),
  CountCT = integer(),
  CTDays = numeric()
)


#------------------------------------------------------------------------------------------------------------------
# Loop over distinct PIDs of interest. Determine Estimated RTEG and other measures for each PID.

countPID <- 1

while ( countPID <= nbrPID ) {      
  # Initialize Key Cycle Time Vector
  SQLQuery <- paste("SELECT CycleTimeHr FROM thisData WHERE OrigPID = ", distinctPID[countPID,1], " ORDER BY CycleTimeHr", sep="")  
  CTHr <- sqldf(SQLQuery)
  lengthCTOrig <- as.numeric( length(CTHr$CycleTimeHr) )
  
  SQLQueryAllInfo <- paste("SELECT * FROM thisData WHERE OrigPID = ", distinctPID[countPID,1], " limit 1", sep="")  
  thisPIDInfo <- sqldf(SQLQueryAllInfo)
  
      #------------------------------------------------------------------------------------------------------------
      # Remove Outliers based on Box Plot method
      
      thisIQR       <- as.numeric(summary(CTHr$CycleTimeHr)[iSumm75th]) - as.numeric(summary(CTHr$CycleTimeHr)[iSumm25th]) # Q3 - Q1
      WhiskerLower  <- as.numeric(summary(CTHr$CycleTimeHr)[iSumm25th] - (thisIQR*1.5))
      WhiskerUpper  <- as.numeric(summary(CTHr$CycleTimeHr)[iSumm75th] + (thisIQR*1.5))
      WhiskerLower  <- max(WhiskerLower, min(CTHr$CycleTimeHr) )
      WhiskerUpper  <- min(WhiskerUpper, max(CTHr$CycleTimeHr) )
      
      # Get Subset with Outliers Removed
      if ( thisIQR > 0 ) {
        newCTHr <- subset(CTHr$CycleTimeHr, CTHr$CycleTimeHr <= WhiskerUpper & CTHr$CycleTimeHr >= WhiskerLower)
      } else {
        newCTHr <- subset(CTHr$CycleTimeHr, CTHr$CycleTimeHr <= WhiskerUpper )
      }
      lengthCTFinal <- as.numeric( length(newCTHr) )
  
      #------------------------------------------------------------------------------------------------------------
      # More basic data set up
      
      # Convert PIDTimeInMilestoneHr to Days 
      thisPIDTimeinMilestoneDay <- round( as.numeric( thisPIDInfo[1,iColPIDTimeInMilestone] ) / ActualHrPerDay, nbrDigits )
      
      # Get current date (from time run in sql) and Date PID of interest started this Milestone
      CurrentDate <- as.POSIXct( thisPIDInfo[1,iColCurrDate], format='%m/%d/%Y', origin = "1900-01-01"  )
      PIDMilestoneStartTime <- as.POSIXct( thisPIDInfo[1,iColPIDStart], format='%m/%d/%Y', origin = "1900-01-01"  )

      # newDT = Current Date - Time PID already in Milestone, then round down to get start of day
      newDT <- CurrentDate - as.difftime(as.numeric( thisPIDInfo[1,iColPIDTimeInMilestone] ) / ActualHrPerDay, unit="days")
      newDT2 <- floor_date(newDT, "day")
      
      # Get vector in Days
      newDFDays <- newCTHr / ActualHrPerDay
      
      # Calc final summary stats for subset with outliers removed
      thisMean      <- as.numeric(summary(newDFDays)[iSummMean])
      thisMedian    <- as.numeric(summary(newDFDays)[iSumm50th])
      thisStdDev    <- round( sd(newDFDays), nbrDigits)
      thisIQR       <- round( as.numeric(summary(newDFDays)[iSumm75th]) - as.numeric(summary(newDFDays)[iSumm25th]), nbrDigits ) # Q3 - Q1
      WhiskerLower  <- as.numeric(summary(newDFDays)[iSumm25th] - (thisIQR*1.5))
      WhiskerUpper  <- as.numeric(summary(newDFDays)[iSumm75th] + (thisIQR*1.5))
      WhiskerLower  <- round( max(WhiskerLower, min(newDFDays) ), nbrDigits )
      WhiskerUpper  <- round( min(WhiskerUpper, max(newDFDays) ), nbrDigits )
      
      #boxplot(newDFDays)
      #hist(newDFDays)
 
      # Create data frame of rows with outliers removed for later analysis
      newCTDayRows <- data.frame(PIDCount = countPID, OrigPID = thisPIDInfo[1,iColPID], CountCT = c(1:lengthCTFinal), CTDays = round(newDFDays, nbrDigits))
      dfNewCTDays <- rbind(dfNewCTDays, newCTDayRows) 
      
      #------------------------------------------------------------------------------------------------------------
      #------------------------------------------------------------------------------------------------------------
      
      # ALGORITHM OVERVIEW:
      #
      # Determine if Standard or Problem PID (for single PID of interest):
      #   1) Standard PID: PID not already in trouble for this Milestone
      #   2) Problem PID: PID is already in trouble for this Milestone
      # Determine Distribution (for vector of CT Days, with outliers removed)
      # Use stats appropriate for the distribution (later in code)
      #   If Normal, use Mean, Confidence Interval for Mean, and regular stats
      #   If Gamma/Lognormal, use Median, Confidence Interval for Median and Box Plot stats
      #   Otherwise: Use Sum of SLAs for current and all subsequent milestonesa
      #     
      #------------------------------------------------------------------------------------------------------------
      # Determine Distribution (for vector of CT Days, with outliers removed)
      # Use stats appropriate for the distribution (later in code)
      #   If Normal, use Mean, Confidence Interval for Mean, and regular stats
      #   If Gamma/Lognormal, use Median, Confidence Interval for Median and Box Plot stats
      #   Otherwise: Use SLA 
      # Leverage DistrNbr
      #        Loop over DistributionType
      #
      # If enough data, Use Kolmogorov-Smirnov to determine Goodness of Fit
      #   Compare Original data to Distr with those estimated parameters
      #   •	H0: The data follow a specified distribution 
      #   •	H1: The data do not follow a specified distribution 
      #   using 2 tailed, 95% CL --> significance level = 0.05 (99% CL, 0.01)
      #   If the p-value is greater than our assumed significance level of 0.05 (for 95% Confidence Level, 2 tail), 
      #   we fail to reject the null hypothesis and conclude that there is no evidence in the data to suggest that the two distributions are different (that is, distributions are the same)
      # If not enough data, Approximate Triangular distribution based on Box Plot Concept
      #     use Summary statistics: min, median, max=75th Percentile+1.5*IQR
      # If No data, approximate based on SLA
      
      signifLevel  <- 0.05
      #signifLevel   <- 0.01
      sampleCutOff  <- 10   # Can only do this distribution fit test if have enough data
      param1        <- 1 # index for first parameter of distribution
      param2        <- 2 # index for second parameter of distribution
      countDistr    <- 1
      #countDistr    <- 3
      
      newDFDays <- sort(newDFDays)
      
      # Loop over distributions to find Best Fit
      #if ( lengthCTFinal >= sampleCutOff ) {
        maxIndex <- 1
        while ( countDistr <= nbrDistrType ){
          
          newDFDays_CT_dist <- fitdistr(newDFDays, as.character(distrData[countDistr,iColDistrGenName]))
          
          if (distrDataTemp$DistrName[countDistr] != "Normal") {
            # How closely does the orig data match assumed distribution with these estimated parameters?
            ksResult <- ks.test(newDFDays, as.character(distrData[countDistr,iColHypTest]), newDFDays_CT_dist$estimate[param1], newDFDays_CT_dist$estimate[param2] )
            pVal <- ksResult$p.value
          } else {
            # *** If testing Normal Distr, ADD Anderson-Darling Normality Test & look at that p-value too ***   
            adResult <- ad.test(newDFDays)
            pVal <- adResult$p.value
            
            # *** DOESN'T SEEM TO WORK IN AZURE ML SO THIS IS TO TRY TO GET WORKING IN THAT ENVIRONMENT ***            
            # Looks like the library nortest is not available in Azure ML yet, so will need to figure out what to do
            #ksResult <- ks.test(newDFDays, as.character(distrData[countDistr,iColHypTest]), newDFDays_CT_dist$estimate[param1], newDFDays_CT_dist$estimate[param2] )
            #pVal <- ksResult$p.value
            #pVal <- 0.0001
          }

          if ( pVal >= signifLevel ) {
            strPVal <- paste(countDistr, "p-value=", pVal, " GOOD FIT", sep="")
          } else {
            strPVal <- paste(countDistr, "p-value=", pVal, " BAD FIT", sep="")
          }
          
          distrDataTemp[countDistr,iColParam1]  <- newDFDays_CT_dist$estimate[param1]
          distrDataTemp[countDistr,iColParam2]  <- newDFDays_CT_dist$estimate[param2]
          distrDataTemp[countDistr,iColPVal]    <- pVal
          distrDataTemp[countDistr,iColLoglik]  <- newDFDays_CT_dist$loglik
          
          # If Gamma, invert rate to get scale 
          distrDataTemp[countDistr,iColPMParam1] <- newDFDays_CT_dist$estimate[param1]
          if (distrDataTemp$DistrName[countDistr] == "Gamma") {                    
            distrDataTemp[countDistr,iColPMParam2] <- 1.0/newDFDays_CT_dist$estimate[param2]  
          } else { 
            distrDataTemp[countDistr,iColPMParam2] <- newDFDays_CT_dist$estimate[param2]  
          }
        
          if ( countDistr > 1 ){
            if ( distrDataTemp[countDistr,iColPVal] >= distrDataTemp[countDistr-1,iColPVal] ){ maxIndex <- countDistr}
          }
          strDiagnostics <- paste(strDiagnostics, "Distr # ", strPVal, " loglik = ", newDFDays_CT_dist$loglik, sep=",")
          countDistr <- countDistr + 1
          
        } # end  loop over Distr types
        
        strThisBestDistr <- paste( distrDataTemp[maxIndex,iColDistrStr1], round(distrDataTemp[maxIndex,iColPMParam1], nbrDigits), ",", round(distrDataTemp[maxIndex,iColPMParam2], nbrDigits), distrDataTemp[maxIndex,iColDistrStr2], sep="")
        
        if ( (distrDataTemp[maxIndex,iColDistrName] == "Gamma") || (distrDataTemp[maxIndex,iColDistrName] == "Lognormal") ) {
          strPrimaryEstimator <- "Median"
          thisEstimate <- thisMedian
        } else { 
          strPrimaryEstimator <- "Mean"
          thisEstimate <- thisMean
        }
           
        # Get SLA info in case need later for Bad Fit or Problem PID     
        SQLQuery <- paste("SELECT SLADays FROM SLAData WHERE MileStoneSequenceNUmber >= ", thisPIDInfo[1,iColMileStoneSequenceNUmber], " ORDER BY StepSequenceNumber", sep="")  
        thisSLA <- sqldf(SQLQuery)
        lengthSLA <- as.numeric( length(thisSLA$SLADays) )        
        SLAEstimate <- sum(thisSLA$SLADays)
        
        if (distrDataTemp[maxIndex,iColPVal] < signifLevel ) { # best fit has p-value below tolerance, so use Sum of SLAs Method
          # Set params to SLA     
          strPrimaryEstimator <- "SLA"
          thisEstimate <- SLAEstimate
          strThisBestDistr <- paste("Sum SLAs", sep="") 
          strDiagnostics <- paste(strDiagnostics, "Bad p-value so replace distr with Sum of SLAs: ", strThisBestDistr, sep=",")   
        }

        # Determine if Standard or Problem PID (for single PID of interest):
        #   1) Standard PID: PID not already in trouble for this Milestone
        #   2) Problem PID: PID is already in trouble for this Milestone
        #   TEST:
        #     IF    1) Time in Milestone already > Upper Whisker (test on data set with outliers removed, before Est RTEG)
        #           2) (BestEstimator Time to Complete (Median or Mean) - Time in Milestone already) < 0
        #     then Problem PID
        #     OW Standard PID
        
        StdPID <- 1 #Assume Standard PID case as default
        thisPIDState <- "Standard PID"
        if ( thisPIDTimeinMilestoneDay > WhiskerUpper ) { 
          StdPID <- 0 
          thisPIDState <- "Problem PID"
          strDiagnostics <- paste(strDiagnostics, "Problem PID Test 1", sep=",")          
        } else {
          if ( (thisEstimate - thisPIDTimeinMilestoneDay) < 0 ) {
            StdPID <- 0 
            thisPIDState <- "Problem PID"
            strDiagnostics <- paste(strDiagnostics, "Problem PID Test 2", sep=",")      
          } else { 
            strDiagnostics <- paste(strDiagnostics, "Standard PID Tests 1 & 2", sep=",") 
          }
        }
        strDiagnostics <- paste(strDiagnostics, "Best Fit: ", strThisBestDistr, "best p-val= ", distrDataTemp[maxIndex,iColPVal], " Best Estimator Type: ", strPrimaryEstimator, " Estimate: ", thisEstimate, sep=",")
        
     # }
      
        #------------------------------------------------------------------------------------------------------------
        # Calc Confidence Intervals for all 3 methods
        
        # CI for the Median
        bootmed = apply(matrix(sample(newDFDays, rep=TRUE, 10^4*length(newDFDays)), nrow=10^4), 1, median)
        CIVal <- quantile(bootmed, c(.025, 0.975))
        CILowerBound_Median <- floor( as.numeric( CIVal[1] ) )
        CIUpperBound_Median <- ceiling( as.numeric( CIVal[2] ) )

        # CI for Mean
        tTestResult <- t.test(newCTDayRows$CTDays, conf.level = 0.95)
        CILowerBound_Mean <- floor( tTestResult$conf.int[1] )
        CIUpperBound_Mean <- ceiling( tTestResult$conf.int[2] )
        
        # CI for Sum(SLAs)
        # Set Confidence Interval based on % Variation of SLA
        thisVar <- SLAEstimate * percVariability
        CILowerBound_SLA <- floor( SLAEstimate - thisVar )
        CIUpperBound_SLA <- ceiling( SLAEstimate + thisVar )

        #------------------------------------------------------------------------------------------------------------
        # Calculate Estimated RTEG 
        # 1) Standard PID: 
        #     Current Date - Time in Milestone + Best Estimator
        #     Have to check this condition after calculate if standard PID case
        #       Estimated RTEG Date < GetDate *** have to do this later, after estimate. not enough info to do now
        # 2) Problem PID: 
        #     Current Date + Sum(SLA)        
             
      if (StdPID == 1){ # Standard PID
        # newDT2 is: Current Date - Time in Milestone 
        # Then Add the Median (convert to Days first)
        EstRTEGDate <- as.Date( newDT2 + as.difftime(ceiling(thisEstimate), unit="days"), format='%m/%d/%Y', origin = "1900-01-01" )
        EstRTEGDate2 <- format.Date( as.character(EstRTEGDate), format='%m/%d/%y' )
        
        #-----------------------------------------------------------
        # Calculate Confidence Interval (based on Distribution Type)

        #http://stats.stackexchange.com/questions/21103/confidence-interval-for-median
        # Method 1: simple
        #CIVal <- sort(newDFDays)[qbinom(c(.025,.975), length(newDFDays), 0.5)]
        # Method 2: bootstrap, little more complex, closer to Minitab (recheck)
        # http://www.stat.wisc.edu/~yandell/st571/R/append7.pdf
     
        if (strPrimaryEstimator == "Median") {
          CILowerBound <- CILowerBound_Median
          CIUpperBound <- CIUpperBound_Median
        } else {
          if (strPrimaryEstimator == "Mean") {
            CILowerBound <- CILowerBound_Mean
            CIUpperBound <- CIUpperBound_Mean
          } else {
            CILowerBound <- CILowerBound_SLA
            CIUpperBound <- CIUpperBound_SLA
          }
        }
          
        LowerBoundDate <- as.Date( newDT2 + as.difftime(CILowerBound, unit="days"), format='%m/%d/%Y', origin = "1900-01-01" )
        UpperBoundDate <- as.Date( newDT2 + as.difftime(as.numeric(CIUpperBound), unit="days"), format='%m/%d/%Y', origin = "1900-01-01" )
        LowerBoundDate <- format.Date( as.character(LowerBoundDate), format='%m/%d/%y' ) 
        UpperBoundDate <- format.Date( as.character(UpperBoundDate), format='%m/%d/%y' )
        
        strDiagnostics <- paste(strDiagnostics, "EstRTEGDate=", EstRTEGDate, "EstRTEGDate2=", EstRTEGDate2, " LowerBoundDate=", LowerBoundDate, " UpperBoundDate=", UpperBoundDate, sep=",")
          
      } else {          # Problem PID
        # If Problem PID, force to use Sum of SLAs and don't subtract off Time already in Milestone
        # Current Date  is date of sql run, without subtractime Time in Milestone
        #SLAEstimate # this is Sum of SLAs
        EstRTEGDate <- as.Date( CurrentDate + as.difftime(ceiling(SLAEstimate), unit="days"), format='%m/%d/%Y', origin = "1900-01-01" )
        EstRTEGDate2 <- format.Date( as.character(EstRTEGDate), format='%m/%d/%y' )
        
        # Set params to SLA     
        strPrimaryEstimator <- "SLA"
        thisEstimate <- SLAEstimate
        strThisBestDistr <- paste("Sum SLAs", sep="") 
        strDiagnostics <- paste(strDiagnostics, "Problem PID, use Sum of SLAs Method 2, SLAEstimate: ", SLAEstimate, strThisBestDistr, sep=",")   
 
        # Set Confidence Interval based on % Variation of SLA
        thisVar <- thisEstimate * percVariability
        CILowerBound <- floor( thisEstimate - thisVar )
        CIUpperBound <- ceiling( thisEstimate + thisVar )
        
        LowerBoundDate <- as.Date( CurrentDate + as.difftime(CILowerBound, unit="days"), format='%m/%d/%Y', origin = "1900-01-01" )
        UpperBoundDate <- as.Date( CurrentDate + as.difftime(CIUpperBound, unit="days"), format='%m/%d/%Y', origin = "1900-01-01" )
        LowerBoundDate <- format.Date( as.character(LowerBoundDate), format='%m/%d/%y' ) 
        UpperBoundDate <- format.Date( as.character(UpperBoundDate), format='%m/%d/%y' )
        
        strDiagnostics <- paste(strDiagnostics, " Percent Variability: ", percVariability, " Est Var: ", thisVar, " CILowerBound: ", CILowerBound, " CIUpperBound: ", CIUpperBound, sep=",")
        strDiagnostics <- paste(strDiagnostics, "EstRTEGDate=", EstRTEGDate, "EstRTEGDate2=", EstRTEGDate2, " LowerBoundDate=", LowerBoundDate, " UpperBoundDate=", UpperBoundDate, sep=",")

      }
      
      # Note: this should never happen now
      # Check to see if this is Problem Data: Estimated RTEG Date before the current date. 
      #currSysTime <- format.Date( as.character(Sys.time()), format='%m/%d/%y' ) 
      if ( EstRTEGDate < as.Date(CurrentDate) ) { 
        strDiagnostics <- paste(strDiagnostics, "Problem PID Test 4, ",  "EstRTEGDate=", EstRTEGDate, "EstRTEGDate2=", EstRTEGDate2, " System Time=", currSysTime, "Problem PID", sep=",")
      }

      #------------------------------------------------------------------------------------------------------------
      # add row to dataframe in final format for ProModel
      newRow <- data.frame( 
        DateRun = format.Date( as.Date(CurrentDate), format='%m/%d/%y' ),
        DeliveryNumber = thisPIDInfo[1,iColPID], 
        PIDState = thisPIDState,
        BestDistribution = strThisBestDistr,
        BestEstimator = strPrimaryEstimator,
        EstimatedRTEGDate = EstRTEGDate2,
        LowerDate = LowerBoundDate,
        UpperDate = UpperBoundDate,  
        EG = thisPIDInfo[1,iColEG],
        DeploymentClass = thisPIDInfo[1,iColDeploymentClass],
        ProjectCategory = thisPIDInfo[1,iColProjectCategory], 
        DataCenterLocation = thisPIDInfo[1,iColDataCenterLocation], 
        DataCenterRegion = thisPIDInfo[1,iColDataCenterRegion], 
        CurrentMileStoneName = thisPIDInfo[1,iColCurrentMileStoneName], 
        MileStoneSequenceNUmber = thisPIDInfo[1,iColMileStoneSequenceNUmber], 
        DPMType = thisPIDInfo[1,iColDPMType],
        PIDMilestoneStartTime = format.Date( PIDMilestoneStartTime, format='%m/%d/%y' ),  
        PIDTimeInMilestoneHr = thisPIDInfo[1,iColPIDTimeInMilestone],
        PIDTimeInMilestoneDay = thisPIDTimeinMilestoneDay,
        TimeToComplete = ceiling(thisEstimate),
        LowerBound = CILowerBound,
        UpperBound = CIUpperBound,
        Mean = thisMean,
        Median = thisMedian,
        LB_Mean = CILowerBound_Mean,
        UB_Mean = CIUpperBound_Mean,
        LB_Median = CILowerBound_Median,
        UB_Median = CIUpperBound_Median,
        LB_SLA = CILowerBound_SLA,
        UB_SLA = CIUpperBound_SLA,
        StandardDeviation = thisStdDev,
        InterquartileRange = round( (summary(newDFDays)[iSumm75th] - summary(newDFDays)[iSumm25th]), nbrDigits),
        FirstQuartile = round(summary(newDFDays)[iSumm25th], nbrDigits),
        ThirdQuartile = round(summary(newDFDays)[iSumm75th], nbrDigits),
        LowerWhisker = WhiskerLower,
        UpperWhisker = WhiskerUpper,
        LengthOrigDataset = lengthCTOrig,
        LengthFinalDataset = lengthCTFinal,
        NumberOutliersRemoved = lengthCTOrig - lengthCTFinal
      )
      dfOutput <- rbind(dfOutput, newRow)
      
      # add row to Diagnostics
      newDiagRow <- data.frame( 
        PIDCount = countPID,
        DistributionType = strThisBestDistr,
        Diagnostics= strDiagnostics
      )
      dfDiagnostics <- rbind(dfDiagnostics, newDiagRow)
      
      strPrimaryEstimator <- ""
      strDiagnostics <- ""
      countPID <- countPID + 1      
                  
} # end Loop over PIDs of interest

# Resetting row names. Bug somewhere is causing these to be like 1st QU ???
rownames(dfOutput) = 1:nrow(dfOutput)


#------------------------------------------------------------------------------------------------------------
# Output Results to File

write.csv(file="C:/aaProjects/aaStats/aaaWhitePaper/Andrew/POC1/RScripts/RTEGPrediction.csv", dfOutput)

write.csv(file="C:/aaProjects/aaStats/aaaWhitePaper/Andrew/POC1/RScripts/Diagnostics.csv", dfDiagnostics)

write.csv(file="C:/aaProjects/aaStats/aaaWhitePaper/Andrew/POC1/RScripts/FinalCTDataset.csv", dfNewCTDays)



warnings()



#------------------------------------------------------------------------------------------------------------
# Enhancements

# handle case Est RTEG < Today
# Include est based on Mean & CI
# Include est based on SLA, with %variation to give upper/lower bounds
#
# 

# warnings()



#------------------------------------------------------------------------------------------------------------
# References

# reference for CI
# http://stackoverflow.com/questions/28419329/r-min-max-mean-and-median-of-a-vector-within-95-confidence-interval-2-5-to

#http://stackoverflow.com/questions/2254986/how-to-subtract-days-in-r

#http://www.bing.com/search?q=r%20programming%20median%20confidence%20interval&qs=n&form=QBRE&pq=r%20programming%20median%20confidence%20interval&sc=0-40&sp=-1&sk=&cvid=DF5596559A4C4F228B09F517DEDA7635



