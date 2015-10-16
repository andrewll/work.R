didextract<-function(){
  
  #########################
  #
  #  Purpose: to clean up the data file exported from MSPOD
  #
  #########################
  
  library(dplyr)
  library(lubridate)
  
  ##read input file
  mydf <- read.csv("C:/Users/andrewll/Documents/R/MCIOdata/All/spodids.csv", stringsAsFactors = TRUE)
  
  ##select variables desired
  mydf2 <- subset(mydf, select = c("Id", 
                                   "Title",
                                   "Description",
                                   "Demand.Status",
                                   "Property.Group",
                                   "RTEGDate",
                                   "Security.Classification",
                                   "Demand.Type",
                                   "Created.By",
                                   "Modified.By",
                                   "Tags",
                                   "Demand.Confidence",
                                   "CAPEX.Approval.Month",
                                   "TAM.Award.Date"))
  
  ##convert dates into correct format
  mydf2$RTEGDate<- as.Date(mydf2$RTEGDate, format = "%m/%d/%Y")
  mydf2$CAPEX.Approval.Month<- as.Date(mydf2$CAPEX.Approval.Month  , format = "%m/%d/%Y")
  mydf2$TAM.Award.Date<- as.Date(mydf2$TAM.Award.Date  , format = "%m/%d/%Y")
  

  
}