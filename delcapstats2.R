delcapstats2<-function(file1){
  
  ####################
  ##
  ##This script calculates the mean, median, and 95th percentile for deployments
  ##Input file is the Delivered Capacity report from DERA, exported as CSV.
  ##As an interim step in Excel, add a column called yearmonth_RTEG and yearquarter_RTEG
  ##Use for the EXO Shiproom meeting with Jay Brickey
  ##Input file is the Delivered Capacity output for EXO
  ##
  ####################
  
  
  library(dplyr)
  library(lubridate)
  
  ##files_list<-list.files(directory,full.names=TRUE)
  ##delcap<-data.frame()
  ##delcap<-read.csv(files_list)
  
  ##read the file and convert it to dplyr table/dataframe using tbl_df
  mydf<-read.csv(file1, stringsAsFactors = FALSE)
  cran <- tbl_df(mydf)
  
  crantemp1<-mutate(cran, RTEGActualDeliveryDate3 = mdy(RTEGActualDeliveryDate2))
  crantemp2<-mutate(crantemp1, yearmonth_RTEG = format(ymd(RTEGActualDeliveryDate3),"%m %Y"))
  crantemp3<-mutate(crantemp2, yearmonth_RTEG = quarter(RTEGActualDeliveryDate3, with_year=TRUE))
  cran2 <- select(crantemp3, yearmonth_RTEG, DTR, ProjectCategory, yearquarter_RTEG)  ##extract the desired columns
  cran2[,2]<-suppressWarnings(as.numeric(cran2[,2]))  ##force numeric format on DTR column
  cran4 <- cran2  ##copy cran2 to cran4
  
 ##create a new table that's grouped by month
 print("By month")
 cran3 <- cran2 %>%
    group_by(yearmonth_RTEG, ProjectCategory) %>%
    summarize(count = n(),
              avg_CT = mean(DTR, na.rm = TRUE),
              median_CT = median(DTR, na.rm = TRUE),
              ninetyfifth_percentile = quantile(DTR,.95, na.rm = TRUE)
              ) %>%
    #filter(ProjectCategory == "PRD") %>%
    arrange(yearmonth_RTEG, ProjectCategory)
  #print
  
 cran3_prd_monthly <- cran3[cran3$ProjectCategory=="PRD",]
 cran3_discrete_monthly <- cran3[cran3$ProjectCategory=="Discrete",]
 print(cran3_prd_monthly)
 print(cran3_discrete_monthly)
 
 ##create a new table that's grouped by quarter
 print("By quarter")
 cran5 <- cran4 %>%
   group_by(yearquarter_RTEG, ProjectCategory) %>%
   summarize(count = n(),
             avg_CT = mean(DTR, na.rm = TRUE),
             median_CT = median(DTR, na.rm = TRUE),
             ninetyfifth_percentile = quantile(DTR,.95, na.rm = TRUE)
   ) %>%
   #filter(ProjectCategory == "PRD") %>%
   arrange(yearquarter_RTEG, ProjectCategory)
  
 cran5_prd_monthly <- cran5[cran5$ProjectCategory=="PRD",]
 cran5_discrete_monthly <- cran5[cran5$ProjectCategory=="Discrete",]

 print(cran5_prd_monthly)
 print(cran5_discrete_monthly)
 

  
}