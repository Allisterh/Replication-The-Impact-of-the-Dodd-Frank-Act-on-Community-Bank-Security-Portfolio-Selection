  #Clear Variables
  rm(list=ls())

  #Loading Necessary Packages 
  library("tidyverse")
  library("lubridate")
  library("ggthemes") 
  library("gridExtra")
  library("sandwich")
  library("lmtest")
  library("lfe")
  library("foreign")
  library("reshape2")
  library("directlabels")
  library("plm")
  library("stargazer")
  library("quantreg")
  library("GGally")
  library("VGAM")
  library("Zelig")
  library("censReg")
  library("stargazer")


  
  #Setting Working Directory
  setwd("//adsroot.itcs.umich.edu/home/amwirth/Documents/Research Projects/939A Dodd Frank/Call_Report_Data")
  
  
  
  #Fucntion Creating Quarter End Dates for Reading in Time Series of Data
  Qtrs <- function(Start, End) {
    Vec <- as.Date(levels(cut(seq.Date(Start, End, by = "month"),
                              breaks = "quarter")))
    Vec[-1] - 1
  }
  
  
  #Creating a date variable to read in the text files,
  #note, enter one quarter beyond what you need for function to work
  dates <- Qtrs(as.Date("2008-03-31"), as.Date("2018-06-30"))
  
  
  #Next, taking vector of dates, and putting them into readable format
  #for reading in the text files from my computer folders
  dates_vector <- c()
  for (i in 1:length(dates)) {
    dates_vector[i] <- format(dates[i], format = "%m %d %Y")
    dates_vector[i] <- str_replace_all(string = dates_vector[i], pattern = " ", repl = "")
  }
  dates_vector <- factor(dates_vector, levels = dates_vector, ordered = TRUE)
  
  
  ###Reading in High Level Summary Data such as Bank Name, 
  ###State, Address, City, Zip code, had to use read_tsv because read.table was giving me error
  POR <- vector(mode = "list", length = length(dates_vector))
  for (i in 1:length(dates_vector))
  {
    path=paste("\\\\adsroot.itcs.umich.edu/home/amwirth/Documents/Research Projects/939A Dodd Frank/Call_Report_Data","/",dates_vector[i],"/",
               "FFIEC CDR Call Bulk POR ",dates_vector[i],'.txt', sep = "")
    POR[[i]] <- read_tsv(path)
    colnames(POR[[i]])[1] <- "IDRSSD_"
  }
  
  
  
  ####Reading in Income Statment Data
  RI <- vector(mode = "list", length = length(dates_vector))
  Headers_RI <- vector(mode = "list", length = length(dates_vector))
  Headers_RI_Names <- vector(mode = "list", length = length(dates_vector))
  
  for (i in 1:length(dates_vector))
  {
    path=paste("\\\\adsroot.itcs.umich.edu/home/amwirth/Documents/Research Projects/939A Dodd Frank/Call_Report_Data","/",dates_vector[i],"/",
               "FFIEC CDR Call Schedule RI ",dates_vector[i],'.txt', sep = "")
    Headers_RI[[i]] <- read_tsv(path, col_names = FALSE, n_max = 2)
    Headers_RI_Names[[i]] <- sapply(Headers_RI[[i]], paste, collapse = "_")
    RI[[i]] <- read.table(path, header = FALSE, sep = "\t", skip = 2, fill = TRUE)
    names(RI[[i]]) <- Headers_RI_Names[[i]]
    colnames(RI[[i]])[1] <- "IDRSSD_"
  }
  


  
  ##Reading in Balance Sheet Data, ie, Schedule RC from dates
  ##RC is a list of dataframes for each quarter of data
  RC <- vector(mode = "list", length = length(dates_vector))
  Headers_RC <- vector(mode = "list", length = length(dates_vector))
  Headers_RC_Names <- vector(mode = "list", length = length(dates_vector))
    
  for (i in 1:length(dates_vector))
    {
      path=paste("\\\\adsroot.itcs.umich.edu/home/amwirth/Documents/Research Projects/939A Dodd Frank/Call_Report_Data","/",dates_vector[i],"/",
                 "FFIEC CDR Call Schedule RC ",dates_vector[i],'.txt', sep = "")
      Headers_RC[[i]] <- read.table(path, header = FALSE, sep = '\t', fill = TRUE, nrows = 2)
      Headers_RC_Names[[i]] <- sapply(Headers_RC[[i]], paste, collapse = "_")
      RC[[i]] <- read.table(path, header = FALSE, sep = '\t', fill = TRUE, skip = 2)
      names(RC[[i]]) <- Headers_RC_Names[[i]]
      colnames(RC[[i]])[1] <- "IDRSSD_"
    }
  
  
  ##Reading in Secuirities Data, ie, Schedule RCB from dates_vector
  ##Data is a List of Securities Information DataFrames
  ##RCB was not broken into two files until 06/30/2009
  RCB1 <- vector(mode = "list", length = length(dates_vector))
  Headers_RCB1 <- vector(mode = "list", length = length(dates_vector))
  Headers_RCB1_Names <- vector(mode = "list", length = length(dates_vector))
  RCB2 <- vector(mode = "list", length = length(dates_vector))
  Headers_RCB2 <-vector(mode = "list", length = length(dates_vector))
  Headers_RCB2_Names <- vector(mode = "list", length = length(dates_vector))
  RCB <- vector(mode = "list", length = length(dates_vector))
  
  
  for (i in 1:length(dates_vector))
  { 
    if (dates_vector[i] < "06302009") 
      {
      path1=paste("\\\\adsroot.itcs.umich.edu/home/amwirth/Documents/Research Projects/939A Dodd Frank/Call_Report_Data","/",dates_vector[i],"/",
                  "FFIEC CDR Call Schedule RCB ",dates_vector[i],'.txt', sep = "")
      Headers_RCB1[[i]] <- read.table(path1, header = FALSE, fill = TRUE, sep = "\t", nrows = 2)
      Headers_RCB1_Names[[i]] <- sapply(Headers_RCB1[[i]], paste, collapse = "_")
      RCB1[[i]] <- read.table(path1, header = FALSE, fill = TRUE, sep = "\t", skip = 2)
      names(RCB1[[i]]) <- Headers_RCB1_Names[[i]]
      RCB[[i]] <- RCB1[[i]]
      } else { 
    path1=paste("\\\\adsroot.itcs.umich.edu/home/amwirth/Documents/Research Projects/939A Dodd Frank/Call_Report_Data","/",dates_vector[i],"/",
                "FFIEC CDR Call Schedule RCB ",dates_vector[i],'(1 of 2)','.txt', sep = "")
    Headers_RCB1[[i]] <-  read.table(path1, header = FALSE, fill = TRUE, sep = "\t", nrows = 2)
    Headers_RCB1_Names[[i]] <- sapply(Headers_RCB1[[i]], paste, collapse = "_")
    RCB1[[i]] <- read.table(path1, header = FALSE, fill = TRUE, sep = "\t", skip = 2)
    names(RCB1[[i]]) <- Headers_RCB1_Names[[i]]
    path2=paste("\\\\adsroot.itcs.umich.edu/home/amwirth/Documents/Research Projects/939A Dodd Frank/Call_Report_Data","/",dates_vector[i],"/",
                "FFIEC CDR Call Schedule RCB ",dates_vector[i],'(2 of 2)','.txt', sep = "")
    Headers_RCB2[[i]] <- read.table(path2, header = FALSE, fill = TRUE, sep = "\t", nrows = 2)
    Headers_RCB2_Names[[i]] <- sapply(Headers_RCB2[[i]], paste, collapse = "_")
    RCB2[[i]] <- read.table(path2, header = FALSE, fill = TRUE, sep = "\t", skip = 2)
    names(RCB2[[i]]) <- Headers_RCB2_Names[[i]]
    RCB[[i]] <- left_join(RCB1[[i]], RCB2[[i]], "IDRSSD_" )
      }
  }
  

  
  
  ####Reading in text files for capital and leverage ratio, note, having issues here
  ###text file definitions changed over time, I have a documentation 
  ##excel workbook on how they changed over sample periods, also
  ##I am using read_tsv instead of read.table because read.table, having issues reading
  ## in data for before 03312014, read.table works for everything after that, ##
  ## read.table works for the skip = 2 statement but not for nrows = 2 when reading in headers 

  RCR <- vector(mode = "list", length = length(dates_vector))
  RCR1 <- vector(mode = "list", length = length(dates_vector))
  Headers_RCR1 <- vector(mode = "list", length = length(dates_vector))
  Headers_RCR1_Names <- vector(mode = "list", length = length(dates_vector))
  RCR2 <- vector(mode = "list", length = length(dates_vector))
  Headers_RCR2 <- vector(mode = "list", length = length(dates_vector))
  Headers_RCR2_Names <- vector(mode = "list", length = length(dates_vector))
  
  

  for (i in 1:length(dates_vector))
  { 
    if (dates_vector[i] < "03312014") 
    {RCR[[i]] <- NULL
      path1=paste("\\\\adsroot.itcs.umich.edu/home/amwirth/Documents/Research Projects/939A Dodd Frank/Call_Report_Data","/",dates_vector[i],"/",
                  "FFIEC CDR Call Schedule RCR ",dates_vector[i],'(1 of 2)','.txt', sep = "")
      Headers_RCR1[[i]] <- read_tsv(path1, col_names = FALSE, n_max = 2)
      Headers_RCR1_Names[[i]] <- sapply(Headers_RCR1[[i]], paste, collapse = "_")
      RCR1[[i]] <- read.table(path1, header = FALSE, sep = "\t", fill = TRUE, skip = 2)
      names(RCR1[[i]]) <- Headers_RCR1_Names[[i]]
      path2=paste("\\\\adsroot.itcs.umich.edu/home/amwirth/Documents/Research Projects/939A Dodd Frank/Call_Report_Data","/",dates_vector[i],"/",
                  "FFIEC CDR Call Schedule RCR ",dates_vector[i],'(2 of 2)','.txt', sep = "")
      Headers_RCR2[[i]] <- read_tsv(path2, col_names = FALSE, n_max = 2)
      Headers_RCR2_Names[[i]] <- sapply(Headers_RCR2[[i]], paste, collapse = "_")
      RCR2[[i]] <- read.table(path2, header = FALSE, sep = "\t", fill = TRUE, skip = 2)
      names(RCR2[[i]]) <- Headers_RCR2_Names[[i]]
      RCR[[i]] <- left_join(RCR1[[i]], RCR2[[i]], "IDRSSD_NA" )
    } 
    else if (dates_vector[i] > "12312013" &  dates_vector[i] < "03312015") {
       path1=paste("\\\\adsroot.itcs.umich.edu/home/amwirth/Documents/Research Projects/939A Dodd Frank/Call_Report_Data","/",dates_vector[i],"/",
                   "FFIEC CDR Call Schedule RCRIA ",dates_vector[i],'.txt', sep = "")
       Headers_RCR1[[i]] <-  read_tsv(path1, col_names = FALSE, n_max = 2)
       Headers_RCR1_Names[[i]] <- sapply(Headers_RCR1[[i]], paste, collapse = "_")
       RCR1[[i]] <- read.table(path1, header = FALSE, sep = "\t", fill = TRUE, skip = 2)
       names(RCR1[[i]]) <- Headers_RCR1_Names[[i]]
       RCR[[i]] <- RCR1[[i]]
     } else {
       path1=paste("\\\\adsroot.itcs.umich.edu/home/amwirth/Documents/Research Projects/939A Dodd Frank/Call_Report_Data","/",dates_vector[i],"/",
                   "FFIEC CDR Call Schedule RCRI ",dates_vector[i],'.txt', sep = "")
       Headers_RCR1[[i]] <- read_tsv(path1, col_names = FALSE, n_max = 2)
       Headers_RCR1_Names[[i]] <- sapply(Headers_RCR1[[i]], paste, collapse = "_")
       RCR1[[i]] <-read.table(path1, header = FALSE, sep = "\t", fill = TRUE, skip = 2)
       names(RCR1[[i]]) <- Headers_RCR1_Names[[i]]
       RCR[[i]] <- RCR1[[i]]
     }
    colnames(RCR[[i]])[[1]] <- "IDRSSD_"
  }
  
  

  
  #Last, I am creating a list of merged bank data for each quarter end
  ##Need to join multple list, RC, RCB, RI, POR, and RCR
  Bank_Data_Merged <- list()
  for (i in 1:length(dates_vector)) {
    Bank_Data_Merged[[i]] <- left_join(POR[[i]], RI[[i]], "IDRSSD_" )
    Bank_Data_Merged[[i]] <- left_join(Bank_Data_Merged[[i]], RC[[i]], "IDRSSD_") 
    Bank_Data_Merged[[i]] <- left_join(Bank_Data_Merged[[i]], RCB[[i]], "IDRSSD_")
    Bank_Data_Merged[[i]] <- left_join(Bank_Data_Merged[[i]], RCR[[i]], "IDRSSD_")
  }
  
  
  #Naming Final List of DataFrames from Every QuarterEnd of Sample
  names(Bank_Data_Merged) <- dates_vector
  
  
  
  #Adding a quarter column to every data frame
  for (i in 1:length(dates_vector)) { 
    Bank_Data_Merged[[i]] <- 
      mutate(.data = Bank_Data_Merged[[i]], "Quarter" = dates_vector[i])
    }
  
  
  
  
  #Removing variables From all of my Data Frames
  #causing problems in creating one dataframe for merging 
  for (i in 1:length(dates_vector)) { 
    Bank_Data_Merged[[i]]$`RCON8678_FISCAL YEAR END` <- NULL
    Bank_Data_Merged[[i]]$`RCFD6724_AUDIT INDICATOR` <- NULL
    Bank_Data_Merged[[i]]$`RCON6724_AUDIT INDICATOR` <- NULL
    Bank_Data_Merged[[i]]$`RCFD1754_HELD-TO-MATURITY SECURITIES.y` <- NULL
    Bank_Data_Merged[[i]]$`RCON1754_HELD-TO-MATURITY SECURITIES.y` <- NULL
    Bank_Data_Merged[[i]]$`RCFD1773_AVAILABLE-FOR-SALE SECURITIES.y` <- NULL
    Bank_Data_Merged[[i]]$`RCON1773_AVAILABLE-FOR-SALE SECURITIES.y` <- NULL
    Bank_Data_Merged[[i]]$`RCOW3792_TOTAL RISK-BASED CAPITAL` <- NULL
    Bank_Data_Merged[[i]]$`RCOW5310_ALLL INCLUDIBLE IN TIER2 CAPITAL` <- NULL
    Bank_Data_Merged[[i]]$`RCOW5311_TIER 2 (SUPPLEMENTARY) CAPITAL` <- NULL
    Bank_Data_Merged[[i]]$`RCOWA223_RISK-WEIGHTED ASSETS` <- NULL
    Bank_Data_Merged[[i]]$`RCOWP870_TIER 2 CAPITAL BEFORE DEDUCTIONS` <- NULL
    Bank_Data_Merged[[i]]$`RCFAH313_ELIGIBLE RETAINED INCOME` <- NULL
    Bank_Data_Merged[[i]]$`RCFAH314_DIST AND DISCRE BONUS PAYMENTS QTR` <- NULL
    Bank_Data_Merged[[i]]$`RCOAH313_ELIGIBLE RETAINED INCOME` <- NULL
    Bank_Data_Merged[[i]]$`RCOAH314_DIST AND DISCRE BONUS PAYMENTS QTR` <- NULL
    Bank_Data_Merged[[i]]$`RIADF228_NONCASH INCOME NEG AMRTZ 1-4 RES PRO` <- NULL
    Bank_Data_Merged[[i]]$`RIAD9106_ACQUISITION DATE` <- NULL
    }
  

  
  ##Creating a Data Frame with all observations 
  Bank_Data_Merged_Final <- bind_rows(Bank_Data_Merged)
  Bank_Data_Merged_Final$Quarter <- factor(Bank_Data_Merged_Final$Quarter, levels = 
                                    dates_vector, ordered = TRUE)
  

  ##Creating DataFrame of FFIEC 041, Domestic Offices Only
  Bank_Data_Merged_Final_Domestic <- Bank_Data_Merged_Final[is.na(Bank_Data_Merged_Final$`RCFD2170_TOTAL ASSETS`) & 
                                                              is.na(Bank_Data_Merged_Final$`RCFD2170_TOTAL ASSETS.x`) &  
                                                                    is.na(Bank_Data_Merged_Final$`RCFD2170_TOTAL ASSETS.y`), ]
                                                                
                                                              
  ##Editing DataFrame to Create FFIEC 031 Banks, Domestic with Foreign Offices
  Bank_Data_Merged_Final_International <- Bank_Data_Merged_Final[!(is.na(Bank_Data_Merged_Final$`RCFD2170_TOTAL ASSETS`) & 
                                                                   is.na(Bank_Data_Merged_Final$`RCFD2170_TOTAL ASSETS.x`) &  
                                                                   is.na(Bank_Data_Merged_Final$`RCFD2170_TOTAL ASSETS.y`)), ]

  #######################
  ###Variable Creation Section for Domestic Banks
  ##Variable Definitions Change Over time for Created Ratios/Variables
  ##This is why I will have many variables for creating simple definitions 
  
  ##After 12/31/2012 dummy , law in effects 01/01/2013, new purhcases and exisiting must be investment grade
  Bank_Data_Merged_Final_Domestic <- 
    mutate(.data = Bank_Data_Merged_Final_Domestic, "After12312012_Logical" = Bank_Data_Merged_Final_Domestic$Quarter >12312012)
  
  Bank_Data_Merged_Final_Domestic <- 
    mutate(.data = Bank_Data_Merged_Final_Domestic, "After12312012_Dummy" = as.numeric(Bank_Data_Merged_Final_Domestic$After12312012_Logical))
  
  
  ###After 06/302012 - Law announced June of 2012
  Bank_Data_Merged_Final_Domestic <- 
    mutate(.data = Bank_Data_Merged_Final_Domestic, "After06302012_Logical" = Bank_Data_Merged_Final_Domestic$Quarter > "06302012")
  
  Bank_Data_Merged_Final_Domestic <- 
    mutate(.data = Bank_Data_Merged_Final_Domestic, "After06302012_Dummy" = as.numeric(Bank_Data_Merged_Final_Domestic$After06302012_Logical))
 
  
  ###Creating Bank Quarter Variable for Tier 1 Leverage Ratio 
  ###From 03312015 to present - RCOA7204, has to be converted to numeric 
  ##From 03312008 to 12312014 - RCON7204
  Bank_Data_Merged_Final_Domestic$`RCOA7204_TIER 1 LEVERAGE CAPITAL RATIO` <- 
   as.numeric(map_chr(Bank_Data_Merged_Final_Domestic$`RCOA7204_TIER 1 LEVERAGE CAPITAL RATIO`, str_replace, 
                      pattern = "%", replacement = ""))
 
 Bank_Data_Merged_Final_Domestic <- 
   mutate(.data = Bank_Data_Merged_Final_Domestic, RCON7204_Percentage = 100 * Bank_Data_Merged_Final_Domestic$`RCON7204_TIER 1 LEVERAGE CAPITAL RATIO`)
 
 
 Bank_Data_Merged_Final_Domestic <- 
   mutate(.data = Bank_Data_Merged_Final_Domestic, Tier1_Leverage_Capital_Ratio_Percentage = 
            ifelse(is.na(Bank_Data_Merged_Final_Domestic$`RCOA7204_TIER 1 LEVERAGE CAPITAL RATIO`) & 
                      is.na(Bank_Data_Merged_Final_Domestic$RCON7204_Percentage), NA, rowSums(
     Bank_Data_Merged_Final_Domestic[ , c("RCOA7204_TIER 1 LEVERAGE CAPITAL RATIO", "RCON7204_Percentage")],
     na.rm = TRUE)))
 
 
 
 ###Creating Bank Quarter Variable for Total Capital Ratio, i.e, (Tier1 + Tier2 Capital) / (Risk-Weighted Assets)
 ###From 03312015 to present - RCOA7205, has to be converted to numeric
 ###From 03312008 to 12312014 - RCON7205
 Bank_Data_Merged_Final_Domestic$`RCOA7205_TOTAL RISK-BASED CAPITAL RATIO` <- 
   as.numeric(map_chr(Bank_Data_Merged_Final_Domestic$`RCOA7205_TOTAL RISK-BASED CAPITAL RATIO`, str_replace, 
                      pattern = "%", replacement = ""))
 
 Bank_Data_Merged_Final_Domestic <- 
   mutate(.data = Bank_Data_Merged_Final_Domestic, RCON7205_Percentage = 100 * Bank_Data_Merged_Final_Domestic$`RCON7205_TOTAL RISK-BASED CAPITAL RATIO`)
 
 Bank_Data_Merged_Final_Domestic <- 
   mutate(.data = Bank_Data_Merged_Final_Domestic, Total_RiskBased_Capital_Ratio_Percentage = ifelse(
     is.na(Bank_Data_Merged_Final_Domestic$`RCOA7205_TOTAL RISK-BASED CAPITAL RATIO`) & 
       is.na(Bank_Data_Merged_Final_Domestic$RCON7205_Percentage), NA,rowSums(
     Bank_Data_Merged_Final_Domestic[ , c("RCOA7205_TOTAL RISK-BASED CAPITAL RATIO", "RCON7205_Percentage") ],
     na.rm = TRUE)))
 

 
 #####Creating Bank Quarter Variable for Net Income 
 Bank_Data_Merged_Final_Domestic <- 
   mutate(.data = Bank_Data_Merged_Final_Domestic, Net_Income = Bank_Data_Merged_Final_Domestic$`RIAD4340_NET INCOME`)
 
 
 
 ###Creating a Bank Quarter Variable for Total Assets - 
  ###Doing this to resolve merging issues later between FFIEC 031 and 041 Data Frames
  ##Have to do this because when merging dataframe two columns for Total_Assets were created that don't overlap in time series 
  Bank_Data_Merged_Final_Domestic <-  
    mutate(.data = Bank_Data_Merged_Final_Domestic, Total_Assets = rowSums(
      Bank_Data_Merged_Final_Domestic[ , c("RCON2170_TOTAL ASSETS.x", 
                                           "RCON2170_TOTAL ASSETS")], na.rm = TRUE))
  
  
  ###Creating a Bank Total_Liabilites Variable
  Bank_Data_Merged_Final_Domestic <- 
    mutate(.data = Bank_Data_Merged_Final_Domestic, Total_Liabilities = 
             Bank_Data_Merged_Final_Domestic$`RCON2948_TOTAL LIABILITIES`)
  
  
  ###Creating Bank Efficiency Variable - 
  ###Define as non-interest expense / (non-interest income + net interest income )
  Bank_Data_Merged_Final_Domestic <- mutate(.data = Bank_Data_Merged_Final_Domestic, 
                                            temporary_denominator = rowSums(
                                              Bank_Data_Merged_Final_Domestic[, c("RIAD4074_NET INTEREST INCOME",
                                                                                  "RIAD4079_TOTAL NONINTEREST INCOME")], na.rm = TRUE))
  Bank_Data_Merged_Final_Domestic <- mutate(.data = Bank_Data_Merged_Final_Domestic, 
                                            Efficiency_Ratio_Percentage = (Bank_Data_Merged_Final_Domestic$`RIAD4093_TOTAL NONINTEREST EXPENSE` / 
                                              temporary_denominator)*100)
  Bank_Data_Merged_Final_Domestic$temporary_denominator <- NULL
  

####Creating a variable for total investments that must meet "investment grade" standards of new regualation, 
#### note definition change at 06/30/2009 and 03/31/2011
Bank_Data_Merged_Final_Domestic <-  
  mutate(.data = Bank_Data_Merged_Final_Domestic,Securities_NoGov = 
             rowSums(Bank_Data_Merged_Final_Domestic[ ,c("RCON1709_OTHR PASS-THRU SECS-HLD-AMORTZD COST", 
                                                         "RCON1713_OTHR PASS-THRU SECS-AVL-FAIR VALUE", 
                                                         "RCON1733_MBS-CMOS&REMICS-ALL OTHER-HLD-AMRTZ", 
                                                         "RCON1736_MBS-CMOS&REMICS-ALL OTHER-AVL-FAIR",
                                                         "RCONC026_HTM(COST):TOTL ASSET-BACK SEC - DERV",  
                                                         "RCONC027_AFS(FAIR):TOTL ASSET-BACK SEC -DERV", 
                                                         "RCON1737_OTHR DOMSTC DEBT SECS-HLD-TO-MAT-AMR", 
                                                         "RCON1741_OTHR DOMSTC DEBT SECS-AVL-FR-SLE-FAI", 
                                                         "RCON1742_FGN DEBT SECS-HELD-TO-MATR-AMRTZ COS",  
                                                         "RCON1746_FGN DEBT SECS-AVL-FOR-SALE-FAIR VALU",
                                                         "RCONG308_MBS RPTS OTHR PT SCTY HTM AMRTZ COST", 
                                                         "RCONG320_MBS OTHR OTHR RES MBS HTM AMRTZ", 
                                                         "RCONK146_MBS CMRL PT SCTY OTHR HTM AMTRZ", 
                                                         "RCONK154_MBS CMRL OTHR CMRL OTHR CMRL HTM AMT",
                                                         "RCONG324_MBS CMRL MBS CMRL PT SCTY HTM AMRTZ", 
                                                         "RCONG328_MBS CMRL MBS OTHR CMRL MBS HTM AMRTZ",
                                                         "RCONG336_ABS STRD FNC PROD CASH HTM AMRTZ", 
                                                         "RCONG340_ABS STRD FNC PROD SYN HTM AMRTZ", 
                                                         "RCONG344_ABS STRD FNC PROD HYBRIDHTM AMRTZ", 
                                                         "RCONG311_MBS RPTS OTHR PT SCTY AFS FV", 
                                                         "RCONG323_MBS OTHR OTHR RES MBS AFS FV", 
                                                         "RCONK149_MBS CMRL PT SCTY OTHR AFS FV", 
                                                         "RCONG327_MBS CMRL MBS CMRL PT SCTY AFS FV",
                                                         "RCONG331_MBS CMRL MBS OTHR CMRL MBS AFS FV", 
                                                         "RCONK157_MBS CMRL OTHR CMRL OTHR CMRL AFS FV", 
                                                         "RCONG339_ABS STRD FNC PROD CASH AFS FV", 
                                                         "RCONG343_ABS STRD FNC PROD SYN AFS FV", 
                                                         "RCONG347_ABS STRD FNC PROD HYBRID FS FV")], na.rm = TRUE))

                                                                      
##Creating a variable for each bank quarter observation
##for total security investments, using ".x" variables because
##the same definition without ".x" only holds values before 2014
Bank_Data_Merged_Final_Domestic <-  
  mutate(.data = Bank_Data_Merged_Final_Domestic,Securities = rowSums(
    Bank_Data_Merged_Final_Domestic[ , c("RCON1754_HELD-TO-MATURITY SECURITIES.x", "RCON1773_AVAILABLE-FOR-SALE SECURITIES.x")]
    , na.rm = TRUE))



##Creating Bank Quarter Observations for the 
##Percentage of Securitiy Investments in Non-US-Gov FI Securiites, these are securities facing new regulation
Bank_Data_Merged_Final_Domestic <-  
  mutate(.data = Bank_Data_Merged_Final_Domestic, Percentage_Securities_NoGov = (Bank_Data_Merged_Final_Domestic$Securities_NoGov / 
           Bank_Data_Merged_Final_Domestic$Securities)*100)


##Creating a Bank Quarter Variable for Percentage of Total Assets in Securities and 
##a Percentage of Total Assets in Non-Gov FI Securities
Bank_Data_Merged_Final_Domestic <-  
  mutate(.data = Bank_Data_Merged_Final_Domestic, Percentage_Assets_Securities = (Bank_Data_Merged_Final_Domestic$Securities / 
           Bank_Data_Merged_Final_Domestic$Total_Assets)*100, 
         Percentage_Assets_Securities_NonGov = (Bank_Data_Merged_Final_Domestic$Securities_NoGov / 
           Bank_Data_Merged_Final_Domestic$Total_Assets)*100) 



##Creating a Bank Quarter Variables for Corporate Bond Investment and Foregin Debt Instruments, 
##This is labeled as just corporate because I realized later than 1742 and 1746 contain foreign government debt that can't be broken 
##out into different categories 
Bank_Data_Merged_Final_Domestic <-  
  mutate(.data = Bank_Data_Merged_Final_Domestic, Securities_Corporate = 
           rowSums(Bank_Data_Merged_Final_Domestic[ ,c("RCON1737_OTHR DOMSTC DEBT SECS-HLD-TO-MAT-AMR", 
                                                       "RCON1742_FGN DEBT SECS-HELD-TO-MATR-AMRTZ COS",
                                                       "RCON1741_OTHR DOMSTC DEBT SECS-AVL-FR-SLE-FAI", 
                                                       "RCON1746_FGN DEBT SECS-AVL-FOR-SALE-FAIR VALU")], na.rm = TRUE))




Bank_Data_Merged_Final_Domestic <-  
  mutate(.data = Bank_Data_Merged_Final_Domestic, Percentage_Securities_Corporate = (Bank_Data_Merged_Final_Domestic$Securities_Corporate / 
           Bank_Data_Merged_Final_Domestic$Securities)*100, 
         Percentage_Assets_Securities_Corporate = (Bank_Data_Merged_Final_Domestic$Securities_Corporate/ 
           Bank_Data_Merged_Final_Domestic$Total_Assets)*100)




##Creating a Bank Quarter Variables for Munis Investment
Bank_Data_Merged_Final_Domestic <-  
  mutate(.data = Bank_Data_Merged_Final_Domestic, Securities_Munis = rowSums(
    Bank_Data_Merged_Final_Domestic[ ,c("RCON8496_SECS ISSD BY US ST&POL SUBDYS-AMRTZ", 
                                        "RCON8499_SECS ISSD BY US ST&POL SUBDYS-FAIR")], na.rm = TRUE))


Bank_Data_Merged_Final_Domestic <- mutate(.data = Bank_Data_Merged_Final_Domestic, 
                                          Percentage_Securities_Munis = (Bank_Data_Merged_Final_Domestic$Securities_Munis/ 
                                            Bank_Data_Merged_Final_Domestic$Securities)*100, 
                                          Percentage_Assets_Securities_Munis = (Bank_Data_Merged_Final_Domestic$Securities_Munis / 
                                            Bank_Data_Merged_Final_Domestic$Total_Assets)*100)



##Creating a Variable for Private Label MBS, ABS, and Structured Products 
Bank_Data_Merged_Final_Domestic <- mutate(.data = Bank_Data_Merged_Final_Domestic, 
                                          Securities_Private_MBS_ABS_Structured = Securities_NoGov - Securities_Corporate)

Bank_Data_Merged_Final_Domestic <- mutate(.data = Bank_Data_Merged_Final_Domestic, 
                                          Percentage_Securities_Private_MBS_ABS_Structured = (Securities_Private_MBS_ABS_Structured/ 
                                                                                                Securities)*100, 
                                          Percentage_Assets_Private_MBS_ABS_Structured = (Securities_Private_MBS_ABS_Structured / 
                                                                                            Total_Assets)*100)


##Creating a Variable for Percentage of Assets in Loans
Bank_Data_Merged_Final_Domestic <- 
  mutate(.data = Bank_Data_Merged_Final_Domestic, Percentage_Assets_Loans = (rowSums(
    Bank_Data_Merged_Final_Domestic[ , c("RCONB529_LOANS & LEASES, NET UNEARN INC&ALLOW", 
                                         "RCON5369_LOANS AND LEASES HELD FOR SALE", 
                                         "RCON5369_LOANS AND LEASES HELD FOR SALE.x")], na.rm = TRUE)) / (
                                           Total_Assets))
                                         

    

########################
         
######Creating Identical Variables for Banks Filing FFIEC031, Banks with Domestic and International Offices 


##After 12/31/2012 dummy , law in effects 01/01/2013, new purhcases and exisiting must be investment grade
Bank_Data_Merged_Final_International <- 
  mutate(.data = Bank_Data_Merged_Final_International, "After12312012_Logical" = Bank_Data_Merged_Final_International$Quarter > "12312012")

Bank_Data_Merged_Final_International <- 
  mutate(.data = Bank_Data_Merged_Final_International, "After12312012_Dummy" = as.numeric(Bank_Data_Merged_Final_International$After12312012_Logical))


###After 06/302012 - Law announced June of 2012
Bank_Data_Merged_Final_International <- 
  mutate(.data = Bank_Data_Merged_Final_International, "After06302012_Logical" = Bank_Data_Merged_Final_International$Quarter > "06302012")

Bank_Data_Merged_Final_International <- 
  mutate(.data = Bank_Data_Merged_Final_International, "After06302012_Dummy" = as.numeric(Bank_Data_Merged_Final_International$After06302012_Logical))




###Creating Bank Quarter Variable for Tier 1 Leverage Ratio 
###From 03312015 to present - RCFA7204, has to be converted to numeric 
##From 03312008 to 12312014 - RCFD7204

Bank_Data_Merged_Final_International$`RCFA7204_TIER 1 LEVERAGE CAPITAL RATIO` <- 
  as.numeric(map_chr(Bank_Data_Merged_Final_International$`RCFA7204_TIER 1 LEVERAGE CAPITAL RATIO`, str_replace, 
                     pattern = "%", replacement = ""))

Bank_Data_Merged_Final_International <- 
  mutate(.data = Bank_Data_Merged_Final_International, RCFD7204_Percentage = 100 * Bank_Data_Merged_Final_International$`RCFD7204_TIER 1 LEVERAGE CAPITAL RATIO`)


Bank_Data_Merged_Final_International <- 
  mutate(.data = Bank_Data_Merged_Final_International, Tier1_Leverage_Capital_Ratio_Percentage = ifelse(
    is.na(Bank_Data_Merged_Final_International$`RCFA7204_TIER 1 LEVERAGE CAPITAL RATIO`) & 
      is.na(Bank_Data_Merged_Final_International$RCFD7204_Percentage), NA, rowSums(
    Bank_Data_Merged_Final_International[ , c("RCFA7204_TIER 1 LEVERAGE CAPITAL RATIO", "RCFD7204_Percentage")],
    na.rm = TRUE)))


###Creating Bank Quarter Variable for Total Capital Ratio, i.e, (Tier1 + Tier2 Capital) / (Risk-Weighted Assets)
###From 03312015 to present - RCFA7205, has to be converted to numeric
###From 03312008 to 12312014 - RCFD7205

Bank_Data_Merged_Final_International$`RCFA7205_TOTAL RISK-BASED CAPITAL RATIO` <- 
  as.numeric(map_chr(Bank_Data_Merged_Final_International$`RCFA7205_TOTAL RISK-BASED CAPITAL RATIO`, str_replace, 
                     pattern = "%", replacement = ""))

Bank_Data_Merged_Final_International <- 
  mutate(.data = Bank_Data_Merged_Final_International, RCFD7205_Percentage = 100 * Bank_Data_Merged_Final_International$`RCFD7205_TOTAL RISK-BASED CAPITAL RATIO`)

Bank_Data_Merged_Final_International <- 
  mutate(.data = Bank_Data_Merged_Final_International, Total_RiskBased_Capital_Ratio_Percentage = ifelse(
    is.na(Bank_Data_Merged_Final_International$`RCFA7205_TOTAL RISK-BASED CAPITAL RATIO`) & 
      is.na(Bank_Data_Merged_Final_International$RCFD7205_Percentage), NA, rowSums(
    Bank_Data_Merged_Final_International[ , c("RCFA7205_TOTAL RISK-BASED CAPITAL RATIO", "RCFD7205_Percentage") ],
    na.rm = TRUE)))


#####Creating Bank Quarter Variable for Net Income
Bank_Data_Merged_Final_International <- 
  mutate(.data = Bank_Data_Merged_Final_International, Net_Income = Bank_Data_Merged_Final_International$`RIAD4340_NET INCOME`)
  
  
  
  ###Creating a Bank Quarter Variable for Total Assets - 
  Bank_Data_Merged_Final_International <-  
    mutate(.data = Bank_Data_Merged_Final_International, Total_Assets = rowSums(
      Bank_Data_Merged_Final_International[ , c("RCFD2170_TOTAL ASSETS.x", "RCFD2170_TOTAL ASSETS")], na.rm = TRUE))
  
  
  ###Creating a Bank Total_Liabilites Variable
  Bank_Data_Merged_Final_International <- 
    mutate(.data = Bank_Data_Merged_Final_International, Total_Liabilities = 
             Bank_Data_Merged_Final_International$`RCFD2948_TOTAL LIABILITIES`)


  
  ###Creating Bank Efficiency Variable - 
  ###Define as non-interest expense / (non-interest income + net interest income )
  Bank_Data_Merged_Final_International <- mutate(.data = Bank_Data_Merged_Final_International, 
                                                 temporary_denominator = rowSums(
                                                   Bank_Data_Merged_Final_International[, c("RIAD4074_NET INTEREST INCOME", 
                                                                                            "RIAD4079_TOTAL NONINTEREST INCOME")], na.rm = TRUE))
  Bank_Data_Merged_Final_International <- mutate(.data = Bank_Data_Merged_Final_International, 
                                                 Efficiency_Ratio_Percentage = (Bank_Data_Merged_Final_International$`RIAD4093_TOTAL NONINTEREST EXPENSE` /  
                                                   temporary_denominator)*100)
  Bank_Data_Merged_Final_Domestic$temporary_denominator <- NULL
  



  ##Creating variable for each bank quarter observation
  #for total investments that must meet "investment grade" standards of new regualation 
  ##for Non-Government and Non-Gov Guarranteed Fixed Income Investment Securities
  ##Same issues as in variable creation for domestic securities, for example, ".x" for some
  ##variables means before 06312009, also Form 041 changed at 06302009 and 03312011         
  Bank_Data_Merged_Final_International <-  
    mutate(.data = Bank_Data_Merged_Final_International,Securities_NoGov = 
           rowSums(Bank_Data_Merged_Final_International[ ,c("RCFDG308_MBS RPTS OTHR PT SCTY HTM AMRTZ COST", 
                                                            "RCFD1709_OTHR PASS-THRU SECS-HLD-AMORTZD COST", 
                                                            "RCFD1733_MBS-CMOS&REMICS-ALL OTHER-HLD-AMRTZ", 
                                                            "RCFD1713_OTHR PASS-THRU SECS-AVL-FAIR VALUE", 
                                                            "RCFD1736_MBS-CMOS&REMICS-ALL OTHER-AVL-FAIR",
                                                            "RCFDG320_MBS OTHR OTHR RES MBS HTM AMRTZ",
                                                            "RCFDK146_MBS CMRL PT SCTY OTHR HTM AMTRZ",
                                                            "RCFDG324_MBS CMRL MBS CMRL PT SCTY HTM AMRTZ",
                                                            "RCFDK154_MBS CMRL OTHR CMRL OTHR CMRL HTM AMT",
                                                            "RCFDG328_MBS CMRL MBS OTHR CMRL MBS HTM AMRTZ", 
                                                            "RCFDC026_HTM(COST):TOTL ASSET-BACK SEC - DERV", 
                                                            "RCFDG336_ABS STRD FNC PROD CASH HTM AMRTZ", 
                                                            "RCFDG340_ABS STRD FNC PROD SYN HTM AMRTZ", 
                                                            "RCFDG344_ABS STRD FNC PROD HYBRIDHTM AMRTZ", 
                                                            "RCFD1737_OTHR DOMSTC DEBT SECS-HLD-TO-MAT-AMR", 
                                                            "RCFD1742_FGN DEBT SECS-HELD-TO-MATR-AMRTZ COS", 
                                                            "RCFDG311_MBS RPTS OTHR PT SCTY AFS FV", 
                                                            "RCFDG323_MBS OTHR OTHR RES MBS AFS FV",  
                                                            "RCFDK149_MBS CMRL PT SCTY OTHR AFS FV", 
                                                            "RCFDG327_MBS CMRL MBS CMRL PT SCTY AFS FV", 
                                                            "RCFDK157_MBS CMRL OTHR CMRL OTHR CMRL AFS FV",
                                                            "RCFDG331_MBS CMRL MBS OTHR CMRL MBS AFS FV", 
                                                            "RCFDC027_AFS(FAIR):TOTL ASSET-BACK SEC -DERV", 
                                                            "RCFDG339_ABS STRD FNC PROD CASH AFS FV", 
                                                            "RCFDG343_ABS STRD FNC PROD SYN AFS FV", 
                                                            "RCFDG347_ABS STRD FNC PROD HYBRID FS FV",   
                                                            "RCFD1741_OTHR DOMSTC DEBT SECS-AVL-FR-SLE-FAI", 
                                                            "RCFD1746_FGN DEBT SECS-AVL-FOR-SALE-FAIR VALU")], 
                                                      na.rm = TRUE))

  
  
  
  
  ##Creating a variable for total security investments
  Bank_Data_Merged_Final_International <-  
    mutate(.data = Bank_Data_Merged_Final_International,Securities = rowSums(
      Bank_Data_Merged_Final_International[ , c("RCFD1754_HELD-TO-MATURITY SECURITIES.x", 
                                                "RCFD1773_AVAILABLE-FOR-SALE SECURITIES.x")]
      ,na.rm = TRUE))
  
  
  
  ##Creating Bank Quarter Observations for the 
  ##Percentage of Securitiy Investments in Non-US-Gov FI Securiites, the securites that face new regulaton
  Bank_Data_Merged_Final_International <-  
    mutate(.data = Bank_Data_Merged_Final_International, Percentage_Securities_NoGov = (Bank_Data_Merged_Final_International$Securities_NoGov / 
             Bank_Data_Merged_Final_International$Securities)*100)
  
  
  
  ##Creating a Bank Quarter Variable for Percentage of Total Assets in Securities and 
  ##a Percentage of Total Assets in Non-Gov FI Securities
  Bank_Data_Merged_Final_International <-  
    mutate(.data = Bank_Data_Merged_Final_International, Percentage_Assets_Securities = (Bank_Data_Merged_Final_International$Securities / 
             Bank_Data_Merged_Final_International$Total_Assets)*100, 
           Percentage_Assets_Securities_NonGov = (Bank_Data_Merged_Final_International$Securities_NoGov / 
             Bank_Data_Merged_Final_International$Total_Assets)*100) 
  
  
  
  
  ##Creating a Bank Quarter Variables for Corporate Bond Investment and Foregin Debt Instruments, 
  ##This is labeled as just corporate because I realized later than 1742 and 1746 contain foreign government debt that can't be broken 
  ##out into different categories 
  Bank_Data_Merged_Final_International <-  
    mutate(.data = Bank_Data_Merged_Final_International, Securities_Corporate = 
             rowSums(Bank_Data_Merged_Final_International[ ,c("RCFD1737_OTHR DOMSTC DEBT SECS-HLD-TO-MAT-AMR", 
                                                              "RCFD1742_FGN DEBT SECS-HELD-TO-MATR-AMRTZ COS",
                                                              "RCFD1741_OTHR DOMSTC DEBT SECS-AVL-FR-SLE-FAI",
                                                              "RCFD1746_FGN DEBT SECS-AVL-FOR-SALE-FAIR VALU")], 
                     na.rm = TRUE))

  
  Bank_Data_Merged_Final_International <-  
    mutate(.data = Bank_Data_Merged_Final_International, Percentage_Securities_Corporate = (Bank_Data_Merged_Final_International$Securities_Corporate / 
             Bank_Data_Merged_Final_International$Securities)*100, 
           Percentage_Assets_Securities_Corporate = (Bank_Data_Merged_Final_International$Securities_Corporate/ 
             Bank_Data_Merged_Final_International$Total_Assets)*100) 
  
  
  ##Creating a Bank Quarter Variables for Munis Investment
  Bank_Data_Merged_Final_International <-  
    mutate(.data = Bank_Data_Merged_Final_International, Securities_Munis = rowSums(
      Bank_Data_Merged_Final_International[ ,c("RCFD8496_SECS ISSD BY US ST&POL SUBDYS-AMRTZ",
                                               "RCFD8499_SECS ISSD BY US ST&POL SUBDYS-FAIR"), ]
      , na.rm = TRUE))
  
  
  Bank_Data_Merged_Final_International <- mutate(.data = Bank_Data_Merged_Final_International, 
                                            Percentage_Securities_Munis = (Bank_Data_Merged_Final_International$Securities_Munis/ 
                                              Bank_Data_Merged_Final_International$Securities)*100, 
                                            Percentage_Assets_Securities_Munis = (Bank_Data_Merged_Final_International$Securities_Munis / 
                                              Bank_Data_Merged_Final_International$Total_Assets)*100)
  
  
  ##Creating a Variable for Private Label MBS, ABS, and Structured Products 
  Bank_Data_Merged_Final_International <- mutate(.data = Bank_Data_Merged_Final_International, 
                                            Securities_Private_MBS_ABS_Structured = Securities_NoGov - Securities_Corporate)
  
  Bank_Data_Merged_Final_International <- mutate(.data = Bank_Data_Merged_Final_International, 
                                            Percentage_Securities_Private_MBS_ABS_Structured = (Securities_Private_MBS_ABS_Structured/ 
                                                                                                  Securities)*100, 
                                            Percentage_Assets_Private_MBS_ABS_Structured = (Securities_Private_MBS_ABS_Structured / 
                                                                                              Total_Assets)*100)
  
  
  
  ##Creating a Variable for Percentage of Assets in Loans
  Bank_Data_Merged_Final_International <- 
    mutate(.data = Bank_Data_Merged_Final_International, Percentage_Assets_Loans = (rowSums(
      Bank_Data_Merged_Final_International[ , c("RCFD5369_LOANS AND LEASES HELD FOR SALE",
                                                "RCFD5369_LOANS AND LEASES HELD FOR SALE.x",
                                                "RCFDB529_LOANS & LEASES, NET UNEARN INC&ALLOW")], na.rm = TRUE)) / (
                                             Total_Assets))
  
  
  #############################################
  ####Mering Domestic and Internatioanl DataFrames
  Bank_Data_Merged_Final_International$`RCFA7204_TIER 1 LEVERAGE CAPITAL RATIO` <- NULL
  Bank_Data_Merged_Final_International$`RCOA7204_TIER 1 LEVERAGE CAPITAL RATIO` <- NULL
  Bank_Data_Merged_Final_International$`RCFA7205_TOTAL RISK-BASED CAPITAL RATIO` <- NULL
  Bank_Data_Merged_Final_International$`RCOA7205_TOTAL RISK-BASED CAPITAL RATIO` <- NULL
  Bank_Data_Merged_Final_Domestic$`RCFA7204_TIER 1 LEVERAGE CAPITAL RATIO` <- NULL
  Bank_Data_Merged_Final_Domestic$`RCFA7205_TOTAL RISK-BASED CAPITAL RATIO` <- NULL
    
  Bank_Data_Merged_Final_Total <- bind_rows(Bank_Data_Merged_Final_Domestic, 
                                          Bank_Data_Merged_Final_International)


  ##############################################
  #Variable Defintion Cleanups and Clarifications before Analysis
  
  
  ###Eliminating Duplicates in 03312008 for just one bank,  this is in the data for some reason
  elimination <- which(Bank_Data_Merged_Final_Total$IDRSSD_ == 3391606 & Bank_Data_Merged_Final_Total$Quarter == "03312008")
  Bank_Data_Merged_Final_Total <- Bank_Data_Merged_Final_Total[-c(elimination[1:length(elimination)-1]),]
  
  
  ####Eliminating a Single Observation that is coming through where the bank has zero assets, than closes the following quarter 
  Bank_Data_Merged_Final_Total <- 
    Bank_Data_Merged_Final_Total%>% 
    filter(!(IDRSSD_ == 3384466 & Quarter == "06302014")) 
  
  

  
  ##Efficiency Ratio Variable - eliminating the few infinity values, and multiplying by 100
  ##to make into percentage and easier to understand Regression intrepretation 
  ##Mabye alter this slightly later due to non-monotone relationship at zero of efficiency ratio
  test <- vector()
  for (l in 1:length(Bank_Data_Merged_Final_Total$Efficiency_Ratio_Percentage)) { 
    if (is.infinite(Bank_Data_Merged_Final_Total$Efficiency_Ratio_Percentage[[l]])) { 
      test[[l]] = NaN} else { 
        test[[l]] = Bank_Data_Merged_Final_Total$Efficiency_Ratio_Percentage[[l]]
      }
  }
  Bank_Data_Merged_Final_Total$Efficiency_Ratio_Percentage <- test
  rm(test)
  
  

  ###Breaking Bank Quarter Obseravtions into Quantiles measured by Total Assets
  Number_Quantiles <- 10
  
  Bank_Data_Merged_Final_Total <- 
    group_by(.data = Bank_Data_Merged_Final_Total, Quarter) %>%
    mutate(Quantile = ntile(Total_Assets, Number_Quantiles))
  
  
  ######Creating a Total Equity Variable
  Bank_Data_Merged_Final_Total <- 
    mutate(.data = Bank_Data_Merged_Final_Total, Total_Equity = 
             Total_Assets - Total_Liabilities)
  
  
  #####Creating ROE_Percentage Variable, and eliminating single infinite value
  Bank_Data_Merged_Final_Total <- 
    mutate(.data = Bank_Data_Merged_Final_Total, ROE_Percentage = 
             (Net_Income / Total_Equity)*100)
  test <- vector()
  for (l in 1:length(Bank_Data_Merged_Final_Total$ROE_Percentage)) { 
    if (is.infinite(Bank_Data_Merged_Final_Total$ROE_Percentage[[l]])) { 
      test[[l]] = NaN} else { 
        test[[l]] = Bank_Data_Merged_Final_Total$ROE_Percentage[[l]]
      }
  }
  Bank_Data_Merged_Final_Total$ROE_Percentage <- test
  rm(test)
  
  
  
  #####Creating ROA_Percentage Variable 
  Bank_Data_Merged_Final_Total <- 
    mutate(.data = Bank_Data_Merged_Final_Total, ROA_Percentage = 
             (Net_Income / Total_Assets)*100)
  
  
  
  #####Adding Quadratic Asset Terms to DataFrame with Observations 
  Bank_Data_Merged_Final_Total <- 
    mutate(.data = Bank_Data_Merged_Final_Total, "Log_Total_Assets" = log(Total_Assets), 
           "Quantile_Squared" = Quantile**2, 
           "Log_Total_Assets_Squared" = (Log_Total_Assets)**2)
  

  #########Creating State Location Variable
  Bank_Data_Merged_Final_Total <- mutate(.data =  Bank_Data_Merged_Final_Total, State = `Financial Institution State`)

  #########Creating zip Code Variable
  Bank_Data_Merged_Final_Total <- mutate(.data = Bank_Data_Merged_Final_Total, Zip_Code = `Financial Institution Zip Code`)
  
  
  ####Creating a Total Equity / Total Liabilites Ratio 
  Bank_Data_Merged_Final_Total <- 
    mutate(Bank_Data_Merged_Final_Total,
           "Total_Equity_Total_Liabilities" = (Total_Assets - Total_Liabilities) / (Total_Liabilities))
  
  

##########################################################################
###############Graphical Analysis 

  
##Histogram of Percentage Securities Non-Gov all Observations, note cutting off a few observatoins that have 100 percent, almost all one small investment"
plot1 <- ggplot(data = Bank_Data_Merged_Final_Total
       , mapping = aes(Bank_Data_Merged_Final_Total$Percentage_Securities_NoGov)) + 
        geom_histogram(breaks=seq(0,100,by = 1), aes(fill=..count..)) + 
  labs(title = "% Securities 'Investment Grade' Regulated", x = "%", y = "Count") + 
    scale_x_continuous(limits = c(0, 45)) + 
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_text(size = 14), 
          panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(face = "bold"))
plot1



plot_hist_structured <- ggplot(data = Bank_Data_Merged_Final_Total
                , mapping = aes(Bank_Data_Merged_Final_Total$Percentage_Securities_Private_MBS_ABS_Structured)) + 
  geom_histogram(breaks=seq(0,100,by = 1), aes(fill=..count..)) + 
  labs(title = "% Securities Private Structured FI", x = "%", y = "Count") + 
  scale_x_continuous(limits = c(0, 20)) + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_text(size = 14), 
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(face = "bold"))

plot_hist_structured



plot_hist_corporate_foreign <- ggplot(data = Bank_Data_Merged_Final_Total
                               , mapping = aes(Bank_Data_Merged_Final_Total$Percentage_Securities_Corporate)) + 
  geom_histogram(breaks=seq(0,100,by = 1), aes(fill=..count..)) + 
  labs(title = "% Securities Corporate and Foreign FI", x = "%", y = "Count") + 
  scale_x_continuous(limits = c(0, 30)) + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_text(size = 14), 
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(face = "bold"))

plot_hist_corporate_foreign




##Histogram of Percentage Securities Non-Gov Given >0.01, don not need for paper/presentation
plot2 <- ggplot(data = Bank_Data_Merged_Final_Total[Bank_Data_Merged_Final_Total$Percentage_Securities_NoGov>0.01, ]
       , mapping = aes(Percentage_Securities_NoGov)) + 
  geom_histogram(breaks=seq(0,100,by = 1), aes(fill=..count..)) + 
  labs(title = "Percentage Securities NonGov Given>0.01") + 
  labs(x = "Percentage", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) 
plot2


##Empircal CDF of Percentage of Securiites Non-Gov, do not need for paper/presentation
plot3 <- ggplot(data = Bank_Data_Merged_Final_Total, aes(x = Bank_Data_Merged_Final_Total$Percentage_Securities_NoGov)) + 
  stat_ecdf(geom = 'step') + 
  labs(title = "Emprical CDF Percentage Securities Non-Gov") + 
  labs(x = "Percentage", y = "F(x)") + 
  theme(plot.title = element_text(hjust = 0.5)) 
plot3


##By Quarter Percentage of Securiites NonGov,Corporate, just fyi, this does exclude banks with no securities 
##Including corporte due to heavy reduction in priavte label MBS/Structured at the time
By_Quarter_Percentage_Securities_NonGov <- group_by(Bank_Data_Merged_Final_Total, Quarter) %>%
  summarise(NonGovPercentage = mean(Percentage_Securities_NoGov, na.rm = TRUE), 
            CorporatePercentage = mean(Percentage_Securities_Corporate, na.rm=TRUE), 
            Private_MBS_ABS_Structured_Percentage = mean(Percentage_Securities_Private_MBS_ABS_Structured, na.rm=TRUE), 
            MunisPercentage = mean(Percentage_Securities_Munis, na.rm = TRUE))
          
                                    
plot4 <- 
  ggplot() +
  geom_line(data = By_Quarter_Percentage_Securities_NonGov,aes(x = Quarter, y=NonGovPercentage, group=1), colour = "red") +  
  geom_line(data = By_Quarter_Percentage_Securities_NonGov,aes(x = Quarter, y=CorporatePercentage, group=1), colour = "blue") + 
  geom_line(data = By_Quarter_Percentage_Securities_NonGov,aes(x = Quarter, y = Private_MBS_ABS_Structured_Percentage, group = 1), colour = "black") + 
  labs(title = "Average % of Securities") + 
  labs(x = "Quarter", y = "%") + 
  geom_text(aes(x = which(unique(By_Quarter_Percentage_Securities_NonGov$Quarter) == "12312012") + 0.5, 
                label="Total 'Investment Grade' Regulated", y=5.2), colour="red", angle=0, vjust = 0, size=4.5) + 
  geom_text(aes(x = which(unique(By_Quarter_Percentage_Securities_NonGov$Quarter) == "12312012") + 2.6, 
                label="Corporate and Foreign FI Securities", y=2.5), colour="blue", angle=0, vjust = 0, size=4.5) + 
  geom_text(aes(x = which(unique(By_Quarter_Percentage_Securities_NonGov$Quarter) == "12312012") - 1.7, 
                label="Private Structured FI Securities", y=1), colour="black", angle=0, vjust = 0, size=4.5) + 
  geom_vline(xintercept = which(unique(By_Quarter_Percentage_Securities_NonGov$Quarter) == "12312012")) + 
  geom_text(aes(x = which(unique(By_Quarter_Percentage_Securities_NonGov$Quarter) == "12312012"), 
                label="Regulation Annoucned", y=4.2), colour="black", angle=90, vjust = -0.2, size=3.3) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_text(size = 14), 
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(face = "bold"))
  plot4




##By Quarter Percentage of Securities NonGov Weighted by Security Total Security Investments
## Also, creating weighted percentage of assets in securities, and percentage of non-gov securiteis in assets
By_Quarter_NonGov_Weighted <- group_by(Bank_Data_Merged_Final_Total, Quarter) %>%
  summarize(Weighted_NonGovPercentage = weighted.mean(x = Percentage_Securities_NoGov, w = Securities), 
            Weighted_CorporatePercentage = weighted.mean(x = Percentage_Securities_Corporate, w = Securities), 
            Weighted_Private_MBS_ABS_Structured_Percentage = weighted.mean(x = Percentage_Securities_Private_MBS_ABS_Structured, 
                                                                           w = Securities),
            Weighted_MunisPercentage = weighted.mean(x = Percentage_Securities_Munis, w = Securities), 
            Weighted_NonGovPercentage_Assets = weighted.mean(x = Percentage_Assets_Securities_NonGov, w = Total_Assets), 
            Weighted_Securities_Assets = weighted.mean(x = Percentage_Assets_Securities, w = Total_Assets))
            

plot5 <- ggplot() + 
  geom_line(data = By_Quarter_NonGov_Weighted,aes(x = Quarter, y=Weighted_NonGovPercentage),group = 1, colour = "red") +  
  geom_line(data = By_Quarter_NonGov_Weighted,aes(x = Quarter, y=Weighted_CorporatePercentage, group = 1), colour = "blue") + 
  geom_line(data = By_Quarter_NonGov_Weighted,aes(x = Quarter, y = Weighted_Private_MBS_ABS_Structured_Percentage), group = 1, colour = "black") + 
  geom_line(data = By_Quarter_NonGov_Weighted,aes(x = Quarter, y=Weighted_NonGovPercentage_Assets),group = 1, colour = "orange") +  
  geom_line(data = By_Quarter_NonGov_Weighted,aes(x = Quarter, y=Weighted_Securities_Assets, group = 1), colour = "purple") + 
  labs(title = "Total Values") + 
  labs(x = "Quarter", y = "%") + 
  geom_text(aes(x = which(unique(By_Quarter_Percentage_Securities_NonGov$Quarter) == "12312012") + 2.3, 
                label="'Investment Grade' Regulated Secuirites / Total Securities", y=29), colour="red", angle=0, vjust = 0, size=4) + 
  geom_text(aes(x = which(unique(By_Quarter_Percentage_Securities_NonGov$Quarter) == "12312012") - 3.4, 
                label="Corporate and Foreign FI / Total Securities", y=10.5), colour="blue", angle=0, vjust = 0, size = 4) + 
  geom_text(aes(x = which(unique(By_Quarter_Percentage_Securities_NonGov$Quarter) == "12312012") + 4.2, 
                label="Private Structured FI Securities / Total Securities ", y=15.7), colour="black", angle=0, vjust = 0, size=4) + 
  geom_text(aes(x = which(unique(By_Quarter_Percentage_Securities_NonGov$Quarter) == "12312012") - 0.3, 
                label="Total 'Investment Grade'   Securities / Total Assets", y=6.5), colour="orange", angle=0, vjust = 0, size=4) + 
  geom_text(aes(x = which(unique(By_Quarter_Percentage_Securities_NonGov$Quarter) == "12312012") - 0.3, 
                label="Total Securities / Total Assets ", y=21.3), colour="purple", angle=0, vjust = 0, size=4) + 
  geom_vline(xintercept = which(unique(By_Quarter_Percentage_Securities_NonGov$Quarter) == "12312012")) + 
  geom_text(aes(x = which(unique(By_Quarter_Percentage_Securities_NonGov$Quarter) == "12312012"), 
                label="Regulation Announced", y=5.9), colour="black", angle=90, vjust = -0.2, size=3) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_text(size = 14), 
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(face = "bold"))

plot5

##Summarzing Quantiles
By_Quarter_Quantiles <- 
  group_by(Bank_Data_Merged_Final_Total, Quarter, Quantile) %>%
  summarise(NonGovPercentage = mean(Percentage_Securities_NoGov, na.rm = TRUE), 
            CorporatePercentage = mean(Percentage_Securities_Corporate, na.rm=TRUE), 
            Private_MBS_ABS_Structured_Percentage = mean(Percentage_Securities_Private_MBS_ABS_Structured, na.rm=TRUE), 
            Percentage_Assets_Securities = mean(Percentage_Assets_Securities, na.rm = TRUE), 
            Percentage_Securities_Munis = mean(Percentage_Securities_Munis, na.rm=TRUE))
            
        

###Making Quantiles Factors for Graphing/Regression  
By_Quarter_Quantiles$Quantile <- 
  as.factor(By_Quarter_Quantiles$Quantile)


####Making Difference Vector Between 1st and 10th Decile for Above Variables
Difference <- list(NonGovPercentage=c(), CorporatePercentage = c(),
                   Private_MBS_ABS_Structured_Percentage = c(),
                   Percentage_Assets_Securities = c(),
                   Percentage_Securities_Munis = c())

for (i in 1:length(unique(By_Quarter_Quantiles$Quarter))) {
  Difference[[1]][[i]] <- 
  (By_Quarter_Quantiles[By_Quarter_Quantiles$Quarter == unique(By_Quarter_Quantiles$Quarter)[i] & By_Quarter_Quantiles$Quantile == Number_Quantiles, "NonGovPercentage"][[1]] - 
    By_Quarter_Quantiles[By_Quarter_Quantiles$Quarter == unique(By_Quarter_Quantiles$Quarter)[i] & By_Quarter_Quantiles$Quantile == 1, "NonGovPercentage"][[1]])
  
  Difference[[2]][[i]] <- 
  (By_Quarter_Quantiles[By_Quarter_Quantiles$Quarter == unique(By_Quarter_Quantiles$Quarter)[i] & By_Quarter_Quantiles$Quantile == Number_Quantiles, "CorporatePercentage"][[1]] - 
    By_Quarter_Quantiles[By_Quarter_Quantiles$Quarter == unique(By_Quarter_Quantiles$Quarter)[i] & By_Quarter_Quantiles$Quantile == 1, "CorporatePercentage"][[1]])
 
  Difference[[3]][[i]] <- 
   (By_Quarter_Quantiles[By_Quarter_Quantiles$Quarter == unique(By_Quarter_Quantiles$Quarter)[i] & By_Quarter_Quantiles$Quantile == Number_Quantiles, "Private_MBS_ABS_Structured_Percentage"][[1]] - 
    By_Quarter_Quantiles[By_Quarter_Quantiles$Quarter == unique(By_Quarter_Quantiles$Quarter)[i] & By_Quarter_Quantiles$Quantile == 1, "Private_MBS_ABS_Structured_Percentage"][[1]])
  
  Difference[[4]][[i]] <- 
   (By_Quarter_Quantiles[By_Quarter_Quantiles$Quarter == unique(By_Quarter_Quantiles$Quarter)[i] & By_Quarter_Quantiles$Quantile == Number_Quantiles, "Percentage_Assets_Securities"][[1]] - 
    By_Quarter_Quantiles[By_Quarter_Quantiles$Quarter == unique(By_Quarter_Quantiles$Quarter)[i] & By_Quarter_Quantiles$Quantile == 1, "Percentage_Assets_Securities"][[1]])
  
   Difference[[5]][[i]] <- 
   (By_Quarter_Quantiles[By_Quarter_Quantiles$Quarter == unique(By_Quarter_Quantiles$Quarter)[i] & By_Quarter_Quantiles$Quantile == Number_Quantiles, "Percentage_Securities_Munis"][[1]] - 
     By_Quarter_Quantiles[By_Quarter_Quantiles$Quarter == unique(By_Quarter_Quantiles$Quarter)[i] & By_Quarter_Quantiles$Quantile == 1, "Percentage_Securities_Munis"][[1]])
   }


Difference_DataFrame <- data_frame(Quarter = unique(By_Quarter_Quantiles$Quarter),NonGovPercentage=Difference[[1]],
                                   CorporatePercentage = Difference[[2]], 
                                   Private_MBS_ABS_Structured_Percentage = Difference[[3]],
                                   Percentage_Assets_Securities = Difference[[4]],
                                   Percentage_Securities_Munis = Difference[[5]])




######Graph of Average Percentage of NonGov Fi Securities by Quantile 
plot6 <- 
  ggplot(data = By_Quarter_Quantiles,  
                       mapping = aes(x = Quarter, y = NonGovPercentage, group = Quantile,
                                     colour = Quantile)) +  
  geom_line() + 
  labs(title = "Average % Securites 'Investment Grade' Regulated by Bank Size Decile") + 
  labs(x = "Quarter", y = "%") + 
  scale_colour_discrete(guide = 'none') +
  scale_x_discrete(expand=c(0, 1)) +
  geom_dl(aes(label = Quantile), method = list(dl.combine("first.points", "last.points"), cex = .5)) + 
  geom_vline(xintercept = which(unique(By_Quarter_Quantiles$Quarter) == "06302012")) + 
  geom_text(aes(x = which(unique(By_Quarter_Quantiles$Quarter) == "06302012"), 
                label="Regulation Announced", y=8), colour="black", angle=90, vjust = -0.2, size = 4) +
  theme(plot.title = element_text(hjust = 0.5, size = 9, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_text(size = 14), 
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(face = "bold"))
plot6


###############Percentage Securities Corporate, maybe will not put in there
plot7 <- 
  ggplot(data = By_Quarter_Quantiles,  
         mapping = aes(x = Quarter, y = CorporatePercentage , group = Quantile,
                       colour = Quantile)) +  
  geom_line() + 
  labs(title = "Average % Securites Corporate and Foreign FI by Bank Size Decile") + 
  labs(x = "Quarter", y = "%") + 
  scale_colour_discrete(guide = 'none') +
  scale_x_discrete(expand=c(0, 1)) +
  geom_dl(aes(label = Quantile), method = list(dl.combine("first.points", "last.points"), cex = 0.5)) + 
  geom_vline(xintercept = which(unique(By_Quarter_Quantiles$Quarter) == "06302012")) + 
  geom_text(aes(x = which(unique(By_Quarter_Quantiles$Quarter) == "06302012"), 
                label="Regulation Annoucned", y=4.5), colour="black", angle=90, vjust = -0.2, size = 4) +
  theme(plot.title = element_text(hjust = 0.5, size = 9, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_text(size = 14), 
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(face = "bold"))

plot7

######Percentage Securites Private Structured
plot8 <- 
  ggplot(data = By_Quarter_Quantiles,  
         mapping = aes(x = Quarter, y = Private_MBS_ABS_Structured_Percentage  , group = Quantile,
                       colour = Quantile)) +  
  geom_line() + 
  labs(title = "Average % Securites Private Structured FI by Bank Size Decile") + 
  labs(x = "Quarter", y = "%") + 
  scale_colour_discrete(guide = 'none') +
  scale_x_discrete(expand=c(0, 1)) +
  geom_dl(aes(label = Quantile), method = list(dl.combine("first.points", "last.points"), cex = 0.5)) + 
  geom_vline(xintercept = which(unique(By_Quarter_Quantiles$Quarter) == "06302012")) + 
  geom_text(aes(x = which(unique(By_Quarter_Quantiles$Quarter) == "06302012"), 
                label="Regulation Annoucned", y=3.7), colour="black", angle=90, vjust = -0.2, size = 4) +
  theme(plot.title = element_text(hjust = 0.5, size = 9, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_text(size = 14), 
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(face = "bold"))

plot8




######Percentage Assets in Securities by Quantiles of Total Assets, do not think I will show this graph, not currently saved 
plot9 <- 
  ggplot(data = By_Quarter_Quantiles,  
         mapping = aes(x = Quarter, y = Percentage_Assets_Securities, group = Quantile,
                       colour = Quantile)) +  
  geom_line() + 
  labs(title = "Average % Assets in Securities by Bank Size Decile") + 
  labs(x = "Quarter", y = "Percentage") + 
  scale_colour_discrete(guide = 'none') +
  scale_x_discrete(expand=c(0, 1)) +
  geom_dl(aes(label = Quantile), method = list(dl.combine("first.points", "last.points"), cex = 0.7)) + 
  geom_vline(xintercept = which(unique(By_Quarter_Quantiles$Quarter) == "06302012")) + 
  geom_text(aes(x = which(unique(By_Quarter_Quantiles$Quarter) == "06302012"), 
                label="Regulation Annoucned", y=20), colour="black", angle=90, vjust = -0.2, size = 4) +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_text(size = 14), 
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(face = "bold"))

plot9

####Differences about 10th and 1st Decile for Respective Securities, key plot 
plot10 <- 
  ggplot() +
  geom_line(data = Difference_DataFrame,aes(x = Quarter, y=NonGovPercentage),group = 1, colour = "red") +  
  geom_line(data = Difference_DataFrame,aes(x = Quarter, y = CorporatePercentage), group = 1, colour = "blue") + 
  geom_line(data = Difference_DataFrame,aes(x = Quarter, y = Private_MBS_ABS_Structured_Percentage), group = 1, colour = 'black') + 
  labs(title = "10th Minus 1st Decile of Banks' Average % of Securities") + 
  labs(x = "Quarter", y = "Percentage") + 
  geom_text(aes(x = which(unique(Difference_DataFrame$Quarter) == "12312012")+1.5, 
                label="Total 'Investment Grade' Regulated", y=6.3), colour="red", angle=0, vjust = 0, size=4.5) + 
  geom_text(aes(x = which(unique(Difference_DataFrame$Quarter) == "12312012") + 0.7, 
                label="Corporate and Foreign FI Securities", y=0.1), colour="blue", angle=0, vjust = 0, size=4.5) + 
  geom_text(aes(x = which(unique(Difference_DataFrame$Quarter) == "12312012")+2, 
                label="Private Structured FI Securities", y=4.6), colour="black", angle=0, vjust = 0, size=4.5) + 
  geom_vline(xintercept = which(unique(Difference_DataFrame$Quarter) == "06302012")) + 
  geom_text(aes(x = which(unique(Difference_DataFrame$Quarter) == "06302012"), 
                label="Regulation Announced", y=2.8), colour="black", angle=90, vjust = -0.2, size=3) +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_text(size = 14), 
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(face = "bold"))

  plot10

  
##Choosing Cutoff of Assets less than $175 million due to statement in regulatory documents
##Need to add other variables 
By_Quarter_SmallBanks <- 
  filter(.data = Bank_Data_Merged_Final_Total, Total_Assets < 175000) %>%
  group_by(Quarter) %>%
  summarise(NonGovPercentage = mean(Percentage_Securities_NoGov, na.rm = TRUE), 
                       CorporatePercentage = mean(Percentage_Securities_Corporate, na.rm=TRUE), 
                       Private_MBS_ABS_Structured_Percentage = mean(Percentage_Securities_Private_MBS_ABS_Structured, na.rm=TRUE), 
                       Percentage_Assets_Securities = mean(Percentage_Assets_Securities, na.rm = TRUE), 
                       Percentage_Securities_Munis = mean(Percentage_Securities_Munis, na.rm=TRUE))

plot11 <- 
  ggplot() +
  geom_line(data = By_Quarter_SmallBanks,aes(x = Quarter, y=NonGovPercentage),group = 1, colour = "red") +  
  geom_line(data = By_Quarter_SmallBanks,aes(x = Quarter, y = CorporatePercentage), group = 1, colour = "blue") + 
  geom_line(data = By_Quarter_SmallBanks,aes(x = Quarter, y = Private_MBS_ABS_Structured_Percentage), group = 1, colour = 'black') + 
  labs(title = "Average % of Securities Investment of Banks with Assets < $175 million") + 
  labs(x = "Quarter", y = "Percentage") + 
  geom_text(aes(x = which(unique(By_Quarter_SmallBanks$Quarter) == "12312012"), 
                label="Total 'Investment Grade' Type", y=4), colour="red", angle=0, vjust = 0, size=4) + 
  geom_text(aes(x = which(unique(By_Quarter_SmallBanks$Quarter) == "12312012"), 
                label="Corporate and Foriegn FI Securities", y=3), colour="blue", angle=0, vjust = 0, size=4) + 
  geom_text(aes(x = which(unique(By_Quarter_SmallBanks$Quarter) == "12312012"), 
                label="Private Structured FI Securities", y=0.6), colour="black", angle=0, vjust = 0, size=4) + 
  geom_vline(xintercept = which(unique(By_Quarter_SmallBanks$Quarter) == "06302012")) + 
  geom_text(aes(x = which(unique(By_Quarter_SmallBanks$Quarter) == "06302012"), 
                label="Regulation Announced", y=1.8), colour="black", angle=90, vjust = -0.2, size=3) +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_text(size = 14), 
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(face = "bold"))

plot11


########Looking at banks with assets of less than 1 billion - Arbitrary definition of community banks
#######over 90% of the banks in the sample meet this definition
By_Quarter_CommunityBanks <- 
  filter(.data = Bank_Data_Merged_Final_Total, Total_Assets < 1000000) %>%
  group_by(Quarter) %>%
  summarise(NonGovPercentage = mean(Percentage_Securities_NoGov, na.rm = TRUE), 
            CorporatePercentage = mean(Percentage_Securities_Corporate, na.rm=TRUE), 
            Private_MBS_ABS_Structured_Percentage = mean(Percentage_Securities_Private_MBS_ABS_Structured, na.rm=TRUE), 
            Percentage_Assets_Securities = mean(Percentage_Assets_Securities, na.rm = TRUE), 
            Percentage_Securities_Munis = mean(Percentage_Securities_Munis, na.rm=TRUE))


plot12 <- 
  ggplot() +
  geom_line(data = By_Quarter_CommunityBanks,aes(x = Quarter, y=NonGovPercentage),group = 1, colour = "red") +  
  geom_line(data = By_Quarter_CommunityBanks,aes(x = Quarter, y = CorporatePercentage), group = 1, colour = "blue") + 
  geom_line(data = By_Quarter_CommunityBanks,aes(x = Quarter, y = Private_MBS_ABS_Structured_Percentage), group = 1, colour = 'black') + 
  labs(title = "Average % of Securities Investment of Banks with Assets < $1 billion") + 
  labs(x = "Quarter", y = "Percentage") + 
  geom_text(aes(x = which(unique(By_Quarter_CommunityBanks$Quarter) == "12312012"), 
                label="Total 'Investment Grade' Regulated", y=4.6), colour="red", angle=0, vjust = 0, size=4) + 
  geom_text(aes(x = which(unique(By_Quarter_CommunityBanks$Quarter) == "12312012"), 
                label="Corporate and Foreign FI Securities", y=3.3), colour="blue", angle=0, vjust = 0, size=4) + 
  geom_text(aes(x = which(unique(By_Quarter_CommunityBanks$Quarter) == "12312012"), 
                label="Private Structured FI Securities", y=0.7), colour="black", angle=0, vjust = 0, size=4) + 
  geom_vline(xintercept = which(unique(By_Quarter_CommunityBanks$Quarter) == "06302012")) + 
  geom_text(aes(x = which(unique(By_Quarter_CommunityBanks$Quarter) == "06302012"), 
                label="Regulation Announced", y=2.1), colour="black", angle=90, vjust = -0.2, size=3.5) +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_text(size = 14), 
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(face = "bold"))
plot12



##Examing big banks how their percentage has changed - they seem to matter for these markets given how little small banks even invest 
##further avenue of research, break down corporate and private label mbs - corporate seems more important due to general decline any 
##way of the other market - down by almost 95% according to community bank report
###Also, 50 billion was threshold for stress testing, recent law passed upping SIFI to $250 billion 
By_Quarter_LargeBanks <- 
  filter(.data = Bank_Data_Merged_Final_Total, Total_Assets > 50000000) %>%
  group_by(Quarter) %>%
  summarise(NonGovPercentage = mean(Percentage_Securities_NoGov, na.rm = TRUE), 
            CorporatePercentage = mean(Percentage_Securities_Corporate, na.rm=TRUE), 
            Private_MBS_ABS_Structured_Percentage = mean(Percentage_Securities_Private_MBS_ABS_Structured, na.rm=TRUE), 
            Percentage_Assets_Securities = mean(Percentage_Assets_Securities, na.rm = TRUE), 
            Percentage_Securities_Munis = mean(Percentage_Securities_Munis, na.rm=TRUE))

plot13 <- 
  ggplot() +
  geom_line(data = By_Quarter_LargeBanks,aes(x = Quarter, y=NonGovPercentage),group = 1, colour = "red") +  
  geom_line(data = By_Quarter_LargeBanks,aes(x = Quarter, y = CorporatePercentage), group = 1, colour = "blue") + 
  geom_line(data = By_Quarter_LargeBanks,aes(x = Quarter, y = Private_MBS_ABS_Structured_Percentage), group = 1, colour = 'black') + 
  labs(title = "Average % of Securities Investment of Banks with Assets > $50 Billion") + 
  labs(x = "Quarter", y = "Percentage") + 
  geom_text(aes(x = which(unique(By_Quarter_LargeBanks$Quarter) == "12312012"), 
                label="Total 'Investment Grade' Regulated", y=29), colour="red", angle=0, vjust = 0, size=4) + 
  geom_text(aes(x = which(unique(By_Quarter_LargeBanks) == "12312012"), 
                label="Corporate and Foreign FI Securities", y=4), colour="blue", angle=0, vjust = 0, size=4) + 
  geom_text(aes(x = which(unique(By_Quarter_LargeBanks$Quarter) == "12312012"), 
                label="Private Structured FI Securities", y=13), colour="black", angle=0, vjust = 0, size=4) + 
  geom_vline(xintercept = which(unique(By_Quarter_LargeBanks$Quarter) == "06302012")) + 
  geom_text(aes(x = which(unique(By_Quarter_LargeBanks$Quarter) == "06302012"), 
                label="Regulation Announced", y=20), colour="black", angle=90, vjust = -0.2, size=3) +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_text(size = 14), 
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(face = "bold"))

plot13


###########Analysis of Munis as Percentage of Secirites, do not know what I am doing with this yet
plot14 <-
  ggplot(data = By_Quarter_Quantiles,
         mapping = aes(x = Quarter, y = Percentage_Securities_Munis, group = Quantile,
                       colour = Quantile)) +
  geom_line() +
  labs(title = "Average % of Securities in Munis by Quantile of Total Assets") +
  labs(x = "Quarter", y = "Percentage") +
  scale_colour_discrete(guide = 'none') +
  scale_x_discrete(expand=c(0, 1)) +
  geom_dl(aes(label = Quantile), method = list(dl.combine("first.points", "last.points"), cex = 0.8)) +
  geom_vline(xintercept = which(unique(By_Quarter_Quantiles$Quarter) == "06302012")) +
  geom_text(aes(x = which(unique(By_Quarter_Quantiles$Quarter) == "06302012"),
                label="Regulation Annoucned", y=18), colour="black", angle=90, vjust = -0.2, size = 3) +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_text(size = 14), 
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(face = "bold"))

  plot14



  
  ######################################
  ####Regression Details
  ###Converting DataFrame to a Panel DataSet with Variables I need
 Bank_Data_Merged_Final_Total <- 
    Bank_Data_Merged_Final_Total %>% 
    mutate(Intercept = 1)
  
  Bank_Data_Merged_Final_Total_Panel <- 
  select(Bank_Data_Merged_Final_Total, Intercept, IDRSSD_, Quarter, Percentage_Securities_Munis, Percentage_Securities_Corporate, 
         Percentage_Securities_NoGov, Percentage_Securities_Private_MBS_ABS_Structured, Total_Assets, Total_Liabilities, Total_Equity, 
         Percentage_Assets_Securities, Log_Total_Assets, Log_Total_Assets_Squared, Quantile, Quantile_Squared, 
         After06302012_Dummy, State, Efficiency_Ratio_Percentage, Zip_Code, ROE_Percentage, ROA_Percentage, 
         Tier1_Leverage_Capital_Ratio_Percentage, Total_RiskBased_Capital_Ratio_Percentage, Percentage_Assets_Loans)

  
  ###Due to issues of Quarterl Data, I am simply converting Quarter to 1 through 41
  translation <- vector(length = length(dates_vector))
  for (i in 1:length(dates_vector)) {
    names(translation)[i] <- levels(Bank_Data_Merged_Final_Total_Panel$Quarter)[i]
    translation[i] <- i
    }
  Bank_Data_Merged_Final_Total_Panel$Quarter_Numeric <- translation[Bank_Data_Merged_Final_Total_Panel$Quarter]
  
  

  
  #####Adding average variables for correlated random effects model
  Bank_Data_Merged_Final_Total_Panel <- 
    group_by(Bank_Data_Merged_Final_Total_Panel, IDRSSD_) %>% 
    mutate(Log_Total_Assets_Bar = mean(Log_Total_Assets, na.rm = TRUE), 
           Log_Total_Assets_Squared_Bar = mean(Log_Total_Assets_Squared, na.rm=TRUE), 
           Tier1_Leverage_Capital_Ratio_Percentage_Bar = mean(Tier1_Leverage_Capital_Ratio_Percentage, na.rm=TRUE), 
           Total_RiskBased_Capital_Ratio_Percentage_Bar = mean(Total_RiskBased_Capital_Ratio_Percentage, na.rm=TRUE), 
           ROE_Percentage_Bar = mean(ROE_Percentage, na.rm=TRUE), 
           ROA_Percentage_Bar = mean(ROA_Percentage, na.rm=TRUE), 
           Efficiency_Ratio_Percentage_Bar = mean(Efficiency_Ratio_Percentage, na.rm=TRUE))
  
  
  

  #####Making Data Panel Data Formally for R Regressions 
  Bank_Data_Merged_Final_Total_Panel <- pdata.frame(Bank_Data_Merged_Final_Total_Panel, index = c("IDRSSD_", "Quarter_Numeric"))
  
  ##Adding NA values to the 3 banks that don't have consequetive quarters 
  which(!is.pconsecutive(Bank_Data_Merged_Final_Total_Panel))
  Bank_Data_Merged_Final_Total_Panel <- make.pconsecutive(Bank_Data_Merged_Final_Total_Panel)
  
  
  ######Only considering variables where I have non-missing values for every used variable 
  Bank_Data_Merged_Final_Total_Panel <- 
  Bank_Data_Merged_Final_Total_Panel %>%
    filter(!is.na(Percentage_Securities_NoGov),!is.na(Log_Total_Assets), !is.na(Log_Total_Assets_Bar), 
           !is.na(Log_Total_Assets_Squared), !is.na(Log_Total_Assets_Squared_Bar), 
           !is.na(Tier1_Leverage_Capital_Ratio_Percentage), !is.na(Tier1_Leverage_Capital_Ratio_Percentage_Bar),
           !is.na(Total_RiskBased_Capital_Ratio_Percentage),  !is.na(Total_RiskBased_Capital_Ratio_Percentage_Bar), 
           !is.na(ROE_Percentage), !is.na(ROE_Percentage_Bar), !is.na(ROA_Percentage), !is.na(ROA_Percentage_Bar), 
           !is.na(Efficiency_Ratio_Percentage), !is.na(Efficiency_Ratio_Percentage_Bar), !is.na(After06302012_Dummy))
  Bank_Data_Merged_Final_Total_Panel <- pdata.frame(Bank_Data_Merged_Final_Total_Panel, index = c("IDRSSD_", "Quarter_Numeric"))
  
  
  ###############Saving two important dataframes exteranlly
  save(Bank_Data_Merged_Final_Total, Bank_Data_Merged_Final_Total_Panel, file = "mydata.rda")
  load("mydata.rda")
  
  ########Summary Statistics, consider putting in _bar values to take up space on the page 
  stargazer(as.data.frame(Bank_Data_Merged_Final_Total_Panel), font.size = "small", summary.stat = c("mean", "sd", "p25", "median", "p75", "max")
            ,notes = "Notes test", label = "Test Label", notes.align = "l")
  
  nrow(Bank_Data_Merged_Final_Total_Panel)
  ########################################
  ###Tobti Random Effects Models, quote package creators 
  
  #####Correlated Random Effects Model with States Test
  State_Vector <- c(rep(0, 55))
  summary(panelResult1_Correlated_state <- censReg(Percentage_Securities_NoGov ~ State + Log_Total_Assets_Bar + Log_Total_Assets_Squared_Bar + 
                                               Tier1_Leverage_Capital_Ratio_Percentage_Bar + Total_RiskBased_Capital_Ratio_Percentage_Bar + 
                                               ROE_Percentage_Bar + ROA_Percentage_Bar + Efficiency_Ratio_Percentage_Bar + 
                                               Log_Total_Assets + Log_Total_Assets_Squared +
                                               Tier1_Leverage_Capital_Ratio_Percentage + Total_RiskBased_Capital_Ratio_Percentage + 
                                               ROE_Percentage + ROA_Percentage + Efficiency_Ratio_Percentage  + After06302012_Dummy + 
                                               Log_Total_Assets*After06302012_Dummy +Log_Total_Assets_Squared*After06302012_Dummy + 
                                               Tier1_Leverage_Capital_Ratio_Percentage*After06302012_Dummy + 
                                               Total_RiskBased_Capital_Ratio_Percentage*After06302012_Dummy + ROE_Percentage*After06302012_Dummy +
                                               ROA_Percentage*After06302012_Dummy + Efficiency_Ratio_Percentage*After06302012_Dummy, 
                                             left = 0, right = 100, tol = 10e-12, data = Bank_Data_Merged_Final_Total_Panel, method = "BHHH" , 
                                             nGHQ = 64, iterlim = 1000, start = c(panelResult1$estimate[[1]], State_Vector, Log_Total_Assets_Bar = 0, Log_Total_Assets_Squared_Bar = 0, 
                                                                                  Tier1_Leverage_Capital_Ratio_Bar = 0, Total_RiskBased_Capital_Ratio_Percentage_Bar = 0, ROE_Percentage_Bar = 0, 
                                                                                  ROA_Percentage_Bar = 0, Efficiency_Ratio_Percentage_Bar = 0, panelResult1$estimate[-1])))
  save(panelResult1_Correlated_state, file = "panelResult1_Correlated_state.rda") 
  
  panelResult1_Correlated_state$start
  
  
  ##Percentage Securities NoGov, ran this model once before re=inputing "start" argument
  summary(panelResult1 <- censReg(Percentage_Securities_NoGov ~ Log_Total_Assets + Log_Total_Assets_Squared + 
                                    Tier1_Leverage_Capital_Ratio_Percentage + Total_RiskBased_Capital_Ratio_Percentage + ROE_Percentage + 
                                  ROA_Percentage + Efficiency_Ratio_Percentage  + After06302012_Dummy + Log_Total_Assets*After06302012_Dummy + 
                                  Log_Total_Assets_Squared*After06302012_Dummy + Tier1_Leverage_Capital_Ratio_Percentage*After06302012_Dummy + 
                                  Total_RiskBased_Capital_Ratio_Percentage*After06302012_Dummy + ROE_Percentage*After06302012_Dummy +
                                  ROA_Percentage*After06302012_Dummy + Efficiency_Ratio_Percentage*After06302012_Dummy, left = 0, right = 100, 
                                    data = Bank_Data_Merged_Final_Total_Panel, method = "BHHH" , nGHQ = 64, iterlim = 10000, start = panelResult1$estimate, 
                                  tol = 10e-12))
  save(panelResult1, file = "panelResult1.rda")
  
  
  
  ########Testing pooled just in case
  summary(panelResult1_Pooled <- censReg(Percentage_Securities_NoGov ~ Log_Total_Assets + Log_Total_Assets_Squared + 
                                           Tier1_Leverage_Capital_Ratio_Percentage + Total_RiskBased_Capital_Ratio_Percentage + ROE_Percentage + 
                                           ROA_Percentage + Efficiency_Ratio_Percentage  + After06302012_Dummy + Log_Total_Assets*After06302012_Dummy + 
                                           Log_Total_Assets_Squared*After06302012_Dummy + Tier1_Leverage_Capital_Ratio_Percentage*After06302012_Dummy + 
                                           Total_RiskBased_Capital_Ratio_Percentage*After06302012_Dummy + ROE_Percentage*After06302012_Dummy +
                                           ROA_Percentage*After06302012_Dummy + Efficiency_Ratio_Percentage*After06302012_Dummy, left = 0, right = 100, 
                                         data = Bank_Data_Merged_Final_Total))
  save(panelResult1_Pooled, file = "panelResult1_Pooled.rda")
  
  
  ###Manualy completing likihood ratio test of null that pooled, i.e, no panel level effects, reject the null in this case 
  pchisq(-2*(logLik(panelResult1_Pooled)[[1]]-logLik(panelResult1)[[1]]),1, lower.tail = FALSE)
  
  
  #####Correlated Random Effects Model, next time I run, do correct start paramaters
  summary(panelResult1_Correlated <- censReg(Percentage_Securities_NoGov ~ Log_Total_Assets_Bar + Log_Total_Assets_Squared_Bar + 
                                               Tier1_Leverage_Capital_Ratio_Percentage_Bar + Total_RiskBased_Capital_Ratio_Percentage_Bar + 
                                               ROE_Percentage_Bar + ROA_Percentage_Bar + Efficiency_Ratio_Percentage_Bar + 
                                               Log_Total_Assets + Log_Total_Assets_Squared +
                                               Tier1_Leverage_Capital_Ratio_Percentage + Total_RiskBased_Capital_Ratio_Percentage + 
                                               ROE_Percentage + ROA_Percentage + Efficiency_Ratio_Percentage  + After06302012_Dummy + 
                                               Log_Total_Assets*After06302012_Dummy +Log_Total_Assets_Squared*After06302012_Dummy + 
                                               Tier1_Leverage_Capital_Ratio_Percentage*After06302012_Dummy + 
                                               Total_RiskBased_Capital_Ratio_Percentage*After06302012_Dummy + ROE_Percentage*After06302012_Dummy +
                                               ROA_Percentage*After06302012_Dummy + Efficiency_Ratio_Percentage*After06302012_Dummy, 
                                             left = 0, right = 100, tol = 10e-12, data = Bank_Data_Merged_Final_Total_Panel, method = "BHHH" , 
                                             nGHQ = 64, iterlim = 1000, start = c(panelResult1$estimate[[1]], Log_Total_Assets_Bar = 0, Log_Total_Assets_Squared_Bar = 0, 
                                                       Tier1_Leverage_Capital_Ratio_Bar = 0, Total_RiskBased_Capital_Ratio_Percentage_Bar = 0, ROE_Percentage_Bar = 0, 
                                                       ROA_Percentage_Bar = 0, Efficiency_Ratio_Percentage_Bar = 0, panelResult1$estimate[-1])))
  save(panelResult1_Correlated, file = "panelResult1_Correlated.rda") 
  
  
  
  
  ###Testing if average coefficients are signifigant which would indicate correlated random effects model, reject random effects where 
  ## indivdual distiburance is not correalted witht the regressors
  pchisq(-2*(logLik(panelResult1)[[1]]-logLik(panelResult1_Correlated)[[1]]), df = 25-17, lower.tail = FALSE)
     
  
  
  ###Manually
  stargazer(panelResult1_Pooled, panelResult1, panelResult1_Correlated, font.size = "tiny", align = TRUE, title = "Results", 
            omit.stat = c("aic", "bic"), dep.var.labels = c("Percentag of Bank Securities Newly Regulated"), 
            intercept.bottom = FALSE, notes = "Standard Erros are from the blank matrix of Maximum Liklihood", notes.align = "l")
  
  
  
  
  ####Need to compute one example of average partial effects for the correlated random tobit model 
  
  ###First, computing partial effects of ln(Total_Assets) at the average of all covariates when dummy = 1, when dummy =0, then taking the differnce 
  ###computing means for each covariates
   averages <- c("Log_Total_Assets_Bar" = mean(Bank_Data_Merged_Final_Total_Panel$Log_Total_Assets_Bar), 
                "Log_Total_Assets_Squared_Bar" = mean(Bank_Data_Merged_Final_Total_Panel$Log_Total_Assets_Squared_Bar),
                "Tier1_Leverage_Capital_Ratio_Percentage_Bar" = mean(Bank_Data_Merged_Final_Total_Panel$Tier1_Leverage_Capital_Ratio_Percentage_Bar), 
                "Total_RiskBased_Capital_Ratio_Percentage_Bar" = mean(Bank_Data_Merged_Final_Total_Panel$Total_RiskBased_Capital_Ratio_Percentage_Bar), 
                "ROE_Percentage_Bar" = mean(Bank_Data_Merged_Final_Total_Panel$ROE_Percentage_Bar), 
                "ROA_Percentage_Bar" = mean(Bank_Data_Merged_Final_Total_Panel$ROA_Percentage_Bar), 
                "Efficiency_Ratio_Percentage_Bar" = mean(Bank_Data_Merged_Final_Total_Panel$Efficiency_Ratio_Percentage_Bar), 
                "Tier1_Leverage_Capital_Ratio_Percentage" = mean(Bank_Data_Merged_Final_Total_Panel$Tier1_Leverage_Capital_Ratio_Percentage), 
                "Total_RiskBased_Capital_Ratio_Percentage" = mean(Bank_Data_Merged_Final_Total_Panel$Total_RiskBased_Capital_Ratio_Percentage), 
                "ROE_Percentage" = mean(Bank_Data_Merged_Final_Total_Panel$ROE_Percentage), 
                "ROA_Percentage" = mean(Bank_Data_Merged_Final_Total_Panel$ROA_Percentage), 
                "Efficiency_Ratio_Percentage" = mean(Bank_Data_Merged_Final_Total_Panel$Efficiency_Ratio_Percentage))
  
  beta_hat <- panelResult1_Correlated$estimate[1:23]
  ###Sigma hat of the random error term for each observation, NOT the individual component variance, labels are different in paper than the regression output
  ###Because r package lables them differently, in R, mu is individula component, and nu is random component, in paper, a is individual component and 
  ### u is random component 
  sigma_hat_nu <- sqrt(exp(panelResult1_Correlated$estimate[[25]])^2)
  total_assets <- c(seq(from = quantile(Bank_Data_Merged_Final_Total$Total_Assets, probs = (.01)), to = 1000000, by = 20000))
  partial_effects_results <- vector(length = length(total_assets))
  Increase_Assets <- 2
  
 length(unique(Bank_Data_Merged_Final_Total_Panel$IDRSSD_))
  
  ####################Computing partial effects of regulation for doubling of bank assets
  
  for (i in 1:length(total_assets)) {
  
  ####First Partial Effect evaluated when dummy = 1, 
  dummy_1_covariate_vector <- c(1, averages[1:7], log(total_assets[i]), log(total_assets[i])^2, averages[8:12], 1,  
                                log(total_assets[i]), log(total_assets[i])^2, averages[8:12])
  
  Dummy1_PartialEffect <- ( (coef(panelResult1_Correlated)[9] + 2*coef(panelResult1_Correlated)[10]*log(total_assets[i]) + 
      coef(panelResult1_Correlated)[17] + 2*coef(panelResult1_Correlated)[18]*log(total_assets[i]))*
    ( pnorm((100 - dummy_1_covariate_vector %*% beta_hat) / (sigma_hat_nu)) - pnorm((0 - dummy_1_covariate_vector %*% beta_hat) / (sigma_hat_nu))) ) *log(Increase_Assets)
  
    
  ###Second partial effect evalulated when dummy = 0, 
  dummy_0_covariate_vector <- c(1, averages[1:7], log(total_assets[i]), log(total_assets[i])^2, averages[8:12], rep(0,8))  
  
  Dummy0_PartialEffect <- ( (coef(panelResult1_Correlated)[9] + 2*coef(panelResult1_Correlated)[10]*log(total_assets[i]))*
    ( pnorm((100 - dummy_0_covariate_vector %*% beta_hat) / (sigma_hat_nu)) - pnorm((0 - dummy_0_covariate_vector %*% beta_hat) / (sigma_hat_nu))) ) *log(Increase_Assets)
  
  partial_effects_results[i] <- Dummy1_PartialEffect - Dummy0_PartialEffect
  }
  
  
  max(partial_effects_results)
  min(partial_effects_results)
  
  
  
  ###########Computing Average Partial Effects for all community bank observations with less than 1 billion in assets 
  
  ###Sigma hat of the random error term + the individual error component, see wooldrige page 710 bottom
  sigma_hat <- sqrt(exp(panelResult1_Correlated$estimate[[24]])^2 + exp(panelResult1_Correlated$estimate[[25]])^2)
  
  ##For every observation, need a predicted value of expcted percentage securities non-gov, for post and pre regulation, assets less than 1 billion 
  #Pre-Regulation Period 
 Bank_Data_Merged_Final_Total_Panel_Pre <- 
   Bank_Data_Merged_Final_Total_Panel %>% 
    filter(After06302012_Dummy == 0, Total_Assets < 1000000) %>% 
   select(Intercept, Log_Total_Assets_Bar, Log_Total_Assets_Squared_Bar, Tier1_Leverage_Capital_Ratio_Percentage_Bar, 
          Total_RiskBased_Capital_Ratio_Percentage_Bar, ROE_Percentage_Bar, ROA_Percentage_Bar, 
          Efficiency_Ratio_Percentage_Bar, Log_Total_Assets, Log_Total_Assets_Squared,
          Tier1_Leverage_Capital_Ratio_Percentage, Total_RiskBased_Capital_Ratio_Percentage,  
          ROE_Percentage, ROA_Percentage, Efficiency_Ratio_Percentage, After06302012_Dummy) 
   
  percentage_securities_nongov_predicted_pre <- as.matrix(Bank_Data_Merged_Final_Total_Panel_Pre) %*% beta_hat[1:16]

  
  
  ###Post Regulation Period 
  Bank_Data_Merged_Final_Total_Panel_Post <- 
    Bank_Data_Merged_Final_Total_Panel %>% 
    filter(After06302012_Dummy == 1, Total_Assets < 1000000) %>% 
    select(Intercept, Log_Total_Assets_Bar, Log_Total_Assets_Squared_Bar, Tier1_Leverage_Capital_Ratio_Percentage_Bar, 
           Total_RiskBased_Capital_Ratio_Percentage_Bar, ROE_Percentage_Bar, ROA_Percentage_Bar, 
           Efficiency_Ratio_Percentage_Bar, Log_Total_Assets, Log_Total_Assets_Squared,
           Tier1_Leverage_Capital_Ratio_Percentage, Total_RiskBased_Capital_Ratio_Percentage,  
           ROE_Percentage, ROA_Percentage, Efficiency_Ratio_Percentage, After06302012_Dummy) %>% 
    mutate(Log_Total_Assets_After06302012_Dummy = Log_Total_Assets*After06302012_Dummy, 
           Log_Total_Assets_Squared_After06302012_Dummy = Log_Total_Assets_Squared*After06302012_Dummy, 
           Tier1_Leverage_Capital_Ratio_Percentage_After06302012_Dummy = Tier1_Leverage_Capital_Ratio_Percentage*After06302012_Dummy,  
           Total_RiskBased_Capital_Ratio_Percentage_After06302012_Dummy = Total_RiskBased_Capital_Ratio_Percentage*After06302012_Dummy,  
           ROE_Percentage_After06302012_Dummy = ROE_Percentage*After06302012_Dummy, 
           ROA_Percentage_After06302012_Dummy = ROA_Percentage*After06302012_Dummy, 
           Efficiency_Ratio_Percentage_After06302012_Dummy = Efficiency_Ratio_Percentage*After06302012_Dummy)
  
  percentage_securities_nongov_predicted_post <- as.matrix(Bank_Data_Merged_Final_Total_Panel_Post) %*% beta_hat
  
  ###Using combination of page 705 Wooldrige equation 17.68 along with Wooldridge CRE Tobit slide 67 and Wooldridge page 710, computing 
  ###average partial effects for banks with less than asset of 1 billion between post and pre
  Dummy1_APE <- mean((coef(panelResult1_Correlated)[9] + 2*coef(panelResult1_Correlated)[10]*(as.matrix(Bank_Data_Merged_Final_Total_Panel_Post$Log_Total_Assets)) + 
                   coef(panelResult1_Correlated)[17] + 2*coef(panelResult1_Correlated)[18]*(as.matrix(Bank_Data_Merged_Final_Total_Panel_Post$Log_Total_Assets))) * 
    ( pnorm((100 - percentage_securities_nongov_predicted_post) / (sigma_hat)) - pnorm((0 - percentage_securities_nongov_predicted_post) / (sigma_hat)))) * 
    log(Increase_Assets)
  
  
  Dummy0_APE <-  mean((coef(panelResult1_Correlated)[9] + 2*coef(panelResult1_Correlated)[10]*(as.matrix(Bank_Data_Merged_Final_Total_Panel_Pre$Log_Total_Assets))) * 
                        ( pnorm((100 - percentage_securities_nongov_predicted_pre) / (sigma_hat)) - pnorm((0 - percentage_securities_nongov_predicted_pre) / (sigma_hat)))) * 
                         log(Increase_Assets)
  
  
  Average_Partial_Effects <- Dummy1_APE - Dummy0_APE
  
  
  
  summary(pnorm((100 - percentage_securities_nongov_predicted_pre) / (sigma_hat)) - pnorm((0 - percentage_securities_nongov_predicted_pre) / (sigma_hat)))
  
  ###########Testing a Model with State ID Variables, this will take a long time to identify I am guessing?
  ##Do I even need to do this considering the explanation in my paper about locality not mattering?
  ### Rand the model, all the t-values are zero, identification looks like a huge issue
  # summary(panelResult1_Correlated_State <- censReg(Percentage_Securities_NoGov ~ Log_Total_Assets_Bar + Log_Total_Assets_Squared_Bar + 
  #                                              Tier1_Leverage_Capital_Ratio_Percentage_Bar + Total_RiskBased_Capital_Ratio_Percentage_Bar + 
  #                                              ROE_Percentage_Bar + ROA_Percentage_Bar + Efficiency_Ratio_Percentage_Bar + State + 
  #                                              Log_Total_Assets + Log_Total_Assets_Squared +
  #                                              Tier1_Leverage_Capital_Ratio_Percentage + Total_RiskBased_Capital_Ratio_Percentage + 
  #                                              ROE_Percentage + ROA_Percentage + Efficiency_Ratio_Percentage  + After06302012_Dummy + 
  #                                              Log_Total_Assets*After06302012_Dummy +Log_Total_Assets_Squared*After06302012_Dummy + 
  #                                              Tier1_Leverage_Capital_Ratio_Percentage*After06302012_Dummy + 
  #                                              Total_RiskBased_Capital_Ratio_Percentage*After06302012_Dummy + ROE_Percentage*After06302012_Dummy +
  #                                              ROA_Percentage*After06302012_Dummy + Efficiency_Ratio_Percentage*After06302012_Dummy, 
  #                                            left = 0, right = 100, tol = 10e-12, data = Bank_Data_Merged_Final_Total_Panel, method = "BHHH" , 
  #                                            nGHQ = 64, iterlim = 1000, start = c(panelResult1$estimate[[1]], Log_Total_Assets_Bar = 0, Log_Total_Assets_Squared_Bar = 0, 
  #                                                                                 Tier1_Leverage_Capital_Ratio_Bar = 0, Total_RiskBased_Capital_Ratio_Percentage_Bar = 0, ROE_Percentage_Bar = 0, 
  #                                                                                 ROA_Percentage_Bar = 0, Efficiency_Ratio_Percentage_Bar = 0, State = rep(0,55),
  #                                                                                 panelResult1$estimate[-1])))
  # 
  # save(panelResult1_Correlated_State, file = "panelResult1_Correlated_State.rda") 
  # 
  
 
  ########Quantile, not sure if I going to use these for now, maybe as robustness tests in appendix run them? Haven't run them yet 
  # summary(panelResult_Quantile1 <- censReg(Percentage_Securities_NoGov ~ Quantile + Quantile_Squared + 
  #                                          Tier1_Leverage_Capital_Ratio_Percentage + Total_RiskBased_Capital_Ratio_Percentage + ROE_Percentage + 
  #                                          ROA_Percentage + Efficiency_Ratio_Percentage  + After06302012_Dummy + Quantile*After06302012_Dummy + 
  #                                          Quantile_Squared*After06302012_Dummy + Tier1_Leverage_Capital_Ratio_Percentage*After06302012_Dummy + 
  #                                          Total_RiskBased_Capital_Ratio_Percentage*After06302012_Dummy + ROE_Percentage*After06302012_Dummy + 
  #                                          ROA_Percentage*After06302012_Dummy + Efficiency_Ratio_Percentage*After06302012_Dummy, left = 0, 
  #                                        data = Bank_Data_Merged_Final_Total_Panel, method = "BHHH" , nGHQ = 16, iterlim = 5000))
  # 
  # summary(panelResult_Quantile1_LessControls <- censReg(Percentage_Securities_NoGov ~ Quantile + Quantile_Squared + 
  #                                                       Tier1_Leverage_Capital_Ratio_Percentage + ROE_Percentage + ROA_Percentage +
  #                                                     Efficiency_Ratio_Percentage  + After06302012_Dummy + Quantile*After06302012_Dummy + 
  #                                                      Quantile_Squared*After06302012_Dummy + Tier1_Leverage_Capital_Ratio_Percentage*After06302012_Dummy + 
  #                                                       ROE_Percentage*After06302012_Dummy + ROA_Percentage*After06302012_Dummy + 
  #                                                       Efficiency_Ratio_Percentage*After06302012_Dummy, left = 0,
  #                                                     data = Bank_Data_Merged_Final_Total_Panel, method = "BHHH", nGHQ = 16, iterlim = 5000))
  # Bank_Data_Merged_Final_Total_Panel$Percentage_Securities_Private_MBS_ABS_Structured
  # 

  
  ############Tobit Models for Private Label MBS/ABS, ran this model once before re=inputing "start" argument
  summary(panelResult1_ABS <- censReg(Percentage_Securities_Private_MBS_ABS_Structured ~ Log_Total_Assets + Log_Total_Assets_Squared + 
                                    Tier1_Leverage_Capital_Ratio_Percentage + Total_RiskBased_Capital_Ratio_Percentage + ROE_Percentage + 
                                    ROA_Percentage + Efficiency_Ratio_Percentage  + After06302012_Dummy + Log_Total_Assets*After06302012_Dummy + 
                                    Log_Total_Assets_Squared*After06302012_Dummy + Tier1_Leverage_Capital_Ratio_Percentage*After06302012_Dummy + 
                                    Total_RiskBased_Capital_Ratio_Percentage*After06302012_Dummy + ROE_Percentage*After06302012_Dummy +
                                    ROA_Percentage*After06302012_Dummy + Efficiency_Ratio_Percentage*After06302012_Dummy, left = 0, right = 100, 
                                  data = Bank_Data_Merged_Final_Total_Panel, method = "BHHH" , nGHQ = 64, iterlim = 10000, tol = 10e-12, 
                                  start = panelResult1_ABS$estimate))
  save(panelResult1_ABS, file = "panelResult1_ABS.rda")
  
  
  ########Testing pooled just in case
  summary(panelResult1_ABS_Pooled <- censReg(Percentage_Securities_Private_MBS_ABS_Structured ~ Log_Total_Assets + Log_Total_Assets_Squared + 
                                           Tier1_Leverage_Capital_Ratio_Percentage + Total_RiskBased_Capital_Ratio_Percentage + ROE_Percentage + 
                                           ROA_Percentage + Efficiency_Ratio_Percentage  + After06302012_Dummy + Log_Total_Assets*After06302012_Dummy + 
                                           Log_Total_Assets_Squared*After06302012_Dummy + Tier1_Leverage_Capital_Ratio_Percentage*After06302012_Dummy + 
                                           Total_RiskBased_Capital_Ratio_Percentage*After06302012_Dummy + ROE_Percentage*After06302012_Dummy +
                                           ROA_Percentage*After06302012_Dummy + Efficiency_Ratio_Percentage*After06302012_Dummy, left = 0, right = 100, 
                                         data = Bank_Data_Merged_Final_Total))
  save(panelResult1_ABS_Pooled, file = "panelResult1_ABS_Pooled.rda")
  
  
  
  ###Manualy completing likihood ratio test of null that pooled, i.e, no panel level effects, reject the null in this case 
  pchisq(-2*(logLik(panelResult1_ABS_Pooled)[[1]]-logLik(panelResult1_ABS)[[1]]),1, lower.tail = FALSE)
  
  
  
  #####Correlated Random Effects Model
  summary(panelResult1_ABS_Correlated <- censReg(Percentage_Securities_Private_MBS_ABS_Structured ~ Log_Total_Assets_Bar + Log_Total_Assets_Squared_Bar + 
                                               Tier1_Leverage_Capital_Ratio_Percentage_Bar + Total_RiskBased_Capital_Ratio_Percentage_Bar + 
                                               ROE_Percentage_Bar + ROA_Percentage_Bar + Efficiency_Ratio_Percentage_Bar + 
                                               Log_Total_Assets + Log_Total_Assets_Squared +
                                               Tier1_Leverage_Capital_Ratio_Percentage + Total_RiskBased_Capital_Ratio_Percentage + 
                                               ROE_Percentage + ROA_Percentage + Efficiency_Ratio_Percentage  + After06302012_Dummy + 
                                               Log_Total_Assets*After06302012_Dummy +Log_Total_Assets_Squared*After06302012_Dummy + 
                                               Tier1_Leverage_Capital_Ratio_Percentage*After06302012_Dummy + 
                                               Total_RiskBased_Capital_Ratio_Percentage*After06302012_Dummy + ROE_Percentage*After06302012_Dummy +
                                               ROA_Percentage*After06302012_Dummy + Efficiency_Ratio_Percentage*After06302012_Dummy, 
                                             left = 0, right = 100, tol = 10e-12, data = Bank_Data_Merged_Final_Total_Panel, method = "BHHH" , 
                                             nGHQ = 64, iterlim = 1000, start = c(panelResult1_ABS$estimate[[1]], Log_Total_Assets_Bar = 0, Log_Total_Assets_Squared_Bar = 0, 
                                                                                  Tier1_Leverage_Capital_Ratio_Bar = 0, Total_RiskBased_Capital_Ratio_Percentage_Bar = 0, ROE_Percentage_Bar = 0, 
                                                                                  ROA_Percentage_Bar = 0, Efficiency_Ratio_Percentage_Bar = 0, panelResult1_ABS$estimate[-1])))
  save(panelResult1_ABS_Correlated, file = "panelResult1_ABS_Correlated.rda") 
  
  
  length(panelResult1_ABS_Correlated$estimate)
  
  ###Testing if average coefficients are signifigant which would indicate correlated random effects model, reject random effects where 
  ## indivdual distiburance is not correalted witht the regressors
  pchisq(-2*(logLik(panelResult1_ABS)[[1]]-logLik(panelResult1_ABS_Correlated)[[1]]), df = 25-17, lower.tail = FALSE)
  
  
  
  
  ##Regression Output
  stargazer(panelResult1_ABS_Pooled, panelResult1_ABS, panelResult1_ABS_Correlated, font.size = "tiny", align = TRUE, title = "Results", 
            omit.stat = c("aic", "bic"), dep.var.labels = c("Percentage of Bank's Securities in Private Structured"),
            intercept.bottom = FALSE, notes = "", notes.align = "l")
  
  
  ####Partial Effects of Private ABS
  beta_hat_ABS <- panelResult1_ABS_Correlated$estimate[1:23]
  
  
  ###Sigma hat of the random error term for each observation, NOT the individual component variance, labels are different in paper than the regression output
  ###Because r package lables them differently, in R, mu is individula component, and nu is random component, in paper, a is individual component and 
  ### mu is random component 
  sigma_hat_nu_ABS <- sqrt(exp(panelResult1_ABS_Correlated$estimate[[25]])^2)
  partial_effects_results_ABS <- vector(length = length(total_assets))
  
  
  
  ####################Computing partial effects of regulation for doubling of bank assets
  for (i in 1:length(total_assets)) {
    
    ####First Partial Effect evaluated when dummy = 1, 
    dummy_1_covariate_vector <- c(1, averages[1:7], log(total_assets[i]), log(total_assets[i])^2, averages[8:12], 1,  
                                  log(total_assets[i]), log(total_assets[i])^2, averages[8:12])
    
    Dummy1_PartialEffect_ABS <- ( (coef(panelResult1_ABS_Correlated)[9] + 2*coef(panelResult1_ABS_Correlated)[10]*log(total_assets[i]) + 
                                 coef(panelResult1_ABS_Correlated)[17] + 2*coef(panelResult1_ABS_Correlated)[18]*log(total_assets[i]))*
                                ( pnorm((100 - dummy_1_covariate_vector %*% beta_hat_ABS) / (sigma_hat_nu_ABS)) - 
                                    pnorm((0 - dummy_1_covariate_vector %*% beta_hat_ABS) / (sigma_hat_nu_ABS))) ) *log(Increase_Assets)
  
    
    ###Second partial effect evalulated when dummy = 0, 
    dummy_0_covariate_vector <- c(1, averages[1:7], log(total_assets[i]), log(total_assets[i])^2, averages[8:12], rep(0,8))  
    
    
    Dummy0_PartialEffect_ABS <- ( (coef(panelResult1_ABS_Correlated)[9] + 2*coef(panelResult1_ABS_Correlated)[10]*log(total_assets[i]))*
                                ( pnorm((100 - dummy_0_covariate_vector %*% beta_hat_ABS) / (sigma_hat_nu_ABS)) - 
                                    pnorm((0 - dummy_0_covariate_vector %*% beta_hat_ABS) / (sigma_hat_nu_ABS))) ) *log(Increase_Assets)
    
    partial_effects_results_ABS[i] <- Dummy1_PartialEffect_ABS - Dummy0_PartialEffect_ABS
  }
  
  max(partial_effects_results_ABS)
  min(partial_effects_results_ABS)
  
  
  
  
  
  ###########Computing Average Partial Effects for all community bank observations with less than 1 billion in assets 
  
  ###Sigma hat of the random error term + the individual error component, see wooldrige page 710 bottom
  sigma_hat_ABS <- sqrt(exp(panelResult1_ABS_Correlated$estimate[[24]])^2 + exp(panelResult1_ABS_Correlated$estimate[[25]])^2)
  
  
  ##For every observation, need a predicted value of expcted percentage securities non-gov, for post and pre regulation, assets less than 1 billion 
  #Pre-Regulation Period 
  percentage_securities_nongov_predicted_pre_ABS <- as.matrix(Bank_Data_Merged_Final_Total_Panel_Pre) %*% beta_hat_ABS[1:16]
  
  ###Post Regulation Period 
  percentage_securities_nongov_predicted_post_ABS <- as.matrix(Bank_Data_Merged_Final_Total_Panel_Post) %*% beta_hat_ABS
 
   
  ###Using combination of page 705 Wooldrige equation 17.68 along with Wooldridge CRE Tobit slide 67 and Wooldridge page 710, computing 
  ###average partial effects for banks with less than asset of 1 billion between post and pre
  Dummy1_APE_ABS <- mean((coef(panelResult1_ABS_Correlated)[9] + 2*coef(panelResult1_ABS_Correlated)[10]*(as.matrix(Bank_Data_Merged_Final_Total_Panel_Post$Log_Total_Assets)) + 
                        coef(panelResult1_ABS_Correlated)[17] + 2*coef(panelResult1_ABS_Correlated)[18]*(as.matrix(Bank_Data_Merged_Final_Total_Panel_Post$Log_Total_Assets))) * 
                       ( pnorm((100 - percentage_securities_nongov_predicted_post_ABS) / (sigma_hat_ABS)) - pnorm((0 - percentage_securities_nongov_predicted_post_ABS) / (sigma_hat_ABS)))) * 
    log(Increase_Assets)
  
  
  Dummy0_APE_ABS <- mean((coef(panelResult1_ABS_Correlated)[9] + 2*coef(panelResult1_ABS_Correlated)[10]*(as.matrix(Bank_Data_Merged_Final_Total_Panel_Pre$Log_Total_Assets))) * 
                           ( pnorm((100 - percentage_securities_nongov_predicted_pre_ABS) / (sigma_hat_ABS)) - 
                               pnorm((0 - percentage_securities_nongov_predicted_pre_ABS) / (sigma_hat_ABS)))) * 
                            log(Increase_Assets)
  
  Average_Partial_Effects_ABS <- Dummy1_APE_ABS - Dummy0_APE_ABS
  Average_Partial_Effects_ABS
  
  
  summary( pnorm((100 - percentage_securities_nongov_predicted_pre_ABS) / (sigma_hat_ABS)) - 
             pnorm((0 - percentage_securities_nongov_predicted_pre_ABS) / (sigma_hat_ABS)))
  
  
  
  #Tobit Models for Percentage Securities Corporate, ran this model once before re=inputing "start" argument
  summary(panelResult1_Corporate <- censReg(Percentage_Securities_Corporate ~ Log_Total_Assets + Log_Total_Assets_Squared + 
                                        Tier1_Leverage_Capital_Ratio_Percentage + Total_RiskBased_Capital_Ratio_Percentage + ROE_Percentage + 
                                        ROA_Percentage + Efficiency_Ratio_Percentage  + After06302012_Dummy + Log_Total_Assets*After06302012_Dummy + 
                                        Log_Total_Assets_Squared*After06302012_Dummy + Tier1_Leverage_Capital_Ratio_Percentage*After06302012_Dummy + 
                                        Total_RiskBased_Capital_Ratio_Percentage*After06302012_Dummy + ROE_Percentage*After06302012_Dummy +
                                        ROA_Percentage*After06302012_Dummy + Efficiency_Ratio_Percentage*After06302012_Dummy, left = 0, right = 100, 
                                      data = Bank_Data_Merged_Final_Total_Panel, method = "BHHH" , nGHQ = 64, iterlim = 10000, tol = 10e-12, 
                                      start = panelResult1_Corporate$estimate))
  
  save(panelResult1_Corporate, file = "panelResult1_Corporate.rda")
  
  
  
  ########Testing pooled just in case
  summary(panelResult1_Corporate_Pooled <- censReg(Percentage_Securities_Corporate ~ Log_Total_Assets + Log_Total_Assets_Squared + 
                                               Tier1_Leverage_Capital_Ratio_Percentage + Total_RiskBased_Capital_Ratio_Percentage + ROE_Percentage + 
                                               ROA_Percentage + Efficiency_Ratio_Percentage  + After06302012_Dummy + Log_Total_Assets*After06302012_Dummy + 
                                               Log_Total_Assets_Squared*After06302012_Dummy + Tier1_Leverage_Capital_Ratio_Percentage*After06302012_Dummy + 
                                               Total_RiskBased_Capital_Ratio_Percentage*After06302012_Dummy + ROE_Percentage*After06302012_Dummy +
                                               ROA_Percentage*After06302012_Dummy + Efficiency_Ratio_Percentage*After06302012_Dummy, left = 0, right = 100, 
                                             data = Bank_Data_Merged_Final_Total))
  save(panelResult1_Corporate_Pooled, file = "panelResult1_Corporate_Pooled.rda")
  
  
  
  ###Manualy completing likihood ratio test of null that pooled, i.e, no panel level effects, reject the null in this case 
  pchisq(-2*(logLik(panelResult1_Corporate_Pooled)[[1]]-logLik(panelResult1_Corporate)[[1]]),1, lower.tail = FALSE)
  
  
  
  #####Correlated Random Effects Model
  summary(panelResult1_Corporate_Correlated <- censReg(Percentage_Securities_Corporate ~ Log_Total_Assets_Bar + Log_Total_Assets_Squared_Bar + 
                                                   Tier1_Leverage_Capital_Ratio_Percentage_Bar + Total_RiskBased_Capital_Ratio_Percentage_Bar + 
                                                   ROE_Percentage_Bar + ROA_Percentage_Bar + Efficiency_Ratio_Percentage_Bar + 
                                                   Log_Total_Assets + Log_Total_Assets_Squared +
                                                   Tier1_Leverage_Capital_Ratio_Percentage + Total_RiskBased_Capital_Ratio_Percentage + 
                                                   ROE_Percentage + ROA_Percentage + Efficiency_Ratio_Percentage  + After06302012_Dummy + 
                                                   Log_Total_Assets*After06302012_Dummy +Log_Total_Assets_Squared*After06302012_Dummy + 
                                                   Tier1_Leverage_Capital_Ratio_Percentage*After06302012_Dummy + 
                                                   Total_RiskBased_Capital_Ratio_Percentage*After06302012_Dummy + ROE_Percentage*After06302012_Dummy +
                                                   ROA_Percentage*After06302012_Dummy + Efficiency_Ratio_Percentage*After06302012_Dummy, 
                                                 left = 0, right = 100, tol = 10e-12, data = Bank_Data_Merged_Final_Total_Panel, method = "BHHH" , 
                                                 nGHQ = 64, iterlim = 1000, start = c(panelResult1_Corporate$estimate[[1]], Log_Total_Assets_Bar = 0, Log_Total_Assets_Squared_Bar = 0, 
                                                                                      Tier1_Leverage_Capital_Ratio_Bar = 0, Total_RiskBased_Capital_Ratio_Percentage_Bar = 0, ROE_Percentage_Bar = 0, 
                                                                                      ROA_Percentage_Bar = 0, Efficiency_Ratio_Percentage_Bar = 0, panelResult1_Corporate$estimate[-1])))
  save(panelResult1_Corporate_Correlated, file = "panelResult1_Corporate_Correlated.rda") 
  
  
  ###Testing if average coefficients are signifigant which would indicate correlated random effects model, reject random effects where 
  ## indivdual distiburance is not correalted witht the regressors
  pchisq(-2*(logLik(panelResult1_Corporate)[[1]]-logLik(panelResult1_Corporate_Correlated)[[1]]), df = 25-17, lower.tail = FALSE)
  
  

  ##Regression Output
  stargazer(panelResult1_Corporate_Pooled, panelResult1_Corporate, panelResult1_Corporate_Correlated, font.size = "tiny", align = TRUE, title = "Results", 
            omit.stat = c("aic", "bic"), dep.var.labels = c("Percentage of Bank's Securities in Corporate Fixed Income"),
            intercept.bottom = FALSE, notes = "", notes.align = "l")
  
  
  
  ####Partial Effects of Corporate Bonds
  beta_hat_Corporate <- panelResult1_Corporate_Correlated$estimate[1:23]
  
  ###Sigma hat of the random error term for each observation, NOT the individual component variance, labels are different in paper than the regression output
  ###Because r package lables them differently, in R, mu is individula component, and nu is random component, in paper, a is individual component and 
  ### mu is random component 
  sigma_hat_nu_Corporate <- sqrt(exp(panelResult1_Corporate_Correlated$estimate[[25]])^2)
  partial_effects_results_Corporate <- vector(length = length(total_assets))
  
  
  
  ####################Computing partial effects of regulation for doubling of bank assets
  for (i in 1:length(total_assets)) {
    
    ####First Partial Effect evaluated when dummy = 1, 
    dummy_1_covariate_vector <- c(1, averages[1:7], log(total_assets[i]), log(total_assets[i])^2, averages[8:12], 1,  
                                  log(total_assets[i]), log(total_assets[i])^2, averages[8:12])
    
    Dummy1_PartialEffect_Corporate <- ( (coef(panelResult1_Corporate_Correlated)[9] + 2*coef(panelResult1_Corporate_Correlated)[10]*log(total_assets[i]) + 
                                     coef(panelResult1_Corporate_Correlated)[17] + 2*coef(panelResult1_Corporate_Correlated)[18]*log(total_assets[i]))*
                                    ( pnorm((100 - dummy_1_covariate_vector %*% beta_hat_Corporate) / (sigma_hat_nu_Corporate)) - 
                                        pnorm((0 - dummy_1_covariate_vector %*% beta_hat_Corporate) / (sigma_hat_nu_Corporate))) ) *log(Increase_Assets)
    
    
    ###Second partial effect evalulated when dummy = 0, 
    dummy_0_covariate_vector <- c(1, averages[1:7], log(total_assets[i]), log(total_assets[i])^2, averages[8:12], rep(0,8))  
    
    Dummy0_PartialEffect_Corporate <- ( (coef(panelResult1_Corporate_Correlated)[9] + 2*coef(panelResult1_Corporate_Correlated)[10]*log(total_assets[i]))*
                                    ( pnorm((100 - dummy_0_covariate_vector %*% beta_hat_Corporate) / (sigma_hat_nu_Corporate)) - 
                                        pnorm((0 - dummy_0_covariate_vector %*% beta_hat_Corporate) / (sigma_hat_nu_Corporate))) ) *log(Increase_Assets)
    
    partial_effects_results_Corporate[i] <- Dummy1_PartialEffect_Corporate - Dummy0_PartialEffect_Corporate
  }
  
  
  max(partial_effects_results_Corporate)
  min(partial_effects_results_Corporate)
  
  
  
  
  ###########Computing Average Partial Effects for all community bank observations with less than 1 billion in assets 
  
  ###Sigma hat of the random error term + the individual error component, see wooldrige page 710 bottom
  sigma_hat_Corpoate <- sqrt(exp(panelResult1_Corporate_Correlated$estimate[[24]])^2 + exp(panelResult1_Corporate_Correlated$estimate[[25]])^2)
  
  
  ##For every observation, need a predicted value of expcted percentage securities non-gov, for post and pre regulation, assets less than 1 billion 
  #Pre-Regulation Period 
  percentage_securities_nongov_predicted_pre_Corporate <- as.matrix(Bank_Data_Merged_Final_Total_Panel_Pre) %*% beta_hat_Corporate[1:16]
  
  ###Post Regulation Period 
  percentage_securities_nongov_predicted_post_Corporate <- as.matrix(Bank_Data_Merged_Final_Total_Panel_Post) %*% beta_hat_Corporate
  
  
  ###Using combination of page 705 Wooldrige equation 17.68 along with Wooldridge CRE Tobit slide 67 and Wooldridge page 710, computing 
  ###average partial effects for banks with less than asset of 1 billion between post and pre
  Dummy1_APE_Corporate <- mean((coef(panelResult1_Corporate_Correlated)[9] + 2*coef(panelResult1_Corporate_Correlated)[10]*(as.matrix(Bank_Data_Merged_Final_Total_Panel_Post$Log_Total_Assets)) + 
                            coef(panelResult1_Corporate_Correlated)[17] + 2*coef(panelResult1_Corporate_Correlated)[18]*(as.matrix(Bank_Data_Merged_Final_Total_Panel_Post$Log_Total_Assets))) * 
                           ( pnorm((100 - percentage_securities_nongov_predicted_post_Corporate) / (sigma_hat_Corpoate)) - 
                               pnorm((0 - percentage_securities_nongov_predicted_post_Corporate) / (sigma_hat_Corpoate)))) * 
    log(Increase_Assets)
  
  
  Dummy0_APE_Corporate <- mean((coef(panelResult1_Corporate_Correlated)[9] + 2*coef(panelResult1_Corporate_Correlated)[10]*(as.matrix(Bank_Data_Merged_Final_Total_Panel_Pre$Log_Total_Assets))) * 
                           ( pnorm((100 - percentage_securities_nongov_predicted_pre_Corporate) / (sigma_hat_Corpoate)) - 
                               pnorm((0 - percentage_securities_nongov_predicted_pre_Corporate) / (sigma_hat_Corpoate)))) * 
    log(Increase_Assets)
  
  Average_Partial_Effects_Corporate <- Dummy1_APE_Corporate - Dummy0_APE_Corporate
  Average_Partial_Effects_Corporate
    
  
  summary(pnorm((100 - percentage_securities_nongov_predicted_pre_Corporate) / (sigma_hat_Corpoate)) - 
            pnorm((0 - percentage_securities_nongov_predicted_pre_Corporate) / (sigma_hat_Corpoate)))
  
  
  
Bank_Data_Merged_Final_Total_Panel %>% 
  filter(Percentage_Securities_Corporate==0) %>% 
  



  
  
#######################################################################################################

###Linear Regression Specifications, these are ultimately meaningless
###due to large cluster of observations at 0, need Tobit

##Fixed Effects and Clustering Examples for Notes
##summary(felm(y ~ x | Firm | 0 | 0, data=test)) # firm fixed effects
##summary(felm(y ~ x | Firm | 0 | Firm + Year, data=test)) # fixed effects with clustering

##Identifying Assumption is Equal Access banks to all of these securities - tough draw
##First round of specifications include log(assets)

##Specification 1  
# summary(Specification1 <- felm(Percentage_Securities_NoGov ~ Log_Total_Assets + Log_Total_Assets_Squared +  
#                          After06302012_Dummy + Log_Total_Assets*After06302012_Dummy + 
#                          Log_Total_Assets_Squared*After06302012_Dummy 
#                        | IDRSSD_ | 0 | IDRSSD_ , data = Bank_Data_Merged_Final_Total))


##Specification 2, added Quarter fixed effects
# summary(Specification2 <- felm(Percentage_Securities_NoGov ~ Log_Total_Assets + Log_Total_Assets_Squared +  
#                          After06302012_Dummy + Log_Total_Assets*After06302012_Dummy + 
#                          Log_Total_Assets_Squared*After06302012_Dummy 
#                        | IDRSSD_ + Quarter | 0 | IDRSSD_ , data = Bank_Data_Merged_Final_Total))



##Specification 3 - Added Controls to Specification 1
# summary(Specification3 <- felm(Percentage_Securities_NoGov ~ Log_Total_Assets + Log_Total_Assets_Squared + Percentage_Assets_Securities + 
#                          Efficiency_Ratio + After06302012_Dummy + Log_Total_Assets*After06302012_Dummy + 
#                          Log_Total_Assets_Squared*After06302012_Dummy + Percentage_Assets_Securities*After06302012_Dummy + 
#                          Efficiency_Ratio*After06302012_Dummy
#                        | IDRSSD_ | 0 | IDRSSD_ , data = Bank_Data_Merged_Final_Total))


# #Specification 4, added Quarter fixed effects 
# summary(Specification4 <- felm(Percentage_Securities_NoGov ~ Log_Total_Assets + Log_Total_Assets_Squared + Percentage_Assets_Securities + 
#                          Efficiency_Ratio + After06302012_Dummy + Log_Total_Assets*After06302012_Dummy + 
#                          Log_Total_Assets_Squared*After06302012_Dummy + Percentage_Assets_Securities*After06302012_Dummy + 
#                          Efficiency_Ratio*After06302012_Dummy
#                        | IDRSSD_ + Quarter| 0 | IDRSSD_ , data = Bank_Data_Merged_Final_Total))


##Corporate Bonds Percentages


##Identifying Assumption is Equal Access banks to all of these securities
# summmary(Specification6 <- felm(Percentage_Securities_Corporate ~ After06302012_Dummy + Log_Total_Assets + Percentage_Assets_Securities +
#                               Log_Total_Assets_Squared + Log_Total_Assets*After06302012_Dummy + 
#                               Percentage_Assets_Securities*After06302012_Dummy + 
#                               Log_Total_Assets_Squared*After06302012_Dummy | IDRSSD_ | 0 | IDRSSD_ , data = Bank_Data_Merged_Final_Total))
# 
# 
# 
# #Specification 7, Same as specification 6, but with quarter fixed effects as well
# summary(Specification7 <-  felm(Percentage_Securities_Corporate ~ Log_Total_Assets + Percentage_Assets_Securities +
#                           Log_Total_Assets_Squared + Log_Total_Assets*After06302012_Dummy + 
#                           Percentage_Assets_Securities*After06302012_Dummy + 
#                           Log_Total_Assets_Squared*After06302012_Dummy | IDRSSD_ + Quarter | 0 | IDRSSD_ , data = Bank_Data_Merged_Final_Total))
# 

##Specification 8 with Quantiles with bank fixed effects and after law announced dummy
# summary(Specification8 <- felm(Percentage_Securities_Corporate ~ After06302012_Dummy + Quantile + 
#                          Percentage_Assets_Securities + Quantile_Squared + 
#                          Quantile*After06302012_Dummy + Percentage_Assets_Securities*After06302012_Dummy  + 
#                          Quantile_Squared*After06302012_Dummy 
#                        | IDRSSD_ | 0 | IDRSSD_ , data = Bank_Data_Merged_Final_Total))
# 
# 
# #Specification9, same as specification 8, but with quarter fixed effects as well
# summary(Specification9 <- felm(Percentage_Securities_Corporate ~ Quantile + 
#                          Percentage_Assets_Securities + Quantile_Squared + 
#                          Quantile*After06302012_Dummy + Percentage_Assets_Securities*After06302012_Dummy  + 
#                          Quantile_Squared*After06302012_Dummy 
#                        | IDRSSD_ + Quarter| 0 | IDRSSD_ , data = Bank_Data_Merged_Final_Total))
# 
# 
# 
# 
# ###Private Label MBS, BAs, CMBS, and Structurd :
# 
# ##Identifying Assumption is Equal Access banks to all of these securities
# summary(Specification10 <- felm(Percentage_Securities_Private_MBS_ABS_Structured ~ After06302012_Dummy + Log_Total_Assets + Percentage_Assets_Securities +
#                          Log_Total_Assets_Squared + Log_Total_Assets*After06302012_Dummy + 
#                          Percentage_Assets_Securities*After06302012_Dummy + 
#                          Log_Total_Assets_Squared*After06302012_Dummy | IDRSSD_ | 0 | IDRSSD_ , data = Bank_Data_Merged_Final_Total))
# 
# 
# 
# #Specification 11, Same as specification 10, but with quarter fixed effects as well
# summary(Specification11 <-  felm(Percentage_Securities_Private_MBS_ABS_Structured ~ Log_Total_Assets + Percentage_Assets_Securities +
#                           Log_Total_Assets_Squared + Log_Total_Assets*After06302012_Dummy + 
#                           Percentage_Assets_Securities*After06302012_Dummy + 
#                           Log_Total_Assets_Squared*After06302012_Dummy | IDRSSD_ + Quarter | 0 | IDRSSD_ , data = Bank_Data_Merged_Final_Total))
# 
# ##Specification 12 with Quantiles with bank fixed effects and after law announced dummy
# summary(Specification12 <- felm(Percentage_Securities_Private_MBS_ABS_Structured ~ After06302012_Dummy + Quantile + 
#                          Percentage_Assets_Securities + Quantile_Squared + 
#                          Quantile*After06302012_Dummy + Percentage_Assets_Securities*After06302012_Dummy  + 
#                          Quantile_Squared*After06302012_Dummy 
#                        | IDRSSD_ | 0 | IDRSSD_ , data = Bank_Data_Merged_Final_Total))
# 
# 
# #Specification13, same as specification 12, but with quarter fixed effects as well
# summary(Specification13<- felm(Percentage_Securities_Private_MBS_ABS_Structured ~ Quantile + 
#                          Percentage_Assets_Securities + Quantile_Squared + 
#                          Quantile*After06302012_Dummy + Percentage_Assets_Securities*After06302012_Dummy  + 
#                          Quantile_Squared*After06302012_Dummy 
#                        | IDRSSD_ + Quarter| 0 | IDRSSD_ , data = Bank_Data_Merged_Final_Total))
# 



