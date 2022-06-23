#store all the companies' data in one folder
setwd("/Users/your_username/Downloads/snp500/")
#create a variable files which is a list of all the files in the folder above
files<-list.files(getwd(),recursive=TRUE)
#create an empty dataframe for later
finalDataframe <- data.frame(companyName=character(), diffDate=double(), cagr=double(), avgFirstPrices=double(), avgLastPrices=double(), firstDate=character(), lastDate=character())
#starting the mega for loop for all 500 odd companies
for (i in 1:502) {
  #loading the first CSV file in the csvFile variable
  csvFile <- read.csv(files[i])
  #naming the variable companyName as the name of the CSV file without the extension
  companyName <- tools::file_path_sans_ext(files[i])
  
  #creating firstDate and lastDate as the first and last date in the file respectively. 
  firstDate <- as.Date(csvFile[1,1])
  lastDate <- as.Date(csvFile[nrow(csvFile),1])
  #difftime gives the difference in days, which we divide to get years and we take the nearest whole number for simplicity
  diffDate <- floor(as.double(difftime(lastDate,firstDate))/365)
  
  #creating variables
  lastPrices <- c()
  firstPrices <- c()
  
  #A for loop to take the average price of the last 12 months 
  for (i in 0:11) {
    tempValue <- csvFile[nrow(csvFile)-i,6] #sixth column holds the adjusted closing price, which includes other financial data such as stock splits or dividends
    lastPrices <- c(lastPrices, tempValue) #append the values to this variable 
  }
  avgLastPrices <- mean(lastPrices) #taking the mean
  
  #same loop for first 12 months
  for (i in 0:11) {
    tempValue2 <- csvFile[1+i,6]
    firstPrices <- c(firstPrices, tempValue2)
  }
  avgFirstPrices <- mean(firstPrices)
  
  #formula for calculating CAGR
  cagr <- 100*(((avgLastPrices/avgFirstPrices)^(1/diffDate))-1)
  
  #Writing the above variables into the first 7 columns of the data frame, after the last filled row of the data frame 
  finalDataframe[nrow(finalDataframe)+1,1:7] <- c(companyName, diffDate, cagr, avgFirstPrices, avgLastPrices, firstDate, lastDate)
  
  #A for loop to know the yearly growth rates
  for (i in 0:(diffDate-1)) { #for all the years the stock data is present
    tempValue3 <- csvFile[(1 +(i*12)),6] #taking the 1st, 13th, 25th month and so on
    tempValue4 <- csvFile[(13 +(i*12)),6] #taking the 13th, 25th, 37th month and so on
    tempValue5 <- (tempValue4/tempValue3)-1 
    finalDataframe[nrow(finalDataframe),8+i] <- tempValue5 #writing the growth rates of all the years as appended columns to the latest row of the data frame
    names(finalDataframe)[8+i] <- paste0('Year',1+i) #naming all the above columns as Year1, Year2 and so on (CAUTION: Year 1 could be 1991, or later if the company data starts from a later date)
    
  }
  
  
}


#Writing our results in a CSV file. We're done!
write.csv(finalDataframe, "/Users/yourusername/Desktop/FINALLL.csv")
