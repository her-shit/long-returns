setwd("/Users/your_username/Downloads/snp500/")
files<-list.files(getwd(),recursive=TRUE)
finalDataframe <- data.frame(companyName=character(), diffDate=double(), cagr=double(), avgFirstPrices=double(), avgLastPrices=double(), firstDate=character(), lastDate=character())

for (i in 1:502) {
  csvFile <- read.csv(files[i])
  companyName <- tools::file_path_sans_ext(files[i])
  
  
  firstDate <- as.Date(csvFile[1,1])
  lastDate <- as.Date(csvFile[nrow(csvFile),1])
  diffDate <- floor(as.double(difftime(lastDate,firstDate))/365)
  
  lastPrices <- c()
  firstPrices <- c()
  for (i in 0:11) {
    tempValue <- csvFile[nrow(csvFile)-i,6]
    lastPrices <- c(lastPrices, tempValue)
  }
  avgLastPrices <- mean(lastPrices)
  
  for (i in 0:11) {
    tempValue2 <- csvFile[1+i,6]
    firstPrices <- c(firstPrices, tempValue2)
  }
  avgFirstPrices <- mean(firstPrices)
  
  cagr <- 100*(((avgLastPrices/avgFirstPrices)^(1/diffDate))-1)
  
  finalDataframe[nrow(finalDataframe)+1,1:7] <- c(companyName, diffDate, cagr, avgFirstPrices, avgLastPrices, firstDate, lastDate)
  
  for (i in 0:(diffDate-1)) {
    tempValue3 <- csvFile[(1 +(i*12)),6]
    tempValue4 <- csvFile[(13 +(i*12)),6]
    tempValue5 <- (tempValue4/tempValue3)-1
    finalDataframe[nrow(finalDataframe),8+i] <- tempValue5
    names(finalDataframe)[8+i] <- paste0('Year',1+i)
    
  }
  
  
}



write.csv(finalDataframe, "/Users/yourusername/Desktop/FINALLL.csv")
