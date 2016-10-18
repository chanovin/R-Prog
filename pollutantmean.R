pollutantmean <- function(directory, pollutant, id=1:332){
  ## directory is a char vector w/ length 1 indicating location
  ## of CSV files
  
  ## pollutant is a char vector w/ length 1 indicating either
  ## sulfate or nitrate
  
  ## id is an integer vector indicating the monitor ID(s) to be used
  
  ## return the mean of the pollutant across all monitors listed
  ## in 'id' vector, ignoring NA values
  ## DO NOT ROUND
  
  ## initialize a vector of means and observations for each monitor
  monitorData <- data.frame(monAvg=NA, monObs=NA)
  
  ## initialize an index variable, j
  j <- 1
  
  ## for each id (monitor) in the vector
  for (i in id){
    ## read the csv located at directory/id.csv
    ## note that IDs are padded to three characters
    thisMonitor <- read.csv(paste(directory,"/",sprintf("%03d",i),".csv",sep=''))
    ## set the corresponding index of monitorMeans to the current monitor's
    ## mean value for the desired pollutant, removing NAs
    monitorData[j,"monAvg"] <- mean(thisMonitor[,pollutant],na.rm=TRUE)
    ## count the number of non-NA observations for the current monitor's
    ## desired pollutant
    monitorData[j,"monObs"] <- sum(!is.na(thisMonitor[,pollutant]))
    
    ##increment j before looping
    j <- j+1
  }
  
  ## when the vector contains means and counts for all monitors of interest
  ## calculate the overall mean
  ## avoid NAs when calculating
  sum(monitorData$monAvg * monitorData$monObs, na.rm=TRUE) / sum(monitorData$monObs)
}