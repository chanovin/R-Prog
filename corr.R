corr <- function(directory, threshold = 0) {
      ## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files
      
      ## 'threshold' is a numeric vector of length 1 indicating the
      ## number of completely observed observations (on all
      ## variables) required to compute the correlation between
      ## nitrate and sulfate; the default is 0
      
      ## Return a numeric vector of correlations
      ## NOTE: Do not round the result!
  
  ## initialize correlation vector
  correlation <- numeric()
  
  ## get a list of complete cases
  compList <- complete(directory)
  
  ## initialize a vector of all monitor IDs
  ##id = 1:332
  
  ## initialize an index counter for the correlation vector
  j <- 1
  
  ## for each monitor
  ## for (i in id){
      ## read the csv file
      ## thisMonitor <- read.csv(paste(directory,"/",sprintf("%03d",i),".csv",sep=''))
      ## get a vector of complete cases
      ## goodObs <- complete.cases(thisMonitor)
      ## check if the number of complete cases exceeds the threshhold
      ## if(sum(goodObs) > threshold){
          ## reduce the monitor data to complete cases only
          ## goodMon <- thisMonitor[goodObs,]
          ## calculate the correlation between sulfate and nitrate
          ## and store it in the correlation vector
          ## correlation[j] <- cor(goodMon$sulfate, goodMon$nitrate)
          ## increment index
          ## j <- j+1
  ##    }
  
  
  ## for each monitor
  for (i in 1:332){
      ## if the observations are greater than threshold
      if(compList[i,"nobs"] > threshold){
        ## read the corresponding csv file  
        thisMon <- read.csv(paste(directory,"/",sprintf("%03d",i),".csv",sep=''))
        ## keep only the good cases
        goodMon <- thisMon[complete.cases(thisMon),]
        ## calculate and store a correlation value for that monitor file
        correlation[j] <- cor(goodMon$sulfate,goodMon$nitrate)
        ## increment the index
        j <- j+1
      }
  }
  correlation
}