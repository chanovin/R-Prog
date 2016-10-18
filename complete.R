complete <- function(directory, id = 1:332) {
      ## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files
      
      ## 'id' is an integer vector indicating the monitor ID numbers
      ## to be used
      
      ## Return a data frame of the form:
      ## id nobs
      ## 1  117
      ## 2  1041
      ## ...
      ## where 'id' is the monitor ID number and 'nobs' is the
      ## number of complete cases
      
    ## initialize a list
    nobslist <- data.frame(id=NA, nobs=NA)
    ## initialize an index counter
    j <- 1
    
    
    ## for each monitor
    for(i in id){
      ## read in the monitor csv file
      thisMonitor <- read.csv(paste(directory,"/",sprintf("%03d",i),".csv",sep=''))
      ## copy the id number to the nobslist
      nobslist[j,] <- i
      ## count the number of complete cases and place it in the nobs column
      nobslist[j,"nobs"] <- sum(complete.cases(thisMonitor))
      ## increment j
      j <- j+1
    }
  
    ## print outcome
    nobslist
}