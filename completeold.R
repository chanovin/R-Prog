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
      
      noblst <- data.frame(id=integer(), nobs=integer())
      
      # for each id passed
      for(i in id){ 
            # convert id to 3 digit width
            idstr <- sprintf("%03d", i)
            # identify file reqd
            fileloc <- paste(getwd(),"/",directory, "/", idstr, ".csv",sep="") 
            # read the file in
            mondat <- read.csv(fileloc)
            
            # set monitor id
            noblst[nrow(noblst)+1,1] <- i
            # set nobs to 0
            monnobs <- 0
            
            # for each entry in the file
            for (j in 1:nrow(mondat)){
                  # check if all three rows are true
                  if(!any(is.na(mondat[j,]))){
                        # if true, increment nobs for this monitor id
                        monnobs <- monnobs+1
                  }
            }
            # set nobs in noblst for this monitor
            noblst[nrow(noblst),2] <- monnobs
      }
      # print nob list
      noblst
}