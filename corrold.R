corr <- function(directory, threshold = 0) {
      ## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files
      
      ## 'threshold' is a numeric vector of length 1 indicating the
      ## number of completely observed observations (on all
      ## variables) required to compute the correlation between
      ## nitrate and sulfate; the default is 0
      
      # get a table of nobs for the directory
      comlst <- complete(directory)
      # instantiate output list
      corrlst <- NULL
      # for each monitor
      for(i in 1:nrow(comlst)){
            # if we exceed threshhold (minimum) of nobs
            if(comlst[i, 2] > threshold){
                  # convert id to 3 digit width
                  idstr <- sprintf("%03d", comlst[i,1])
                  # identify file reqd
                  fileloc <- paste(getwd(),"/",directory, "/", idstr, ".csv",sep="") 
                  
                  # debugging code below
                  #### fileloc <- paste(getwd(),"/","specdata", "/", idstr, ".csv",sep="") 
                  # read the file in
                  mondat <- read.csv(fileloc)
                  # add the correlation to the output
                  newcor <- cor(mondat$sulfate,mondat$nitrate, use="complete.obs")
                  corrlst <- append(corrlst,newcor)
            }
      }
      corrlst
      ## Return a numeric vector of correlations
      ## NOTE: Do not round the result!
}