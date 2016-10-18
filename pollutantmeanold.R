pollutantmean <- function(directory, pollutant, id = 1:332) {
      ## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files
      
      ## 'pollutant' is a character vector of length 1 indicating
      ## the name of the pollutant for which we will calculate the
      ## mean; either "sulfate" or "nitrate".
      
      ## 'id' is an integer vector indicating the monitor ID numbers
      ## to be used
      
      # vector containing all values of the desired pollutant
      despoll <- NULL
      
      # for each id passed
      for(i in id){ 
            # convert id to 3 digit width
            idstr <- sprintf("%03d", i)
            # identify file reqd
            fileloc <- paste(getwd(),"/",directory, "/", idstr, ".csv",sep="") 
            # read the file in
            monitordata <- read.csv(fileloc)
            
            # for each entry in the file
            for (j in 1:nrow(monitordata)){
                  # pull the pollutant desired
                  newpoll <- monitordata[j,pollutant]
                  # if it isn't NA for this entry
                  if(!is.na(newpoll)){
                        # append it onto the pollutant vector
                        despoll <- append(despoll,newpoll)
                  }
            }       
      }
      
      ## Return the mean of the pollutant across all monitors list
      ## in the 'id' vector (ignoring NA values)
      ## NOTE: Do not round the result!
      
      mean(despoll)
}

