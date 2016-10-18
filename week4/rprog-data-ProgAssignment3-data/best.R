best <- function(state, outcome){
      ## read outcome data
      data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
      
      ## check that state is valid
      if(!(state %in% state.abb)){
            stop('invalid state')
      }
      
      ## check if outcome is valid
      else if(!(outcome=="heart attack" || outcome=="heart failure" ||
              outcome=="pneumonia")){
            # if you get here, outcome is invalid
            stop('invalid outcome')
      }
      
      else {
            ## determine which col you care about
            if(outcome=="heart attack"){
                  outCol <- 11      
            }
            else if(outcome=="heart failure"){
                  outCol <- 17
            }
            else{
                  outCol <- 23
            }
            ## reduce down to the state you care about
            data <- data[data$State==state,]
            
            ## force column of interest to numeric
            data[,outCol] <- as.numeric(data[,outCol])
            
            ## print state dataset for debugging
            ## print(data[,2])
            
            ## find the lowest mortality rate of interest
            lowest = min(data[,outCol], na.rm = TRUE)
            ## print(lowest)
            
            ## return hospital name in that state with lowest 30d mortality
            hospName <- data$Hospital.Name[data[,outCol] == lowest]
            
            ## print dataset for debugging
            ## print(hospName)
            sort(hospName)
            ## hospName
      }
      
      
}