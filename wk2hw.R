pollutantmean <- function(directory, pollutant, id=1:332){
  ## directory is a char vector w/ length 1 indicating location
  ## of CSV files
  
  ## pollutant is a char vector w/ length 1 indicating either
  ## sulfate or nitrate
  
  ## id is an integer vector indicating the monitor ID(s) to be used
  
  ## return the mean of the pollutant across all monitors listed
  ## in 'id' vector, ignoring NA values
  ## DO NOT ROUND
  
  read.csv(c(directory, ))
}