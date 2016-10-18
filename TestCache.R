makeCacheMatrix <- function(x = numeric()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function(){
            x    
      } 
      setsol <- function(solve){
            s <<- sol
      }
      getsol <- function(){
            s
      }
      list(set = set, get = get, setsol = setsol, getsol = getsol)
}

cacheSolve <- function(x, ...) {
      s <- x$getsol()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setsol(s)
      s
}

makeVector <- function(x = numeric()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmean <- function(mean) m <<- mean
      getmean <- function() m
      list(set = set, get = get,
           setmean = setmean,
           getmean = getmean)
}

cachemean <- function(x, ...) {
      m <- x$getmean()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- mean(data, ...)
      x$setmean(m)
      m
}