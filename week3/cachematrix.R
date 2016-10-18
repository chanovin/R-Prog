## makeCacheMatrix() creates an object that can store its inverse.
## cacheSolve() checks for a cached inverse before solving.
## Written by Vincent Chan [chanovin], based on code by Roger Peng [rdpeng].

## makeCacheMatrix() stores a matrix of values as well as four functions:
## * set - to set/store the value of the object
## * get - to return the value of the object
## * setsol - to set/store the inverse of the object
## * getsol - to return the stored inverse of the object

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            ## reset the solution to NULL to prevent cacheSolve() from returning
            ## a defunct inverse for a prior value of x
            s <<- NULL
      }
      get <- function(){
            x    
      } 
      setsol <- function(sol){
            s <<- sol
      }
      getsol <- function(){
            s
      }
      list(set = set, get = get, setsol = setsol, getsol = getsol)
}


## cacheSolve takes an object created by makeCacheMatrix() and checks for a 
## stored inverse. If that inverse exists, it is returned. If not, cacheSolve() 
## solves for it and returns it.

cacheSolve <- function(x, ...) {
      ## Get any solution in object x
      s <- x$getsol()
      ## If it exists, return it
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      ## If not, get the value of object x
      data <- x$get()
      ## solve for its inverse
      s <- solve(data, ...)
      ## cache the inverse for future use
      x$setsol(s)
      ## return the inverse
      s
}
