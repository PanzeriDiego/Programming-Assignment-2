makeCacheMatrix <- function(x = matrix()) {
 
  pippo <- NULL
  
  set <- function(y) {                          ## Allows to change the argument
    x <<- y                                     ## matrix to a new one (w/out calling
    pippo <<-NULL                               ## the function again) and clears the cache.
  }                                             ## "<<-" allows to reassign values to variables
                                                ## in the (upper) parent environment.
  
  get <- function() x                           ## Returns the initial matrix (x).
  
  setsolve <- function(solve) pippo <<- solve       ## Changes the value of the inverse (pippo).
  
  getsolve <- function() pippo                      ## Returns the value of the inverse (pippo).
  
  list(set = set, get = get,                    ## Defines a list that allows to call
       setsolve = setsolve,                     ## the 4 function independently, given 
       getsolve = getsolve)                     ## we assign makeCacheMatrix to a variable,
}                                               ## using the format *variable*$*function*.


## This function returns the inverse of the special matrix. By checking the "pippo"
## variable of "makeCacheMatrix" it can either return an existing cached value or,
## if pippo=NULL, compute the inverse and cache the value for later use.

cacheSolve <- function(x, ...) {
 
  pippo <- x$getsolve()                             ## Retrieves the s value from "makeCacheMatrix".
  
  if(!is.null(pippo)) {                             ## If "pippo" as a value different from NULL,
    message("getting cached data")                  ## it is returned without computing it again.
    return(pippo)
    
  } else {
    
    data <- x$get()                             ## If "pippo" is NULL, the initial matrix is retrieved
    pippo <- solve(data, ...)                   ## in "data", the inverse is computed and the result
    x$setsolve(pippo)                           ## is passed to the "setsolve" function that caches
                                                ## the value in the variable "pippo" of "makeCacheMatrix".
  }
  
  pippo                                             ## Prints the calculated or retrieved inverse matrix.
}
