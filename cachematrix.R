## [creates a special matrix that will inverse its value and cache it]
## It contains 4 functions get, set -> to get and set the values of the matrix 
## getCache and setCache functions will get and set the inversed value from and to the cache

## makeCacheMatrix will accept a matrix of inversible numbers and return a special matrix of the 
## numbers and the 4 functions to assist in getting or setting the cache values

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setCache <- function(sVar) { m <<- sVar }
  
  getCache <- function() m
  
  list(set = set, get = get,
       setCache = setCache,
       getCache = getCache)
  
}

## Accepts a matrix as an argument. It checks to see if the inverse of it exists in the 
## cache, if yes, it gets the values from the cache and returns as a matrix
## if not found, it creates the inverse of the matrix to store in the cache and returns 
## the output as a matrix as well
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getCache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setCache(m)
  m
  
}
