## Provides functions to create a matrix object, calculate the inverse of
## the matrix, and cache the inverse of the matrix to increase efficience
## Only works for square matrices

## Returns a list of functions that get/set a matrix object and get/set
## the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns the inverse of a matrix object
## Checks first to see if the inverse is cached, and computes it if it is not
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  
  if(!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
