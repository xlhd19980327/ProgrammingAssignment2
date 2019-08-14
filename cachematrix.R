## Can calculate the inverse matrix of a customized matrix, 
## as well as cache the result. 

## Fuctions that generates functions of setting matrix, calculating the 
## inverse matrix, caching the result and changing the setting matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  func <- list()
  func$set <- function(x = matrix()){
    x <<- x
    inverse <<- NULL
  }
  func$get <- function() x
  func$setinv <- function(x) inverse <<- x
  func$getinv <- function() inverse
  return(func)
}


## Function that returns the inverse matrix or the caching matrix result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinv(inverse)
  return(inverse)
}
