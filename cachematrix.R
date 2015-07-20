## Implementation of a cache for the inverse of a matrix.

## This function implement a cache for the solve (or Inverse) matrix.
## When create the chacheMatrix the original matrix is stored in the environment, 
## the new cache matrix expose the method get/set and getInverse/setInverse in order to 
## store/retrieve the original matrix and her inverse, after the function cacheSolve is invoked
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(anInverse) inv <<- anInverse
  getInverse <- function() inv
  list(set = set, get=get, setInverse = setInverse, getInverse = getInverse)
}


## giving as parameter a cacheMatrix calculate the inverse using solve and return it
## if the cacheMatrix has already the inverse calculated return it.
cacheSolve <- function(x, ...) {
  invers <- x$getInverse()
  if(!is.null(invers) ) {
    return(invers)
  }
  data <- x$get()
  temp <- solve(data, ...)
  x$setInverse(temp)
  temp
}
