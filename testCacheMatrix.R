## Unit tests

test.cacheMatrix <- function() {
  aMatr <- matrix(c(1,2,3,0,1,4,5,6,0),3,3)
  cacheMatrix <- makeCacheMatrix(aMatr)
  checkEquals(cacheMatrix$get(), aMatr)
}

test.cacheSolve <- function() {
  aMatr <- matrix(c(1,2,3,0,1,4,5,6,0),3,3)
  cacheMatrix <- makeCacheMatrix(aMatr)
  checkEquals(cacheSolve(cacheMatrix), solve(aMatr))
}

test.cacheSolveMoreMatrix <- function() {
  aMatr <- matrix(c(1,2,3,0,1,4,5,6,0),3,3)
  aMatr2 <- matrix(c(1,3,2,0,1,5,4,6,0),3,3)
  cacheMatrix <- makeCacheMatrix(aMatr)
  checkEquals(cacheSolve(cacheMatrix), solve(aMatr))
  temp <- solve(aMatr2)
  
  checkTrue(!identical(cacheSolve(cacheMatrix) ,temp) )
  cacheMatrix$set(aMatr2)
  checkTrue( is.null(cacheMatrix$getInverse()))
  checkEquals(cacheSolve(cacheMatrix),solve(aMatr2))
}