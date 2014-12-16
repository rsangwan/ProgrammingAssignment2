##
## Objective: Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix  
## rather than computing it repeatedly.The following pair of functions 
## (makeCacheMatrix and cacheSolve) cache the inverse of a matrix.
##

##
##  Function: makeCacheMatrix 
##  Purpose:  This function creates a special "matrix" object that
##  can cache its inverse
##
makeCacheMatrix <- function(mtrx = matrix()) {
  invrse <- NULL
  set <- function(m) {
    mtrx <<- m
    invrse <<- NULL
  }
  get <- function() mtrx
  setinverse <- function(i) invrse <<- i
  getinverse <- function() invrse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## 
## Function: cacheSolve 
## Purpose: This function computes the inverse of the special 
## "matrix" returned by  makeCacheMatrix  above. If the inverse has 
## already been calculated (and the matrix has not changed), then  
## cacheSolve retrieves the inverse from the cache.
## Assumption: The special matrix returned by makeCacheMatrix is
## always invertible
##
cacheSolve <- function(m, ...) {
  i <- m$getinverse()
  if(!is.null(i)) {
    message("getting cached matrix inverse")
    return(i)
  }
  data <- m$get()
  i <- solve(data, ...)
  m$setinverse(i)
  
  ## Return a matrix that is the inverse of 'm'
  i
}
