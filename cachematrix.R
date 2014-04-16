##
## makeCacheMatrix
##
## Create a special 'matrix object' with methods for getting and setting a memoized value
## 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##
## cacheSolve
##
## A wrapper to memoize the calculation of an inverse matrix
##
##  Note: Only works for matrices that have non-zero determinant since solve
##        will fail for those cases since there is no solution.
##

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


#
#  Test (and example usage)
#
# source('~/cacheMatrix.R')
#
# m <- matrix(c(1,2,3,4),2,2)
# v <- makeCacheMatrix(m)
#
# Should produce the identity matrix
# cacheSolve(v) %*% m
#
# Should use cached version to produce identity matrix
# cacheSolve(v) %*% m
