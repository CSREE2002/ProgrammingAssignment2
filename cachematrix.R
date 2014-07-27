# Programming Assignment 2:
# Write an R function is able to cache potentially time-consuming computations

# Matrix inversion is usually a costly computation and their may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly
# This assignment is to write a pair of functions that cache the inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse.
# The special "matrix",  is really a list containing a function to
#   set the matrix
#   get the matrix
#   set the matrix inverse
#   get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  # invx stores inverse of matrix
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }

  get <- function() x
  setinverse <- function(inverse) invx <<- inverse
  getinverse <- function() invx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
# has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data (matrix)")
    return(m)
  }
  data <- x$get()
  # Computing the inverse of a square matrix can be done with the solve function in R
  # For this assignment, assume that the matrix supplied is always invertible.
  # For example, if X is a square invertible matrix, then solve(X) returns its inverse.
  # if not square invertible matrix, solve would throw error
  # example:
  # --- wrong:
  # > k = matrix (rnorm(100), 5,2)
  # > solve(k)
  # Error in solve.default(k) : 'a' (5 x 2) must be square
  # --- right:
  # > k = matrix (rnorm(100), 2,2)
  # > solve(k)
  # [,1]     [,2]
  # [1,]  1.001194 1.910409
  # [2,] -1.279846 1.645838
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
