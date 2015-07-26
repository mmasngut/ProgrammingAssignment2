#This file consist of 2 functions i.e. makecacheMAtix() and cacheSolve()


# This function creates a special "matrix" object that can cache its inverse.
# The "matrix" created has four object functions that could do as follows:
# 1.  set the value of the matrix
# 2.  get the existing value of the matrix
# 3.  set the value of the inverse matrix
# 4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  x <- matrix()
  i <- NULL
  
  #1. set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #2. get the existing value of the matrix
  get <- function() x
  
  #3. set the value of inverse matrix.
  setinverse <- function(inverse) i <<- inverse
  
  #4. get the value of inverse matrix either compute it if its not yet calculated
  ##  or retrive from cache
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## try to get the inverse matrix
  ## if already been calculated, retrieve from cache
  ## and return the inverse value and exit from cacheSolve() function
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #This part of code only implemented if no inverse was cached
  #Get the matrix then compute the inverse and set the new inverse value
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
