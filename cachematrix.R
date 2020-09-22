## This function creates a list comprising of 4 functions:

## set <- sets matrix x in the makeCacheMatrix context 
##        and clears the previous cached inverse (m)

## get <- gets matrix x in the makeCacheMatrix context 

## setinverse <- sets cached inverse m in the makeCacheMatrix context

## getinverse <- cached inverse m in the makeCacheMatrix context 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function checks whether the inverse was already calculated and cached
## If that's the case, just returns the cached value
## Otherwise, calculates the inverse via solve function, caches and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
