## These functions allow the inverse of a matrix to be stored in a cache
## to save repeating this computationally intensive activity.
## In it's current form it is assumed that the matrix supplied
## is ALWAYS invertible.

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## This function computes the inverse of the matrix returned by the function
## makeCacheMatrix. If the inverse has already been calculated,
## (and the matrix has not changed), then the cacheSolve should retrieve
## the inverse from the cache, saving computational resources.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
