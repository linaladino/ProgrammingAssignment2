## Put comments here that give an overall description of what your
## functions do
## these functions take an invertible matrix and allow you to store its inverse matrix in cache memory 
## in order to return the result efficiently without taking care about its dimensions or time consuming 
## in its calculation.
## Write a short comment describing this function

## for example, this particular function saves the original matrix, its inverse image to be able to 
## use it later.

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


## Write a short comment describing this function

## this function checks if there is an inverse matrix saved by the previous function, otherwise
## this function calculates the inverse matrix and saves it in the cache memory of the new object.
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
