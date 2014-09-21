## A pair of functions to facilitate the cacheing of a matrix and its inverse

## Creates an object to store, set, and get the matrix and inverse 
## matrix of a given input matrix
## Function Description:
## get() - Returns the current matrix
## set() - Sets the current matiix
## get.inverse() - Retuns the inverse matrix
## set.inverse() - Sets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- x
  im <- NULL
  set <- function(y) {
    m <<- y
    im <<- NULL
  }
  get <- function() m
  set.inverse <- function(inverse.matrix) im <<- inverse.matrix
  get.inverse <- function() im
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


## Solves for the inverse matrix for a makeCacheMatrix object, or returns 
## the current inverse matrix if it's already solved

cacheSolve <- function(x, ...) {
  im <- x$get.inverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  m <- x$get()
  im <- solve(m)
  x$set.inverse(im)
  im
        ## Return a matrix that is the inverse of 'x'
}
