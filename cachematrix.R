## These functions are used to calculate the inverse of a matrix and cache the inverse
## so that it does not have to be recalculated every time it is needed
## Usage: 
## 1. create a faux "matrix" object from the actual matrix that you wish to invert
## >mat <- makeCacheMatrix(x) # where x is the actual matrix
## 2. Calculate and cache the inverse of the matrix
## >cacheSolve(mat)
## Every subsequent call to the cacheSolve() function will return the cached inverse of the matrix
## provided that the matrix has not changed.


## Create a "Matrix" object that can cache its inverse
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


## Calculate the inverse of a matrix, or retrieve it from cache if it has already been calculated
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting Cached Inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
