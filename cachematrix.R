## The first function, makeVector creates a special "matrix"
## object that can cache its inverse, the special "matrix"
## is really a list containing a function to:
##    * set the value of the vector
##    * get the value of the vector
##    * set the value of the mean
##    * get the value of the mean

## The second function calculates the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

## makeCacheMatrix: This function creates a special
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv_matrix <<- solve
  getinverse <- function() inv_matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of
## the special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv_matrix <- x$getinverse()
    if(!is.null(inv_matrix)) {
      message("Getting cached matrix inverse")
      return(inv_matrix)
    }
    data <- x$get()
    inv_matrix <- solve(data, ...)
    x$setinverse(inv_matrix)
    inv_matrix
}