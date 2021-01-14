#Peer graded assignment week 3 - R programming course

## 1. This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  # Start the inverse
  invmatrix <- NULL
  # Define and set the matrix
  set <- function(y) {
    m <<- matrix
    invmatrix <<- NULL
  }
  get <- function() m
  # Set and get the inverse
  setInverse <- function(inverse) {
    invmatrix <<- inverse
  }
  getInverse <- function() {
    invmatrix
  }
  # List of the methods
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## 2. This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##    If the inverse has already been calculated (and the matrix has not changed), 
##    then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Return the inverse matrix of matri x
  invmatrix <- x$getInverse()
  if (!is.null(invmatrix)) {
    message("getting cached matrix")
    return(invmatrix)
  }
  # Get the matrix
  mat <- x$get()
  invmatrix <- solve(mat) %*% mat
  x$setInverse(invmatrix)
  invmatrix
}
