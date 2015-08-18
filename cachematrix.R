## Functions for calculating and caching inversions of matrix

## Creates a special cacheable object for matrix

makeCacheMatrix <- function(x = matrix()) {
  # Check the input
  if (!is.matrix(x)) {
    stop("'x' must be a numeric matrix")
  }

  inverse <- NULL

  # Getter, setter for matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x

  # Getter, setter for inverse of matrix
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse

  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns an inverse of matrix created by makeCacheMatrix
## Caches the result on the first call

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  # Check if already cached
  if (!is.null(inverse)) {
    message("Getting cached inverse of matrix")
    return(inverse)
  }
  # Or calculate inverse of matrix
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
