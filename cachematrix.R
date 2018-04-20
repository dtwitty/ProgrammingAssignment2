## These functions cache the inverse of a matrix.

## This function makes an object that can be used to cache the inverse of a matrix.
## We call this object a CacheMatrix.
makeCacheMatrix <- function(x = matrix()) {
  # This will always either be NULL or the inverse of x.
  # This invariant will always hold, even if x is updated.
  inverse <- NULL
  
  set.matrix <- function(y) {
    # Update the stored matrix.
    x <<- y
    # The inverse of x is now unknown, so clear the cached inverse.
    inverse <<- NULL
  }
  
  # This function updates the inverse of x.
  # It is assumed that the i is actually the inverse of x.
  set.inverse <- function(i) inverse <<- i
  
  get.matrix <- function() x
  get.inverse <- function() inverse
  
  list(
    set.matrix = set.matrix,
    get.matrix = get.matrix,
    set.inverse = set.inverse,
    get.inverse = get.inverse
  )
}

## This function takes in a CacheMatrix and returns its mean.
cacheSolve <- function(x, ...) {
  # Check whether the cached inverse exists.
  if (is.null(x$get.inverse())) {
    # If there is no cached inverse, update the cache.
    print("Calculating the inverse!")
    
    inverse <- solve(x$get.matrix(), ...)
    x$set.inverse(inverse)
  } else {
    print("Using the cached inverse!")
  }
  
  # By this point, x is guaranteed to have a cached inverse.
  x$get.inverse()
}