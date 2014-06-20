# makeCacheMatrix: return a list of functions as below
# set: set the value of the matrix
# get: get the value of the matrix
# setinv : set the value of the inverse
# getinv : get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  # inv will store the cached inverse matrix
  inv <- NULL
  
  # Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Get the matrix
  get <- function() x
  
  # Set the inverse
  setinv <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getinv <- function() inv
  
  # Return the matrix with defined functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: Compute the inverse of the matrix.
# if inv already calculated and set, the cached inverse is returned.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # If the inverse is cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # The inverse is not cached, calculate it
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setinv(inv)
  
  # Return it
  inv
}