makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- solfunction() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  print('checkpoint')
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheinverse <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv0
}
