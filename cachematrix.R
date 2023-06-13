makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- solfunction() x
  set_Inv <- function(inverse) inv <<- inverse
  get_Inv <- function() inv
  list(set = set,
       get = get,
       set_Inv = set_Inv,
       get_Inv = get_Inv)
}


cacheinverse <- function(x, ...) {
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$set_inv(inv)
  inv0
}
