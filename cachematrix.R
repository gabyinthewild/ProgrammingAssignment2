## The assignment is to write a pair of functions that cache the inverse of a matrix.
#
#
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) n <<- inverse
  getinverse <- function() n
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


# comment


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  n <- x$getinverse()
  if (!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinverse(n)
  n      ## Return a matrix that is the inverse of 'x'
}