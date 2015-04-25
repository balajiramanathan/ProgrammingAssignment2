## This is a set of two functions that take a matrix and will calculate and
## cache the inverse of the matrix.  If the cacheinverse() function is
## called with the same matrix after the inverse has already been calculated
## the cached copy of the inverse is returned rather than recalculating it.

## makeCacheMatrix contains functions to get and set the original matrix
## and the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes a matrix x, and returns its inverse.  If the inverse
## has already been calculated and cached (m is not a null matrix), then
## m is returned, otherwise, m is calculated and stored before being returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
