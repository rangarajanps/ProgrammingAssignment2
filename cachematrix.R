## Input is always assumed to be a square invertible matrix

## creates a special "matrix" object that can cache a matrix object

makeCacheMatrix  <- function(x = matrix()) {
  
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



## Returns the inverse of the matrix from cache if available
## otherwise calculates and returns

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  m <- solve(x$get(), ...)
  x$setinverse(m)
  m      
}
