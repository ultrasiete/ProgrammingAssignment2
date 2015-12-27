## Assignment2: Caching the inverse of a Matrix

## This function creates a special "matrix" object that can cached its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set, get=get, setInverse = setInverse, getInverse=getInverse)
}


## This function computes the inverse of a matrix object returned by makeCacheMatrix.
## If the inverse has already been calculated this method will return the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if ( !is.null(inv)){
    message("getting inverse from cache")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
