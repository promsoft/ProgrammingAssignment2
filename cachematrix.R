## Matrix inversion is usually a costly computation and their
# may be some benefit to caching the inverse of a matrix
# rather than compute it repeatedly 
# So makeCacheMatrix creates a special "matrix" object that can cache its inverse
# and cacheSolve does compute and retrieve the inverse from the cache

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setinv <- function(mean) mi <<- mean
  getinv <- function() mi
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve 
## does retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  mi <- x$getinv()
  if(!is.null(mi)) {
    return(mi)
  }
  data <- x$get()
  mi <- solve(data, ...)
  x$setinv(mi)
  mi
}
