## The pair of functions below calculate and cache the inverse of a matrix,
## storing the result in a special matrix object which has custom methods
## for setting and getting it's inverse

##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mInverse <- NULL
  set <- function(y) {
    x <<- y
    mInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) mInverse <<- inverse
  getInverse <- function() mInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  mInverse <- x$getInverse()
  if(!is.null(mInverse)) {
    message("getting cached data")
    return(mInverse)
  }
  xMatrix <- x$get()
  mInverse <- solve(xMatrix, ...)
  x$setInverse(mInverse)
  mInverse
}
