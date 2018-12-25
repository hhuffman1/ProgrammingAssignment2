## Generally speaking, these functions together take a matrix x, and solve for its inverse.
## The function makeCacheMatrix creates a special matrix object that can cache the inverse of 
## x, and can be used as an argument for
## the function cachesolve, which computes the inverse of x. If x has been previously been passed to
## cachesolve, then the inverse of x is recalled from the cache, 
## instead of being computed a second time.

## makeCacheMatrix takes one argument, a matrix x, and creates a "special" matrix object that can
## cache the inverse of x.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cachesolve takes an aforementioned "special" matrix object created by 
## makeCacheMatrix x, and computes its inverse using the solve() function within. 
## If the inverse of x has been previously calculated by cachesolve, the 
## inverse will be recalled from the cache, instead of being computed a second time.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mtx <- x$get()
  inv <- solve(mtx, ...)
  x$setInverse(inv)
  inv
}