## This functions allow the creation of a special matrix object that caches its
## inverse when computed, so the next time computing it won't consume as much
## time as the first time (if the matrix hasn't changed).


## makeCacheMatrix returns a "cacheable" matrix object with the methods:
## get, set, getinverse and setinverse.
## getinverse returns the cached inverse.
## If the matrix is changed (by the method set) the cache is deleted, so it has
## to be computed again for the new matrix, that way, it won't return an old and
## wrong cached inverse.

## it takes as a parameter the original matrix to make "cacheable".

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(newInverse) inverse <<- newInverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of a "cacheable" matrix object.
## It first checks if the inverse has been already computed and if so, it
## returns that cached inverse.
## If the inverse is not available, as when the matrix has been just created
## and the cache has never been computed, or when the matrix has changed and the
## inverse has been deleted, cacheSolve computes the new inverse and stores it
## in the cache of the "cacheable" matrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}