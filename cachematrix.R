## The following functions work together to find and store the inverse of 
## matrices. Resources are conserved by storing inverses that have already 
## been calculated.

## The makeCacheMatrix creates a special "matrix" object that can cache its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinv <- function(solve) i <<- solve
      getinv <- function() i
      list(set =set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## The cacheSolve function computes the inverse of the special "matrix"
## returned by makeCacheMatrix, or if the inverse has already been calculated
## it retrieves the cached result.

cacheSolve <- function(x, ...) {
      i <- x$getinv()
      if(!is.null(i)) {
          message("getting cached data")
          return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i
}
