## Coursera assignment #2.

##Creates a cachable matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { ## set function passes x to the parent environment and sets inv to NULL
    x <<- y
    inv <<- NULL
  }
  get <- function() x ## returns x
  setinverse <- function(solve) inv <<- solve ## passes the solved matrix to the inv variable and places it in the parent environment
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


##Returns the inverse of a cached matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse() ## calls the getinverse function passed
        if(!is.null(inv)){ ## if inv already has a value, it reports the cached version
          message("getting cached inverse")
          return(inv)
        }
      data <- x$get() ## if there is no cached version, the value of the matrix is passed to the data function
      inv <- solve(data,...) ## the inverse is calculated using the solve function
      x$setinverse(inv) ## the setinverse function is used to cache the result for future calls
      inv ## the inverse matrix is returned
}
