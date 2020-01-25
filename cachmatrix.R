## Coursera assignment #2.

##Creates a cachable matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


##Returns the inverse of a cached matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
          message("getting cached inverse")
          return(inv)
        }
      data <- x$get()
      inv <- solve(data,...)
      x$setinverse(inv)
      inv
}