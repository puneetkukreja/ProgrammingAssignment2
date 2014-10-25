## There are two functions in this file. 1) makeCacheMatrix creates a matrix that can be inversed. 
## 2) cacheSolve fetches the cached inverse value if available.

## makeCacheMatrix creates a matrix that can be cached

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}

## cacheSolve fetches the cached value if available else it caclualtes the matrix inverse
cacheSolve <- function(x, ...)  {
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
  }
  
