makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}



m <- matrix(c(2, 3, 3, 2), nrow = 2, ncol = 2)
mCache <- makeCacheMatrix(m)
cacheSolve(mCache)
cacheSolve(mCache)


m2 <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
nCache2 <- makeCacheMatrix(m2)
cacheSolve(nCache2)
cacheSolve(nCache2)


