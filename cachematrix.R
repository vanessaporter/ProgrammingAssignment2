## Cache and retrieve an inversed matrix

## This function creates an empty matrix and caches its inverse 

makeCacheMatrix <- function(x = matrix(), ...) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will retrieve the cached inverse matrix i created above, and it hasn't been inversed yet, inverse the matrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- inv(data, ...)
  x$setinverse(i)
  i
}
