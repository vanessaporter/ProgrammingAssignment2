## Cache and retrieve an inversed matrix

## This function creates an empty matrix and caches its inverse 

makeCacheMatrix <- function(x = matrix(), ...) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(mean) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function will retrieve the cached inverse matrix m created above, and it hasn't been inversed yet, inverse the matrix

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inv(data, ...)
  x$setinv(m)
  m
}
