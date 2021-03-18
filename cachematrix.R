makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
  }
  get <- function() x
  setinvert <- function(inv) m <<- inv
  getinvert <- function() m
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}

cachesolve <- function(x) {
  m <- x$getinvert()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinvert(m)
  m
}

