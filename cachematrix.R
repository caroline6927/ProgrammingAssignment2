##cache a matrix and check if there is a cache if the new matrix is the same as the old one. 
##retrieve if yes and compute inverse if no.

## Set a list of functions to get and set a matrix and get and set the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInver <- function(Inver) m <<- Inver
  getInver <- function() m
  list(set = set, get = get,
       setInver = setInver,
       getInver = getInver)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getInver()
  if(!is.null(m) & identical(x,m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInver(m)
  m
}
