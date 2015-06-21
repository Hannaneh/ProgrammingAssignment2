
# Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setine <- function(solve) m <<- solve
  getine <- function() m
  list(set = set, get = get,
       setine = setine,
       getine = getine)
  
}


## Compute the inverse of the special matrix returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  m <- x$getine()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setine(m)
  m
}
