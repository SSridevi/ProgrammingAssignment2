## Caching the inverse of a square matrix and 
## retrieving it from the Cache or computing the inverse if not found in the cache


## Creates the special matrix object that caches it's inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve function retrieves the inverse of the square matrix cached and returned 
## by the function makeCacheMatrix or
## computes the inverse of the matrix if not cached yet

cacheSolve <- function(x, ...) {
  ## Get the cached matrix that is the inverse of 'x'
  m <- x$getsolve() 
  if(!is.null(m)) 
  {
    message("getting cached data")
    ## Return the cached matrix that is the inverse of 'x'
    return(m) 
  }
  data <- x$get()
  ## Compute the inverse of the matrix 'x'
  m <- solve(data, ...)
  x$setsolve(m)
  ## Return a matrix that is the inverse of 'x'
  m
  
}