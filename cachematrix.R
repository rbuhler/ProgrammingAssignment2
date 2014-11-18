# The folowing functions implemnt the feature of caching
# results of the execution of a function. Time consuming
# executions may be abreviated with the recovery of results
# from already executed calculations.

# The function makeCacheMatrix creates a special "vector", wich
# stores a matrix and a list of functions, as decribed below:
# 1. set - sets a matrix to the vector and clear the cache;
# 2. get - gets the current matrix;
# 3. getsolve - gets the cached execution result;
# 4. setsolve - sets the cache with an execution result.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
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


# The function cacheSolve calculates the inverse matrix of
# the matrix stored into the special "vector" makeCacheMatrix.
# However, if first checks in the "vector" for an already
# calcualted result, if so this is the returned result. 
# Otherwise the inverse matrix is calculated and the result
# is stored into the "vector" and returned as the function
# execution result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}