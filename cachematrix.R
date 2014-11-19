# The folowing functions implemnt the feature of caching
# results of the execution of a function. Time consuming
# executions may be abreviated with the recovery of results
# from already executed calculations.

# The function makeCacheMatrix creates a special "vector", wich
# stores an informed matrix and a list of functions, as decribed below:
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

# The function cacheSolve returns the inverse matrix of
# an informed matrix on the special "vector" of makeCacheMatrix.
# If a calculation is already executed the return is the 
# cached result, otherwise the inverse matrix is calculated and
# the result stored into the cache for future requests that may
# apply, the function then returns this result.
cacheSolve <- function(x, ...) {
  # Check for an already calculated result
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Otherwise executes the calculation
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}