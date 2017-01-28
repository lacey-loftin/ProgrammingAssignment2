## The following two functions will calculate the inverse matrix or retrieve the inverse matrix 
## from the cache.

## This function, makeCasheMatrix, creates a special “matrix” object that can cache its inverse. 
## makeCacheMatrix contains 4 functions: set, get, setmean, getmean.
## (1)get is a function that returns the vector x stored in the main function.
## (2)set is a function that changes the vector stored in the main function.
## (3)setmean and getmean are functions very similar to set and get.
## (4)They don’t calculate the mean, they simply store the value of the input in a variable m.
## (5)into the main function makeVector (setmean) and return it (getmean).

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
  
