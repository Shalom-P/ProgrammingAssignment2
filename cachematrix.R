## makeCacheMatrix() takes in a matrix and creates a vector that contains 
## a list of functions then the cachesolve() matrix checks if the inverse of that
## matrix has been solved before and if it has then the function returns the already 
## existing data, if that matrix has not been calculated then it calculates and caches
## and returns the inverse of that matrix.

## the makeCacheMatrix function has four functions which is assigned to a vector
## set(set the value of the vector), 
## get(get the value of the vector),
## setinvmat(set the value of the inverse of the matrix),
## getinvmat(get the value of the inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmat <- function(solve) m <<- solve
  getinvmat <- function() m
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}


## The cacheSolve() function will check to see if the result already exists
## if it exists then it will get the result and skip the calculating part.
## but if the result doesnt exist then it will calculate the inverse of this matrix
## and return and cache that value.
cacheSolve <- function(x, ...) {
  m <- x$getinvmat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvmat(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
