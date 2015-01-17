## Fxns create a special "matrix" object that can cache its inverse
## Four "work" fxns cache or retrieve values of matrix and its inverse
## Does not work in matrix input into makeCacheMatrix is changing


## makeCacheMatrix creates a list of fxns that will set matrix value,
## get matrix value, cache matrix inverse, and get matrix inverse
## default argument is a 1-by-1 empty matrix

makeCacheMatrix <- function(x = matrix()) {
  
  #preset value of the inverse to be NULL
  inv <- NULL
  
  #set the matrix value
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #get the matrix value
  get <- function() x
  
  #cache the value of the matrix inverse with anonymous function 
  setinv <- function(inverse) inv <<- inverse
  
  #get the value of the matrix inverse
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve uses the fxns returned by makeCacheMatrix to output inv matrix of 'x'
#x must be an output of makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  #get the matrix
  mat <- x$get()
  
  #get the inverse
  inv <- x$getinv()
  
  #if inv is already calculated and matrix has not changed then get inv from cache
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  #otherwise use solve() to calculate inverse of matrix 
  inv <- solve(mat, ...)
  
  #cache value of inverse
  x$setinv(inv)
  
  #output inverse to console
  inv
}
