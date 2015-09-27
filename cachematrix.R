## makeCacheMatrix()
##---------------------------------------------------------------------------
## The following function creates a "special" matrix, which is actually a list
## consisting of several functions which perform the following operations,
##
##  set()    - sets the value of the input matrix.
##  get()    - gets the value of the input matrix.
##  setinv() - sets the value of the matrix inverse.
##  getinv() - gets the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  
## Null any stored calculated inverse
  inv <- NULL
  
# Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
## Get value of stored matrix
  get <- function() x
  setinv <- function(a) inv <<- a
  getinv <- function() inv

## Save and return a list of the functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
main<-makeCacheMatrix

## cacheSolve()
##---------------------------------------------------------------------------

cacheSolve <- function(x, ...) {

## Gets the stored inverse of the matrix, if there is no stored value, the inverse is calculated and stored. 
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    
## Return a matrix that is the inverse of 'x'
    x$setinv(inv)
    inv
}
