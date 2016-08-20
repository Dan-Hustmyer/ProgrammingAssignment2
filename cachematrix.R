## Defines two functions that work togeter to create an R object that 
## wiil create a matrix that will calculate its inverse and store it. 
## If it has already been calculated the object will returned the stored inverse matrix



## Example usage.
## cachedmatrix <- makeCacheMatrix(matrix(c(1,-1/4,-1/4,1), nrow = 2, ncol = 2))
## cacheSolve(cachedmatrix)


## 1. makeCacheMatrix creates and returns an R object that contains
## the matrix and the methods needed to instantiate itself as well as
## its accessor functions.

makeCacheMatrix <- function(x = matrix()) {
	## initializer
      im <- NULL
      set <- function(y) {
            x <<- y
            im <<- NULL
      }

	## Accessors	
      get <- function() x
      setinverse <- function(solve) im <<- solve
      getinverse <- function() im

	## named list of the accessors allows methods to be accessed using $.
      list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## 2. cacheSolve returns the inverse of the matrix passed to it.
##    note: you must pass a matrix of type created by makeCacheMatrix
##    or you will get an error.

## For cacheSolve to work the matrix passed to must have been 
## created by makeCacheMatrix and must be a square numeric or complex matrix.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      im <- x$getinverse()
      if(!is.null(im)) {
            message("getting cached inverse matrix")
            return(im)
      }

	## if the inverse has not been calculated get the matrix and calc the inverse
      data <- x$get()
      im <- solve(data, ...)
      x$setinverse(im)
      im
}
