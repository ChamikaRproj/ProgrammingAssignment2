## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special “matrix” object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { ## This defines the argument with default mode of "matrix"
  inv <- NULL  ## this line initialize inv as NULL; will hold value of matrix inverse
  set <- function(y){ ## defines the set function to assign new value of matrix in parent environment. if there is a new matrix, reset inv to NULL
    x <<- y
    inv <<- NULL
  }
  get <- function() {x} ## this defines the get fucntion - returns value of the matrix argument
  setInverse <- function(solveMatrix) {inv <<- solveMatrix} ## this assigns value of inv in parent environment
  getInverse <- function() {inv} ## this gets the value of inv where called
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ## you need this in order to refer to the functions with the $ operator
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matr <- x$get()
  inv <- solve(matr, ...)
  x$setInverse(inv)
  inv
}
