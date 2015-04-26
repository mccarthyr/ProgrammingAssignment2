# The following functions allow for the creation of a square matrix 
# (assumed to be square for this assignment) 
# The makeCacheMatrix function can get and set the
# value of a matrix and set and get its cached inverse matrix value also.
# The cacheSolve function computes the inverse of the matrix returned by the
# makeCacheMatrix. If the inverse has already been calculated then it will use
# the cached copy that is stored in the makeCacheMatrix function.

# This function takes a square matrix (assumed to be square for this assignment)
# and sets the matrix locally, which is then accessible externally via the
# get() and set() functions. It also stores a cached version of the inverse of
# the matrix which is set via the setMatrixInverse() function.
# This functions returns a list of the functions it contains so they can be accessed.
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setMatrixInverse <- function(invMatrix) inverseMatrix <<- invMatrix
  getMatrixInverse <- function() inverseMatrix
  list(set = set, get = get, setMatrixInverse = setMatrixInverse, getMatrixInverse = getMatrixInverse)
}


# This function takes the returned result of the makeCacheMatrix() function and
# uses it to either return a cached copy of the inverse of the matrix supplied 
# to that makeCacheMatrix() function or else it will compute the inverse  of the matrix
# that the makeCacheMatrix() function contains and then cache it in the makeCacheMatrix() function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getMatrixInverse()
  if(!is.null(inverseMatrix)) {
    message("Getting the cached inverse matrix data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data)
  x$setMatrixInverse(inverseMatrix)
  inverseMatrix
}


