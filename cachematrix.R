# Programming Assignment 2: Lexical Scoping

# Returns a list of functions to make a «cached» matrix.
# set        - set the matrix
# get        - get the matrix
# setinverse - set the inverse matrix
# getinverse - get the inverse matrix
makeCacheMatrix <- function(a_matrix = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    a_matrix <<- y
    inv_matrix <<- NULL
  }
  get <- function() a_matrix
  setinverse <- function(inverse) inv_matrix <<- inverse
  getinverse <- function() inv_matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# Returns the inverse of a matrix. The inverse matrix could be cached.
cacheSolve <- function(x, ...) {
  inv_matrix <- x$getinverse()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data, ...)
  x$setinverse(inv_matrix)
  inv_matrix
}
