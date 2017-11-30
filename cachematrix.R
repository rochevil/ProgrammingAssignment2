## cachematrix.R has 2 main functions makeCacheMatrix
## and cacheSolve. It solves the inverse of a matrix
## if the entered matrix is new, and fetch it from
## the cache if it was already solved before.

## makeCacheMatrix accepts a matrix x (defaults to an
## empty matrix if no value is entered), and returns a
## list with names get.x (input matrix), set.inverse
## (setter of the inverse matrix, no initial value),
## get.inverse (getter of the inverse matrix, initial
## value is an empty matrix).

makeCacheMatrix <- function(x = matrix()) {
  inverse.matrix <- matrix()
  get.x <- function()
    x
  set.inverse <- function(inverse)
    inverse.matrix <<- inverse
  get.inverse <- function()
    inverse.matrix
  list (get.x = get.x, set.inverse = set.inverse,
        get.inverse = get.inverse)
}


## cacheSolve accepts argument x (result of makeCacheMatrix)
## and checks if the inverse matrix is already solved.
## If already solved, it outputs an indicator message
## and the cached value. If not, it solves for the
## inverse matrix and store it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse.matrix <- x$get.inverse()
  if(!any(is.na(inverse.matrix))) {
    message("Fetching cached inverse matrix")
    return(inverse.matrix)
  }
  input.matrix <- x$get.x()
  inverse.matrix <- solve(input.matrix,...)
  x$set.inverse(inverse.matrix)
  inverse.matrix
  }
