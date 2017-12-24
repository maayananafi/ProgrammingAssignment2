## These functions cache the value of the matrix and its inverse, and then returns the inverse if it cached and computes it if it is not cached.

## This function sets and gets the value of the matrix, and sets and gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  setMatrix <- function(y) {
    x <<- y
    im <<- NULL
  }
  getMatrix <- function() x
  setinverse <- function(inverse) im <<- inverse
  getinverse <- function () im
  list(setMatrix=setMatrix,
       getMatrix=getMatrix,
       setinverse=setinverse,
       getinverse=getinverse)
}


## This function returns the inverse matrix. It uses ifelse to first determine if there is a cached inverse matrix. If there is no cached inverse matrix (that is, if im is null), it computes the inverse.

cacheSolve <- function(x, ...) {
  im <- x$getinverse()
  if(!is.null(im)) {
    message("Printing cached inverse matrix")
    return(im)
  }
  data <- x$getMatrix()
  i <- solve(data,...)
  x$setinverse(i)
  i
}

## Testing

my_matrix <- matrix(1:4,2)
solve(my_matrix)
my_matrix1 <- makeCacheMatrix(my_matrix)
cacheSolve(my_matrix1)
identical(solve(my_matrix),cacheSolve(my_matrix1))
