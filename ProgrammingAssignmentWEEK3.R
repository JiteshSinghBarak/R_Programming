## [Put comments here that describe what your functions do]


makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  ## set the matrix
  setMatrix <- function(y){
    x <<- y
    inverseMatrix <- NULL
  }
  
  ##get the matrix
  getMatrix <- function() x
  
  ## cache the inverse
  setInverse <- function(inverse) inverseMatrix <<- inverse
  
  ## return the saved inverse
  getInverse <- function() inverseMatrix
  
  list(setMatrix = setMatrix , getMatrix = getMatrix , getInverse = getInverse , setInverse = setInverse)
}


cacheSolve <- function(x, ...) {
  ## get the saved inverse
  inverseMatrix <- x$getInverse()
  ## if there is saved inverse return it
  if(!is.null(inverseMatrix)){
    message("Getting the saved inverse")
    return(inverseMatrix)
  }
  ##if there is no saved inverse compute it , save it and return it
  matrix <- x$getMatrix()
  inverseMatrix <- solve(matrix , ...)
  x$setInverse(inverseMatrix)
  return(inverseMatrix)
}