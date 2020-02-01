makeCacheMatrix <- function(x = matrix()) {
  #Creates a matrix that can cache its inverse
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  #Computes the inverse of matrix
  m <- x$getInverse()
  
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

m <- makeCacheMatrix(matrix(rnorm(8),2,2))
cacheSolve(m)
