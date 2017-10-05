#This set of functions can create a matrix object that can cache 
#its inverse and either compute this inverse or retrieve the inverse 
#from the cache if it has already been computed.

#This function creates the matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#This function either computes the inverse or retrieves the inverse 
#from the cache if it has already been computed.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setinverse(inverse)
  inverse
}
