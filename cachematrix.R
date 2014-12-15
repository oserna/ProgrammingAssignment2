# Creates a new type of matrix, a kind of wrapper around the normal type, whose
# new capability is to cache its inverse. 

makeCacheMatrix <- function(matrixToCache = matrix()) {
  
  #stores the calculated inverse of the matrix
  inverse <- NULL
  
  #set a new matrix 
  set <- function(newMatrixToCache) {
    matrixToCache <<- newMatrixToCache
    inverse <<- NULL
  }
  
  #get the original matrix
  get <- function() {
    matrixToCache
  }
  
  #stores the inverse of the matrix
  setInverse <- function(inverse) {
    inverse <<- inverse
  }
  
  getInverse <- function() {
    inverse
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


cacheSolve <- function(inverseCacheableMatrix, ...) {
  
  matrixInverse <- inverseCacheableMatrix$getInverse()
  
  # If the inverse has already computed, just only return it
  if(!is.null(matrixInverse)) {
    message("getting inverse cached data")
    return(matrixInverse)
  }
  
  # if not, we have to calculate the inverse and cache it for future uses
  originalMatrix <- inverseCacheableMatrix$get()
  inverse <- solve(originalMatrix, ...)
  inverseCacheableMatrix$setInverse(inverse)
  inverse
}
