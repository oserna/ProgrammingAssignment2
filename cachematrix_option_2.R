# Version 2
# 
# Creates a new type of matrix, a kind of wrapper around the normal type, whose
# new capability is to cache its inverse. 
#
# This cache is a little bit diferent because the inverse of the matrix is computed
# within the getInverse() function. From the client function perspective get the inverse
# is easier.
# 
# The final result maybe is more elegant
# 

inverseCacheableMatrix <- function(matrixToCache = matrix()) {
  
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
    
  getInverse <- function() {
    if(is.null(inverse)) {
      inverse <<- solve(matrixToCache)
      message("matrix inverse computed and cached")
    }
    inverse
  }
  
  list(set = set, get = get, getInverse = getInverse)
  
}


# Only the first time the inverse is calculated, as you can see the trace
# shows when the inverse is calculated
seeHowItWorks <- function(normalMatrix, ...) {

  # the inverse is going to be calculated
  decoratedMatrix <- inverseCacheableMatrix(normalMatrix)  
  print(decoratedMatrix$getInverse())

  # the inverse is already computed so it will be retrieved from cache
  print(decoratedMatrix$getInverse())
 

  
}
