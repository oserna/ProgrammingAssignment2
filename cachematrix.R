# Creates a new type of matrix, a kind of wrapper around the normal type, whose
# new capability is to cache its inverse. The current approach for using it:
#
# -> the client of the makeCacheMatrix object has to calculate the inverse
# -> once the inverse has been calculated, it has to be set with setInverse() function
# -> retrieve the inverse when it´s needed
#
# IMHO this approach could be even better if the inverse of the matrix were calculated
# within the function getInverse(). This function should compute the inverse of the matrix
# the first time you call getInverse() and later every time you call getInverse() you
# will receive the pre-computed value instead of calculate it. In order to show this
# enhancement I´ve included a new file in this repo: cachematrix_option_2.R. 
#
# Also I´ve include unit test not only for cachematrix.R but also for cachematrix_option_2.R
#
makeCacheMatrix <- function(matrixToCache = matrix()) {
  
  # Stores the calculated inverse of the matrix
  inverse <- NULL
  
  # Set a new matrix whose inverse will be cacheable
  # Also reset the previous inverse, if exists
  set <- function(newMatrixToCache) {
    matrixToCache <<- newMatrixToCache
    inverse <<- NULL
  }
  
  # Get the original matrix
  get <- function() {
    matrixToCache
  }
  
  # Stores the inverse of the matrix.  
  setInverse <- function(inverse) {
    inverse <<- inverse
  }
  
  # Get the stored inverse
  getInverse <- function() {
    inverse
  }
  
  # Describes the wrapper
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


# For a given matrix wrapper returns its inverse
cacheSolve <- function(inverseCacheableMatrix, ...) {
  
  # Get the inverse of the given matrix 
  matrixInverse <- inverseCacheableMatrix$getInverse()
  
  # If the inverse has already computed, just only return it
  if(!is.null(matrixInverse)) {
    message("getting inverse cached data")
    return(matrixInverse)
  }
  
  # If not, we have to calculate the inverse and cache it for future uses
  
  # get the original matrix
  originalMatrix <- inverseCacheableMatrix$get()
  
  # calculate its inverse
  inverse <- solve(originalMatrix, ...)
  
  # Stores the calculated inverse in order not to calculate it in later calls
  inverseCacheableMatrix$setInverse(inverse)
  
  # returns the inverse
  inverse
}
