## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(our_matrix = matrix()) {
  computed_inverse <- NULL
  
  set <- function(new_matrix) {
    our_matrix <<- new_matrix #Update our matrix to the new one
    computer_inverse <<- NULL #Clear the cached value as the matrix has changed (so we recompute it)
  }
  
  get <- function() our_matrix
  
  setInverse <- function(inverted_matrix) computed_inverse <<- inverted_matrix
  
  getInverse <- function() computed_inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data) #Not using ... here we need to force defaults otherwise we can get different answers for the same input matrix!
  x$setInverse(inverse)
  inverse
}
