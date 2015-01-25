## Provides two functions to create a special matrix object and another to perform work upon that 
## special matrix object in order to cache its inverse

## Creates a clojure object containing the supplied matrix, has internal variables for managing state of the 
## current matrix
## Note that changes to the matrix will reset the cached value, this handles changes to the matrix

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


## Takes the object returned from a makeCacheMatrix call and allows you to return either a cached inverse
## or to compute one if we haven't calculated it yet
## There is an important change here which is not to pass additional parameters to the solve function as its other
## paramters could modify output (see cacheSolve(t,b=M) as an example)

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
