## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  # Create a new variable to store the inverse and assign it as NULL whenever
  # we create a new cached matrix
  x.inverse <- NULL
  
  # Mutator method 
  set <- function(y){
      # setting matrix from parent frame with y and inverse of matrix with NULL
      x <<- y
      x.inverse <<- NULL
  }
  
  # Return the original matrix object
  get <- function() {
    x
  }
  
  # Mutator method to solve and set the inverse
  setinverse <- function(solve) {
    # solving the inverse using the object from the parent frame
    x.inverse <<- solve
  }
  
  # Return the inverse matrix object
  getinverse <- function() {
    x.inverse
  }
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x.inverse <- x$getinverse()
  
  if(!is.null(x.inverse)){
    message("getting cached data")
    return(x.inverse)
  }
  
  x.matrix.new <- x$get()
  x.inverse.new <- solve(x.matrix.new, ...)
  x$setinverse(x.inverse.new)
  x.inverse.new
}

m1 = matrix(c(1, 2, 3, 4), nrow=2, ncol=2) 
m2 = matrix(c(1, 2, 3, 4), nrow=2, ncol=2) 
cm1 <- makeCacheMatrix(m1)
print('cm1: Attempt 1.')
cacheSolve(cm1)
print('cm1: Attempt 2.')
cacheSolve(cm1) # solving again using cached matrix [should prompt 'getting cached data']
cm2 <- makeCacheMatrix(m2)
print('cm2: Attempt 1.')
cacheSolve(cm2) # should not prompt 'getting cached data'
print('cm2: Attempt 2.')
cacheSolve(cm2) # solving again using cached matrix [should prompt 'getting cached data'.]