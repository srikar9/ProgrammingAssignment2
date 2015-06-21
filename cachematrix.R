## Put comments here that give an overall description of what your
## functions do

## The below function creates a special kind of matrix object that can cache its Inverse

makeCacheMatrix <- function(x = matrix()) {
# Initializing the Inverse Matrix
  inv<- NULL
# Method to set the Matrix
  set<- function(y){
    matrix<<- y
    inv<<- NULL
  }
# Method to get the Matrix
  get<- function(){
    # returns the matrix
        matrix
  }
## Method to set the Inverse of the Matrix
   setInverse<- function(inverse){
  # storing the inverse
      inv<<- inverse
   }
## Method to get the Inverse of the Matrix
   getInverse<- function(){
     # Returns the inverse
     inv
   }
## Returns the list of methods
   list(set=set,get=get,
        setInverse=setInverse,
        getInverse=getInverse)
}


## Write a short comment describing this function
## The below function computes the inverse of the special matrix returned by the "makeCacheMatrix" function. 
## If the inverse has already been calculated (and the matrix has not changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## getting a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  ## returns if the inverse has already been calculated (i.e. if !is.null(m)==TRUE)
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## If the inverse wasn't yet been calculated ##
  
  
  ## getting the matrix from our object
  data <- x$get()
  
  ## calculating the inverse by using matrix multiplication
  m <- solve(data) %*% data
  
  ## storing the inverse to the object for future usage
  x$setInverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m 
}

