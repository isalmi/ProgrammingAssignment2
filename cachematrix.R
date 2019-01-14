## isalmi Coursera Data Science Programming Assignment 2

## Calculate and cache the inverse of a square matrix
## Create a makeCacheMatrix() object with a matrix x as its argument
## Call cacheSolve() on this mCM to calculate the inverse of x or retrieve the cached inverse

## Requires a matrix x
## Creates a set of functions to cache and retrieve x and its inverse
## Returns a named list of the functions
makeCacheMatrix <- function(x = matrix()) {
  ## instantiate the inverse as NULL
  inv <- NULL
  
  ## set() resets the vector object for this cache in the parent 
  ## environment and resets the inverse to NULL
  set<- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ## get() retrieves the vector from the parent environment
  get <- function() x
  
  ## setinvers() sets the parent enviroment's inv variable to the
  ## input matrix
  setinverse <- function(inverse) inv <<- inverse
  
  ## getinverse() retrieves cached inverse matrix from the parent envi
  getinverse <- function() inv
  
  ## return a list of named function objects
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## Requires a makeCacheMatrix object x
## Returns the inverse of the matrix that is cached in x
cacheSolve <- function(x, ...) {
  
  ## retrieve the cached inverse from the mCM object
  inv <- x$getinverse()
  
  ## if inv is not null, return the inverse matrix
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  ## if inv is null, calculate the inverse of the matrix cached in 
  ## the mCM object, cache that inverse, and return it
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
