## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## creates a matrix object that can cache its inverse
  
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get_data <- function()
  x
  set_inverse = function(inverse) 
  m <<- inverse
  get_inverse = function() 
  m
  
  list(set = set, get_data = get_data, 
       set_inverse = set_inverse, 
       get_inverse = get_inverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## computes the inverse of the special "matrix".If the inverse has
  ##already been calculated (and the matrix has not changed), then
  ## `cacheSolve` should retrieve the inverse from the cache.
  
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("Getting cached data (already stored)")
    return(m)
  }
  
  data <- x$get_data()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
    
}
