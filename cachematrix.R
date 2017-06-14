## Put comments here that give an overall description of what your
## functions do
## The following functions are used to create a special object that stores a matrix and caches its inverse. 
## The first function, makeCacheMatrix creates a special “matrix”, which is a function that has the job of:
##settting the value of the matrix

##gettting the value of the matrix

##setting the value of the inverse

##gettting the value of the inverse
 
makeCacheMatrix <- function(x = matrix()) {
nv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special “matrix” returned by the function above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
