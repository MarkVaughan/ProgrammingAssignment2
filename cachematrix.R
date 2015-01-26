## Put comments here that give an overall description of what your
## functions do
## The main goal of this script is to perform matrrix inversion. This can
## be computationally intensive so this implements caching of the inverse
## of the matrix so that the calculation does not have to be repeated

## Write a short comment describing this function

## This function will specify a create a square matrix which will be used
## to calculate the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
## m will be the inverse matrix cache  
  m <- NULL

## The set() function is to change the value of the matrix on 
## which to calculate the inverse without having to reassign the 
## function to the 
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinversemat <-function(solve) m <<-solve  
  getinversermat <- function() m
  list(set = set, get = get, setinversemat = setinversemat, getinversemat = getinversemat)
}


## Write a short comment describing this function
##
## This will calculate the inverse of the matrix unless it has not already
## been solved, if it has the result will be retrieved from the cached 
## result
## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
          m <- x $getinversemat()
  
## need to check if a cached inverse matrix exists i.e m is not NULL
          if (!is.null(m)){
              message("getting cached data")
              return(m)
          }
## Otherwise calculate the inverse using the solve function and return result as m
        matrixdat   <- x$get()
        m  <- solve(matrixdat, ...)
        x[setinversemat(m)]
        m
      
}
