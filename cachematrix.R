## Put comments here that give an overall description of what your
## functions do
## This function create a object it's mean makeCacheMatrix create
## a special vector which is really a list containing function to
## 1. Set the matrix
## 2. get the matrix
## 3. Set the inverse matrix
## 4. Get the inverse matrix

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  ## Inicialization of matrix
  m <- NULL
  ## Set matrix
  set <- function(y) {
          x <<- y
          m <<- NULL ## Inicialization of matrix again
  }
  ## Get the matrix
  get <- function() x
  ## Set inverse matrix
  setInverse <- function(inverse) m <<- inverse  
  ## Get inverse matrix
  getInverse <- function() m
  ## List of functions to this object
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## cacheSolve check if matrix is inverse, in case it is true
## return matrix
## otherwise inverse the matrix
cacheSolve <- function(x, ...) {
        m <- x$getInverse() ## get inverse matrix 
        
        if( !is.null(m) ) { ## Case inverse matrix exist
          message("getting cached inverse matrix")
          return(m) ## return inverse matrix
        }
        ## Case inverse matrix is not calculated
        matrix <- x$get() ## Get matrix 
        m <- solve(matrix) ## calculate inverse matrix with solve function
        ## Return a matrix that is the inverse of 'x'
        m
}

##EXAMPLES 
## testData <- matrix(c(1,2,3, 0,1,4, 5,6,0), nrow = 3, ncol = 3)
## Cache <- makeCacheMatrix(testData)
## Inverse <- cacheSolve(Cache)
