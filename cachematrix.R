## The code for these functions are an adaptation of the functions provided
## in the makeVector example for the programming assignment.
## 
## makeCacheMatrix() - Function to create a special list object that can store a matrix and its
## inverse and exposes methods to get and set each.
##
## cacheSolve() - Function to check the special list object for an existing inverse
## matrix and solve for it and set it if it does not exist.


## Usage Exmaple:
##
## newMatrix<-matrix(c(1,0,8,2,7,4,0,4,1),nrow=3,ncol=3)
##
## myCacheMatrix<-makeCacheMatrix(newMatrix)
##
## cacheSolve(myCacheMatrix)  ## calculates, sets and returns the inverse
## cacheSolve(myCacheMatrix)  ## finds the cached inverse and returns it


## Create a special list object to store and retrieve a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {

  ## Create NULL inverse on object creation
  xInverse<- NULL

  ## Set the matrix, assign NULL to inverse when new matrix is set
  setMatrix <- function(y){
    x <<- y
    xInverse <<- NULL
  }
  
  ## Return a stored matrix
  getMatrix <- function() x

  ## Store the solved inverse as xInverse
  setInverse <- function(solve) xInverse <<- solve
  
  ## Return the stored inverse
  getInverse <- function() xInverse
  
  ## Return list object with getter and setter methods
  list(setMatrix=setMatrix,
       getMatrix=getMatrix,
       setInverse=setInverse,
       getInverse=getInverse)
}


## Checks a makeCacheMatrix object and returns a cached inverse
## if it has been previously calculated, and if not, calculates
## the inverse of the matrix using "solve" and caches it back in the list object.

cacheSolve <- function(x, ...) {

  ## Get the Inverse from the special matrix object
  xInverse <- x$getInverse()
  
  ## If xInverse is not null, it has been cached.  Note that and return inverse.
  if(!is.null(xInverse)) {
    message("Getting cached inverse.")
    return(xInverse)
  }
  
  ## If xInverse was NULL, get the matrix from the object passed to the function
  data <- x$getMatrix()
  
  ## Get inverse of matrix using "solve" function
  xInverse <- solve(data, ...)
  
  ## Cache the inverse in the special object for future use
  x$setInverse(xInverse)
  
  ## Return the inverse of the matrix x
  xInverse

}
