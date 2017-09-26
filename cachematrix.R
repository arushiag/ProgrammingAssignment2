## Put comments here that give an overall description of what your
## functions do
## These functions written in partial fulfillment of Coursera Data Science: R Programming 
## Week 3 Assignment; week beginning September 26, 2017; GitHub user: arushiag
## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL    ## initialize invMat as NULL; will hold value of matrix inverse 
  set <- function(y) { ## define the set function to assign new 
    x <<- y        
    invMat <<- NULL
  }
  get <- function() x ## define the get fucntion - returns value of the matrix argument
  setInv <- function(inverse) invMat <<- inverse ## assigns value of invMat in parent environment
  getInv <- function() invMat   ## gets the value of inv where called
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv) ## you need this in order to refer to the functions with the $ operat
}

## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMat <- x$getInv()
  if(!is.null(invMat)) {
      message("Getting Cached Invertible Matrix")
      return(invMat)
  }
  data <- x$get()
  invMat <- solve(data)
  x$setInv(invMat)
  invMat
}
