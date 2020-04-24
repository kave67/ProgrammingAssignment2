## Put comments here that give an overall description of what your
## functions do
## the follwoing function creates matrix object which would cache its inverse
makeCacheMatrix <- function(x = matrix()) {
   inver <- NULL      ## initialize inver as NULL; will hold value of matrix inverse               
   set <- function (y) { ## define the set function to assign new
    x <<- y        ## value of matrix in parent environment
    inver <<- NULL  ## if there is a new matrix, reset inver to NULL
   }
   
   get <- function()x   ## this get fucntion - returns value of the matrix argument
   setinverse <- function(inverse) inver <<- inverse  ## assigns value of inv in parent environment
   getinverse <- function() inver  ## gets the value of inv where called
   list(set= set, get= get, setinverse =setinverse, getinverse= getinverse)
}inverse
## matrix inverse
## The following function computes the inverse of the matrix returned by the above function
## The inverse is computed already then the following fucntion just pulls it from the cache

caceSolve <- function(x,...){  ## returns a matrix which is the inverse of x
   inver <- x$getinverse()
   if (!is.null(inver)){
     message("retrieving from the cache")
     return(inver)
   }
   
   data <- x$get()
   inver <- solve(data,...)
   x$setinverse(inver)
   inver
}
   
     
   