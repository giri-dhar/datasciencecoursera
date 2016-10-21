## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## function to create matrix object
makeCacheMatrix <- function(x = matrix()) {
  cacheInv <- NULL ## initialize
  set <- function(usrValue = matrix()) {
    x <<- usrValue 
    cacheInv <<- NULL
  }
  get <- function() x
  
  ##set inverse variable in parent env to desired value and return the value as a convenience
  setInvrs <- function(inverse) {
    cacheInv <<- inverse 
    return(cacheInv)
  }
  
  getInvrs  <- function() cacheInv
  list(set=set, get=get, setInvrs=setInvrs, getInvrs=getInvrs)

}


## Write a short comment describing this function
##this function checks the input and returns inverse if it is there other wise will solve
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Check in the cache
  clinvrs <- x$getInvrs() 
  
  ##check if there's a cached value AND it's a matrix
  if(!is.null(clinvrs))  { 
    message("getting cached data")
    return(clinvrs)
  }
  
  ## otherwise get the matrix
  mtrxtosolve <- x$get()  
  
  ## solve the matrix 
  clinvrs <-  solve(mtrxtosolve,...)
  message("Setting the value of inverse to:") 
  x$setInvrs(clinvrs)
  clinvrs
}

