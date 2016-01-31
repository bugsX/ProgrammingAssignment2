## This is a cache for storing inverse of a matrix. Instead of computing a cache of 
## a matrix everytime, this function stores the matrix and its cache. When ever there 
## is a need for inverse, it is first looked in the cache and if it exists, the value is returned.
## If it is not present in cache the inverse is stored in cache and returned. The inverse
## value is cleared when the matrix changes.

## This function is caches a matrix and its inverse.
## The function makeCacheMatrix creates  special Matrix which is a list containing 4 functions
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL 

  set <- function(mat) {
    x <<- mat
    invM <<- NULL
  }
  
  get <-function() x
  
  setInv <- function(inv) invM <<- inv
  
  getInv <- function() invM
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


## Checks if the inverse of the special matrix is present in cache. If it is present it returns
## the value. Else it conputes the inverse and stores in cache.

cacheSolve <- function(x, ...) {
  invM <- x$getInv()
  
  if(!is.null(invM)) {
    message("Getting Cached Data")
    return (invM)
  }
  
  invM <- solve(x$get(), ...)
  x$setInv(invM)
  invM
}
