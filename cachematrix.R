#1
makeCacheMatrix <- function(x = matrix()) {
  #at the beginning it doesn't cache anything
  inv_x <- NULL
  #store matrix
  setMatrix <- function(y) {
    x <<- y
  #Since the matrix is assigned a new value, it is to be NULL
    inv_x <<- NULL
  }
  #get the matrix returened
  getMatrix <- function() x
  #cache
  setInverse<- function(inverse) inv_x <<-inverse
  #cached-value
  getInverse <- function() inv_x
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

#2 
cachesolve<-function(x,...){
  #get cached values
  inv_x <- x$getInverse()
  #check the status of existing of cached values
  if (!is.null(inv_x)) {
  message("getting the cached inverse matrix")
  #get the matrix if there is no catched data, otherwise caluculate  
    return(inv_x)
    } else {
  　inv_x <- solve(x$getMatrix())
  　x$setInverse(inv_x)
  　return(inv_x)
  　}
}
