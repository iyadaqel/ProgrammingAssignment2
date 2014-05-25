#This function is an efficient way of computing the inverse of the matrix by caching the inverse.
#if the matrix doesn't change. 

## This function will create the new type of Matrix and its a list of four things. Setting the matrix
#getting the matrix , setting the inverse and getting the inverse.


makeCacheMatrix <- function(x = matrix()) {
i <- NULL 
set <- function(y){
  x <<- y 
  i <<- NULL
}
get <- function()x
setInverse <- function(inverse) i <<- inverse
getInverse <- function()i 
list(set=set , get=get , setInverse = setInverse , getInverse = getInverse)
}


## This function will be used to compute the inverse of the matrix and cache it if its null.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)){
    return(i)
  }
  m <- x$get()
  i <- solve(m)
  x$setInverse(i)
  i 
}
