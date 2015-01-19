## The combination of the two functions will allow caching of matrix inverse
## avoiding the need to calculate it every time for the same matrix.

## Create a cached matrix from the provided matrix,
## and returns a list containing the available operations on the cached matrix.
## Use names() attribute on the returned matrix to find the operations name.
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<- function(m)
  {
    x<<- m
    inv<<- NULL
  }
  get <- function(){
    x
  }
  getInverse <- function(){
    inv
  }
  setInverse <- function(inverse){
    inv<<- inverse
  }
  list(set=set,
       get=get,
       get.inverse=getInverse,
       set.inverse=setInverse)
}

## Returns the inverse of a cached matrix.
## It try to solve the matrix from cache if computed before 
## or compute it and assign it to cache.
## This function assumes that the matrix is invertible.
cacheSolve <- function(x, ...) {
  inverse <- x$get.inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$set.inverse(inverse)
  inverse
}