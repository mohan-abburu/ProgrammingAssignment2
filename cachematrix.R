## Caching the Inverse of a Matrix
##  Mohan for programming assingment2 on 19th Feb,2018

## This function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  
  get<-function() x
  
  setInverse<-function(inv) i<<-inv
  
  getInverse<-function() i
  
  list( set= set,
        get=get,
        setInverse=setInverse,
        getInverse=getInverse)
  
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  i<-x$getInverse()
  if(!is.null(i)){
    message("getting cached inverse")
    return(i)
  }
  i<-solve(x$get(),...)
  x$setInverse(i)
  return(i)
}
