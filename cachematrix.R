## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## function to built list overhead with matrix and all functionality
## get allow you to get matrix - result of setmatrix operation
## set allow you to set matrix 
## m is a value of inverse matrix stored
## getmatrix allow you to get matrix - result of setmatrix operation
## setmatrix allow you to operate on matrix 

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set<-function(y){
            x<<-y
            m<<-NULL
      }
      get<-function() x
      setmatrix<-function(solve) m<<- solve
      getmatrix<-function() m
      list(set=set, get=get,
           setmatrix=setmatrix,
           getmatrix=getmatrix)
}


## Write a short comment describing this function
## function to manipulate components of makeCacheMatrix
## if value is cashed - return the result of getmatrix, otherwise - perform solve and save to makeCacheMatrix for future reusage


cacheSolve <- function(x=matrix(), ...) {
      ## Return a matrix that is the inverse of 'x'
      m<-x$getmatrix()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      matrix<-x$get()
      m<-solve(matrix, ...)
      x$setmatrix(m)
      m
}
