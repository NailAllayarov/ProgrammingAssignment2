## Below are defined two functions "makeCacheMatrix" and "cacheSolve" which allow us to compute 
## the inverse of the given matrix. One important feature of these function is the ability to restore 
## the calculations of inverse matrix without calculating it again (so called "caching")


## At first we define the function "makeCacheMatrix" which creates a list object which contains 4 functions
## (nested inside): get, set, getinv and setinv. "get" retrieves us the matrix x which is the argument of
## function "makeCacheMatrix". "set" allows us to pass another matrix as the argument (x) without calling again 
## and creating the object of the type "makeCacheMatrix". Analogously "getinv" retrieves the inverse of the given matrix 
## and "setinv" allows to set the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse_x) m <<- inverse_x
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The actual calculations of inverse matrix of x or getting the cached values in case the calculation were already performed 
## programmed by the "cacheSolve" function. In the first step we perform a check whether the argument "m" (means the inverse of x) 
## is not NULL i.e. the calculations were already performed correctly - in this case we get a message that we get a cached data
## and become the inverse matrix. If the inverse was not calculated we define the variable "data" which is just our matrix x,
## calculate straightforward its inverse and set this inverse matrix inside the definded object. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinv()
  if(!is.null(m)){
    print("getting cached value")
    return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$setinv(m)
  m
}


