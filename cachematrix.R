## The first function is used to create a special "matrix" object that can cache
## its inverse. The second function This function computes the inverse of the special 
## "matrix" returned by the first function.

## The following function consists of 4 functions, where the first one sets the value of
##  matrix; the second gets of the matrix; third sets the value of the inverse and the
## fourth gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setm<-function(y){
    x<<-y
    m<<-NULL
  }
  getm<-function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(setm = setm, getm = getm,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function checks if the makeCachematrix function has calculated the 
## inverse matrix, if so it prints the inverse. If not, the following function
## calculates the inverse and places it in cache, using the getm function from 
## makeCachematrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getm()
  m <- solve(data, ...)
  x$setinv(m)
  m
}