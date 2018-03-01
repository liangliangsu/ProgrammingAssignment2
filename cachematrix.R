## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL         #initialize the invsersed matrix
  set <- function(y) {      #get the value of the matrix and the invserse matrix
    x <<- y
    invmatrix <<- NULL
  }
  get <- function() x       #get the matrix
  setinv <- function(inverse) invmatrix <<- inverse  #set the invsersed matrix
  getinv <- function() invmatrix                     #get the invsersed matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmatrix <- x$getinv()
  if(!is.null(invmatrix)) {            #if invsersed matrix exist
    message("getting cached data")
    return(invmatrix)                  #return invsersed matrix
  }
  data <- x$get()
  invmatrix <- solve(data, ...) 
  x$setinv(invmatrix)
  invmatrix
  
}
