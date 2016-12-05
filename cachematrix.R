## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #The first function, makeCacheMatrix creates a special "matrix", 
  #which is really a list containing a function to
  #         1. set the value of the matrix
  #         2. get the value of the matrix
  #         3. set the value of the matrix
  #         4. get the value of the matrix
  
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) invmat <<- inverse
  getinv <- function() invmat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  invmat <- x$getinv()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinv(invmat)
  invmat
}