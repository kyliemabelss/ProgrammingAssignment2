## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse of the matrix
#get the value of the inverse of the matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inverso <- NULL
  set <- function(y){
    x <<- y
    inverso <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverso <<- inverse
  getinverse <- function (inverse) inverso
  list (set=set, get=get,
        setinverse=setinverse,
        getinverse=getinverse)
}


## cacheSolve returns the inverse of the matrix. it checks if the inverse
# is already computed, if so, it gets the result and skips computation.
#if not, it computes the inverse

cacheSolve <- function(x, ...) {
        
        inverso <-x$getinverse()
  if(!is.null(inverso)){
    message("getting cached data")
    return(inverso) ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  inverso <- solve(data)
  x$setinverse(inverso)
  inverso
}
