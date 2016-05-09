## Functions to store a matrix along with its inverse
## assuming there is an inverse

## makeCacheMatrix stores a matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # gets the original matrix
  get <- function() x
  ## sets the inverse
  setinverse <- function()  inverse <<- solve(x)
  ## returns the inverse
  getinverse <- function() inverse
  
  # Returns list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Cachesolve 
## Takes variable set from makeCacheMatrix and either returns inverse
## if already calculated or converts matrix to dataframe and runs solve. After this it is cached using setinverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <-x$getinverse()
  if(!is.null(inverse)) {
    message("getting inverse")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
