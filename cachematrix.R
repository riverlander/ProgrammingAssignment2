## The objective is to reduce the execution times when we perform an operation (in this case the inverse of a square matrix)
## For this we will perform the caching of the result of the operation so that when we need that result again it will not be necessary to recalculate it.

## With the following function we create a "special" matrix with four associated functions: get, set, getinverse and setinverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function receives as argument a "special" matrix defined with the previous function. 
## Check if it is stored in the enviroment the value of the inverse of the matrix and otherwise calculate and store it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  p = x$getinverse()
  if(!is.null(p)) {
    message("getting cached data")
    return(p)
  }
  data = x$get()
  p = solve(data, ...)
  x$setinverse(p)
  p
}
