## The functions objective is to cache the inverse of a matrix and retrieve 
## when recomputed.

## This function creates a list contatining the functions to store 
## the matrix and its inverse

makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
}



## This function rerieves the inverse of a matrix if its has been 
## computed already

cachesolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

