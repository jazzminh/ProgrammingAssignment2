## This is a programming assignment for a Coursera Data Scientist Course 
## The goal of these functions are to cache the inverse of a matrix.

## This function caches the Matrix

makeCacheMatrix <- function(x = matrix()) {
  b <- NULL
  set <- function(y) {
    x <<- y
    b <<- NULL
  }
  
  get <- function() x 
  setinverse <- function(inverse) b <<- solve(x)
  getinverse <- function() b
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
}


## This function caches the inverse

cacheSolve <- function(x, ...) {
  b <- x$getinverse()
    if(!is.null(b)) {
      message("Getting cached data...")
      return(b)
    }
  data <- x$get()
  b <- inverse(data, ... )
  x$setinverse(b)
  b
}

}
