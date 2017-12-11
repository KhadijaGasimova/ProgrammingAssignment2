## It enables us to cache time-consuming computation such as matrix 
## inversion

## makeCacheMatrix function creates a matrix object that caches its inversion.

makeCacheMatrix <- function(x = matrix()) {  i <- NULL
  set <- function(y) {
          x <<- y 
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function. computes the inversion of the special matrix object created by function makeCacheMatrix

cacheSolve <- function(x, ...) {i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
