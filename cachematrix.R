## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix and returns a list of special get/set functions for caching the matrix state

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
       x <<- y
       m <<- NULL
     }
     get <- function() x
     setmatinv <- function(matinv) m <<- matinv
     getmatinv <- function() m
     list( set = set, get = get, setmatinv=setmatinv, getmatinv=getmatinv)
}


## This function first looks at previously saved results for matrix inverse. If there is no previously saved results, it finds matrix inverse, saves it and returns the inverse. If there is previously saved results, it returns the saved results

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatinv()
  if (!is.null(m))
  {
    message("getting cached data")
    return (m)
  }
  data <- x$get()
  m<-solve(data, ...)
  x$setmatinv(m)
  m
}


