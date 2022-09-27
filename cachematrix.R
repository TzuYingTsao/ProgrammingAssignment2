## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#create a makeCacheMatrix function
##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = numeric()) {
  i <- NULL ##initializing i as NULL
  set <- function(y) {
    
    ##<<- saved the data object in the global environment outside of the user-defined function.
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  ##It first checks to see if the inverse has already been calculated. 
  ##If so, it gets the inverse from the cache and skips the computation.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ##Otherwise, it calculates the inverse of the data and sets the value of the mean in the cache via the setmean function.
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

