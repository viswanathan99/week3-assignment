## we write a function to cache the inverse of  a matrix and retreive its cache later
##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse1<-NULL
    set <- function(y){
    x <<- y
     inverse1 <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse1 <<- inverse
  getinverse <- function() inverse1
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## this function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse1 <- x$getinverse()
  if(!is.null(inverse1)) {
    message("getting cached data")
    return(inverse1)
    
  }
  mat <- x$get()
  inverse1 <- solve(mat, ...)
  x$setinverse(inverse1)
  inverse1
  
  
}
  