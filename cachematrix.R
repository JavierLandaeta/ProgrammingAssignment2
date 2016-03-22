## both functions creates a matrix object that can cache its inverse. Then, if
##the matrix already been calculated, the function uses the matrix already cached.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  get <- function() x
  setinv <- function (inv) i <<- inv
  getinv <- function () i
  
  list (get = get, setinv = setinv, getinv = getinv)

}


## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if (!is.null(i)){
    message("getting cache data")
    return(i)
  }
  i <- solve(x$get())
  x$setinv(i)
  i
  }
