##Caching the inverse of matrix is a simplified approach to compute matrix inversion. Below are two functions: 
## makeCacheMatrix creates a special matrix that cache's its own inverse and 
##cacheSolve that computes the inverse of the matrix returned by makeCacheMatrix and if the inverse has already been computed #then the cacheSolve function retrieves the inverse from cache.

## makeCacheMatrix creates a list containing a function to
# 1. set the value of matrix (set)
# 2. get the value of matrix (get)
# 3. set the value of inverse of matrix (setinverse)
# 4. get the value of inverse of matrix (getinverse)

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix. It first checks if the inverse has already been calculated. If yes, then it #retrieves the inverse from cache using getinverse function and skips the computation (second part of the function). If not, #then it computes the inverse and sets the value in the cache by using setinverse function.

cacheSolve <- function(x, ...) {
m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
