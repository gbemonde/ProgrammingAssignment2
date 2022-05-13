## Put comments here that give an overall description of what your
## functions do

## This function creates the inverse of the matrix x and put it in the cache under the name mat_inv

makeCacheMatrix <- function(x = matrix()) {
mat_inv <- NULL
  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  get <- function() x
  if (!det(x)==0) {
  setsolve <- function(solve) mat_inv <<- solve}
  
  getsolve <- function() mat_inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function searches the cache for the inverse of the matrix x. 
## If the search is negative, it will create the inverse and assignes it to mat_inv

cacheSolve <- function(x, ...) {
        mat_inv <- x$getsolve
  if(length(mat_inv)>1) {
    message("getting cached data")
    return(mat_inv)
  }
  data <- x$get
  mat_inv <- solve(data, ...)
  x$setsolve(mat_inv)
  mat_inv
}
