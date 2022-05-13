## Put comments here that give an overall description of what your
## functions do

## MakeCacheMatrix take a square matrix and calculate its inverse. 
## The result is saved in cache where it can be pulled later

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


## This fuction takes a matrix and pulled its inverse from the cache.
## If the inverse is not in the cache, the function will calculate the inverse and saved it in the cache.

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
