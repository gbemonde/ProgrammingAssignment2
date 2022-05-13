## The first function, makeCacheMatrix creates a  inverse of a "matrix", such that matrix* inverse (matrix)=1

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


## This function calculates the inverse of a "matrix" created with the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
