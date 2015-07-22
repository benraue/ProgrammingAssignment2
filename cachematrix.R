## The first function takes a matrix and creates four other functions
## based on that matrix.
## The second function then takes that first function and, if there 
## has already been an inverse calculated, uses that inverse, or 
## otherwise takes the inverse previously calculated.

## This function takes a matrix.

makeCacheMatrix <- function(x = matrix()) {
## 'inv' represents the inverted matrix. If you run this function, 
## the inverted matrix is reset to NULL.
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
## This function 'get' just pulls the original matrix.
  get <- function() x
## This function 'setinv' sets the inverse to whatever it points to.
  setinv <- function(z) inv <<- z
## This function pulls the existing 'inv'
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function needs to be pointed to the result of the previous
## function.

cacheSolve <- function(x) {
## 'inv' is set to 'inv' as it currently stands in the first function.
  inv <- x$getinv()
## if 'inv' is not null, this section returns 'inv' along with a message.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
## if 'inv' is null, 'mat' pulls the matrix referred to in the original function.
  mat <- x$get()
## in this row, the original matrix is inverted.
  inv <- solve(mat)
## 'inv' in the original function is now set to the result of the previous row.
  x$setinv(inv)
## the result is printed.
  inv
}