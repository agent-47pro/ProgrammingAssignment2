## Here I will cache the inverse of a matrix
## It is better to cache the inverse of the matrix for efficiency purposes.
## My assignment is to write a pair of functions that cache the inverse of a matrix.

## The purpose of this function is to create a special matrix object to achieve the objective of aching the inverse.

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  Setting <- function(z){
    x <<- z
    Inv <<- NULL
  }
  Getting <- function()x
  set.Inverse <- function(Inverse) Inv <<- Inverse
  get.Inverse <- function()Inv
  list(Setting = Setting,
       Getting = Getting,
       set.Inverse = set.Inverse,
       get.Inverse = get.Inverse)
}


## The purpose of this second function is to inverse the above. However, if the inverse is already calculated
## (without the matrix being changed), then it will retrieve the cache's inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    Inv <- x$get.Inverse()
    if(!is.null(Inv)) {
      message("Data is already cached")
      return(Inv)
    }
mtx <- x$Getting()
Inv <- solve(mtx, ...)
x$set.Inverse(Inv)
}
