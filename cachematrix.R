## Functions caching time consuming computations
## Pair of functions that can cache the inverse of a matrix.

## makeCacheMatrix creates a special matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {

## Initialize the inverse
      inverse <- NULL
## Set the matrix
      set <- function(Y) {
      x <<- Y
      inverse <<- NULL
      }
 ## Get the matrix
      get <- function() x
 ## Set the inverse of a matrix
      setmatrix <- function(solved) inverse <<- solved
 ## Get the inverse of a matrix
      getmatrix <- function() inverse
 ## Return a list of methods
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
           }


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated and the matrix has not changed, the function retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
   inverse <- x$getmatrix()
## Return the inverse if it is already set
   if(!is.null(inverse)) {
   message("getting cache data")
   return(inverse)
   }
## Get the matrix
   matrix <- x$get()
## Calculate and set the inverse
   inverse <- solve(matrix, ...)
   x$setmatrix(inverse)
## Return matrix
   inverse
   }

