## Used in combination, the following two functions check whether the inverse of
## a given matrix has already been cached, and if so, retrieve it from cache. Alternatively, 
## the inverse is calculated, cache'd and returned.


## The first function initiates two objects (initially empty), a matrix and its inverse.
## It then creates a list with four functions that set and retrieve the matrix and set and retrieve the inverse
## in the functions' parent environment.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set_matrix <- function(y){
              x <<- y
              inverse <<- NULL
              }
  get_matrix <- function() x
  set_inverse <- function(Solve) inverse <<- Solve
  get_inverse <- function() inverse
  list(set_matrix = set_matrix, 
       get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}



## This function, which takes the result from makeCacheMatrix as input, checks whether the inverse of a given matrix (input in makeCacheMatrix) has already been calculated.
## If so, the function retrieves the inverse from cache. If not, the function retrieves the matrix, calculates the inverse, sends it to cache and returns it.

cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()        
  if(!is.null(inverse)){
      message("getting cached data")
      return(inverse)
      }
  data <- x$get_matrix()
  inverse <- solve(data)
  x$set_inverse(inverse)
  inverse
}

