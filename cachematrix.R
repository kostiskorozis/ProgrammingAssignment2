## Put comments here that give an overall description of what your
## functions do

## Same as the example provided for vector, makeCacheMatrix creates a special
## matrix object with the four setmatrix/getmatrix/setinverse/getinverse functions

makeCacheMatrix <- function(x = matrix()) {
    #initialize inverse matrix
    inv_matrix <- NULL
    
    set_matrix <- function (y){
      x <<- y
      inv_matrix <<- NULL
    }
    get_matrix <- function () x
    
    set_inverse <- function(inverse) inv_matrix <- inverse
    
    get_inverse <- function () inv_matrix
    
    list(set_matrix = set_matrix, get_matrix = get_matrix,
         set_inverse = set_inverse, get_inverse= get_inverse)
}


## Same as the example provided for vector, cacheSolve gets the special
## matrix object and solves to return its inverse unless it is already cached
## in which case it returns the cached matrix

cacheSolve <- function(x, ...) {
    inv_matrix <- x$get_inverse()
    if (!is.null(inv_matrix)) {
          message("getting cached data")
          return(inv_matrix)
    }
    data <- x$get_matrix()
    inv_matrix <- solve(data)
    x$set_inverse(inv_matrix)
    inv_matrix
}
