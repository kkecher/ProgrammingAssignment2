## Put comments here that give an overall description of what your
## functions do
# Cache calculated data to use in other computations.

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    cached_inverse <- NULL
    set <- function(y) {
        x <<- y
        cached_inverse <<- NULL
    }
    get <- function() x
    set_inverse <- function(calculated_inverse) cached_inverse <<- calculated_inverse
    get_inverse <- function() cached_inverse
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse
         )
}

## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inversed_matrix <- x$get_inverse()
    if(!is.null(inversed_matrix)) {
        message("getting cached inverse matrix")
        return(inversed_matrix)
    }
    data <- x$get()
    inversed_matrix <- solve(data)
    x$set_inverse(inversed_matrix)
    inversed_matrix
}
