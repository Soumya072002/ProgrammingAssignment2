## Creating a function to carry out Matrix Inversion for square (2x2)matrix
## The function makeCacheMatrix will generate the inverse matrix of the input matrix
## In order to retrieve the inverse matrix for an already generated one 
## we will use another function cacheSolve , R will check if there is 
## data available in cache (via cacheSolve function)-will retrieve if available
## will generate if not available & feed makeCacheMatrix function

## function makeCacheMatrix will generate the inverse matrix of the input matrix
## R will check if there is data available in cache (via cacheSolve function)-
## will retrieve if available
## will generate if not available & feed makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
    inv_mat <- NULL #Named variable as inv_mat for Inverted Matrix
    set_mat <- function(y) {
    x<<- y
    inv_mat <<- NULL
    }
    get_mat <- function()x #Getting the Matrix data from set_mat
    set_matinv <-function(solve) inv_mat<<- solve
    get_matinv <-function()inv_mat
    list(set_mat = set_mat, get_mat = get_mat, 
         set_matinv = set_matinv, get_matinv = get_matinv )
}

## In order to retrieve the inverse matrix for an already generated one 
## we will use another function cacheSolve , R will check if there is 
## data available in cache (via cacheSolve function)-will retrieve if available
## will generate if not available & feed makeCacheMatrix function

cacheSolve <- function(x, ...) {
       inv_mat <- x$get_matinv()
       if (!is.null(inv_mat)) {
       message( "getting cached inverted matrix")
       return(inv_mat)
       }
       data_mat <- x$get_mat()
       inv_mat <- solve(data_mat, ...)
       inv_mat ## Return a matrix that is the inverse of 'x'
}
