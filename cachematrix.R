## Put comments here that give an overall description of what your
## functions do

## https://cran.r-project.org/doc/manuals/R-intro.html#Scope

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## initialise local variable m 
    m <- NULL;
    list (
    ## set: cache passed matrix x, clear cached solution m 
    set = function(y) {
        x <<- y
        m <<- NULL},
    ## get: retrieve cached matrix from x
    get = function() x,
    ## setinv: cache solved inverse to m
    setinv = function(inv) m <<- inv,
    ## getinv: retrieve cached solution from m
    getinv = function() m
    )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    if (is.null(x$getinv())) {
        x$setinv(solve(x$get()))
    }
    x$getinv()
}
