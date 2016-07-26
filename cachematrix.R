##  Caching function results using lexical scoping
##
##  Function results can be cached using a global varible however this approach
##  introduces the risk that the cached variable may be unintentionally modified
##  by other functions.
##
##  A solution to this problem is to store the cached result
##  within an environment bound to a symbol. The cached result can then be
##  retrieved using a list of returned functions.
##
##  When the makeCacheMatrix() is assigned to a symbol, for eg:
##      > cachetest <- makeCacheMatrix(matrix(runif(1000000), 1000, 1000))
##  the environment created by makeCacheMatrix() is bound to the symbol cachetest.
##  The matrix passed to the function is assigned to variable x, and m is set to a
##  null value.
##  If the function is called without a matrix as an arguement x is created as an
##  empty matrix.
##
##  The tree of environments created by this process are:
##
##  -> Global Environment
##        [ cachetest ]
##  ---> makeCacheMatrix()
##          [ x, m, list(get(), set(), setinv(), getinv())]
## 
##  The variables x and m cannot be accessed directly from the global environment.
##  Calling the returned functions creates a new frame within the environment bound
##  to cachetest which is destroyed when the execution of the function is completed.
##
##  ---> cachetest
##          [x, m]
##  ------> get()
##              [ ]
##
##  The special assignment operator "<--" allows the returned function to explicity
##  access variables in the functions parent environment. This provides a method to modfiy the
##  the cached variables x and m. 
##  
##  Further reading:
##      R-project.org, An Introduction to R, "10.7 Scope" 
##          https://cran.r-project.org/doc/manuals/R-intro.html#Scope
##      Gentleman, R, and Ihaka, R, Lexical Scope and Statistical Computing, "4.1 Random Number Generators"
##          https://www.stat.auckland.ac.nz/~ihaka/downloads/lexical.pdf

###########################
##  makeCacheMatrix
###########################
##  makeCacheMatrix() is used to create an enviroment which contains a matrix,
##  and a cached copy of it's inverse.
##  Calling the function without an argument creates x as an empty matrix.
##  The cached inverse is cleared when is the function is called directly,
##  or by calling the returned function set().
##  If the inverse solution is available getinv() returns a NULL value.
##
##  A cache object can be prepared in two ways:
##  > cachetest <- makeCacheMatrix()
##  > cachetest$set(matrix(runif(1000000), 1000, 1000))
##  or
##  > cachetest <- makeCacheMatrix(matrix(runif(1000000), 1000, 1000))

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL;
    list (
    ## set: cache passed matrix x, clear cached solution m 
    set = function(y) {
        x <<- y
        cache <<- NULL},
    get = function() x,
    setinv = function(inv) cache <<- inv,
    getinv = function() cache
    )
}


###########################
##  cacheSolve
###########################
##  cacheSolve() takes an object created with makeCacheMatrix() as an arguement.
##  for e.g.
##  >  cacheSolve(cachetest)
##  The function calls the returned function getinv() which returns NULL if a
##  solution for the current matrix has not been cached. If the result is NULL
##  get() is called to retreive the matrix stored in "cachetest".
##  The retireved matrix is passed to solve(), and the result is cached by calling
##  setinv(). getinv() is called to return the cached inverse.
 
cacheSolve <- function(x, ...) {
    if (is.null(x$getinv())) {
        message('Solving matrix inverse...')
        x$setinv(solve(x$get()))
    } else {
        message('Retrieving cached solution...')
    }
    ## Return cached copy of inverse matrix
    x$getinv()
}
