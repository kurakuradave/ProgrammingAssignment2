## Put comments here that give an overall description of what your
## functions do
## first function, makeCacheMatrix, creates a special "matrix" object that can cache its inverse.
## second function, cacheSolve, computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## Write a short comment describing this function
## the function returns a list with four functions, for getting and setting the data (matrix), and for getting and setting the inverse of the matrix.
## the data and inverse are stored in a different environment using the <<- operator, taking advantage of R's lexical scoping rules.
## NOTE: input matrix is assumed to be square and has an inverse

makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    set <- function( some_matrix ) {
        x <<- some_matrix
        mi <<- NULL
    }
    get <- function() x
    setSolved <- function( some_solved ) mi <<- some_solved
    getSolved <- function() mi
    list( set=set,
          get=get,
         setSolved=setSolved,
         getSolved=getSolved
    )
}


## Write a short comment describing this function
## first check to see if there's a value for the inverse (inverse had been calculated before). If yes, return the inverse.
## if not, compute the inverse and then store it in the other environment, via setSolved() function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mi <- x$getSolved()
    if( !is.null(mi) ){
        print( "getting cached inverse of the matrix" )
        return( mi )
    }
    da_matrix <- x$get()
    mi <- solve( da_matrix )
    x$setSolved( mi )
    mi
}

## for testing (uncomment to test)
## czmx <- matrix( rnorm(25), nrow=5 ) # create some square matrix
## yeap <- makeCacheMatrix( czmx )     # create the cache matrix object (list)
## cacheSolve(yeap)                   # first call to cacheSolve(), should compute the inverse
## cacheSolve(yeap)                   # subsequent calls to cacheSolve(), should just read from cache

### create a new square matrix
## czmx <- matrix( rnorm(25), nrow=5 ) 
## yeap <- makeCacheMatrix(czmx)       # update the yeap object, so there's a change to the matrix
## cacheSolve(yeap)                   # call cacheSolve() after matrix changed, should compute inverse
## cacheSolve(yeap)                   # subsequent calls to cacheSolve(), should just read from cache
