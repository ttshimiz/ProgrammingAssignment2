## The two functions in this file control the data and inverse of a matrix.
## The inverse of a matrix can be cached in the memory using cacheSolve so it
## doesn't need to be repeatedly calculated.
## Example:
##      m <- matrix(c(-1, -2, 1, 1), 2,2)
##      x <- makeCacheMatrix(m)
##      inv <- cacheSolve(x)


## makeCacheMatrix creates a list of functions that get and set the data and
## inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    ## 'set' sets the values of the matrix and initializes 'inv' which will
    ## contain the inverse of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    ## 'get' returns the matrix values
    get <- function() x

    ## 'setinverse' saves the value of the inverse given
    setinverse <- function(inverse) inv <<- inverse

    ## 'getinverse' returns the inverse
    getinverse <- function() inv

    ## Return the list of functions
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve calculates and/or returns the inverse of a matrix that was created
## by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    ## Get the stored value for the inverse using 'getinverse' function
    inv <- x$getinverse()

    ## Check to see if the inverse has been calculated already
    ## If it has then return it.
    if (!is.null(inv)) {
        message('getting cached inverse')
        return(inv)
    }

    ## If the inverse hasn't been calculated then use 'solve' to calculate
    ## the inverse and set it.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
