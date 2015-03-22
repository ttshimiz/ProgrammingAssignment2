## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    ## Get the stored value for the inverse using 'getinverse' function
    inv <- x$getinverse()

    ## Check to see if the inverse has been calculated already
    if (!is.null(inv)) {
        message('getting cached inverse')
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
