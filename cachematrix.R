## Caching a Given Matrix

## This function does the following:
## Set the value of the matrix
## Get the value of the matrix
## Set the inverse of the matrix
## Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i 
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## Solving the Inverse of a Given Matrix

## This function returns the inverse of a given matrix.
## It checks first if there is an existing value of inverse already.
## If there is, it skips the computation and returns the value.
## If there's none, then it computes the inverse then return the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}