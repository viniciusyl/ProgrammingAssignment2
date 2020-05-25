## The functions makeCacheMatrix and cacheSolve will cache the inverse of a
# matrix and retrieve the results when it's necessary

## Creates a list with the funciotns set, get, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## The following function checks if has already been calculated the inverse 
## of a matrix. In Positive case, retrieve the results has already calculated. 
## In Negative case, will calculate de inverse and show the results.

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
