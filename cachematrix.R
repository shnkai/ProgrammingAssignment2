## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ##inverse matrix storage variable
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x ##give original matrix back
        setinv <- function(inv) i <<- inv ##store inverse matrix in storage variable
        getinv <- function() i ## give storage variable back
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

cacheSolve <- function(x, ...) {
        i <- x$getinv() ##checking whether inverse matrix is in cache
        if(!is.null(i)) { ##if it is in cache, return inverse matrix
                message("getting cached data")
                return(i)
        }##it is not in the cache
        data <- x$get()  ##storing original matrix in variable
        i <- solve(data, ...) ##solve originalfor inverse matrix
        x$setinv(i) ##set inverse matrix
        i ## Return a matrix that is the inverse of 'x'
}
