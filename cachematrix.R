## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix holds a matrix and its inverse, once set

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
       x <<- y
       inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## returns inverse of matrix, checks makeCacheMatrix for
##   solution before calling solve()

cacheSolve <- function(x, ...) {
        invmat <- x$getinv()
        if(!is.null(invmat)) {
             message("getting cached data")
             return(invmat)
        }
        data <- x$get()
        invmat <- solve(data)
        x$setinv(invmat)
        invmat
}
