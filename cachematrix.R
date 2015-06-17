## The function makeCacheMarix creates a matrix that can cache its inverse
## The function cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## The inverse matrix is retrieved from cacheSolve

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
        rev <- NULL
        set <- function(y) {
                x <<- y
                rev <<- NULL
        }
        get <- function() x
        setreverse<- function(reverse) rev <<-reverse
        getreverse <- function() rev
        list(set = set, get = get,
             setreverse = setreverse,
             getreverse = getreverse)
}
        
cacheSolve <- function(x, ...) {
        rev <- x$getreverse()
        if (!is.null(rev)) {
                message("getting cached reververse matrix")
                return(rev)
        } else {
                rev <- solve(x$get())
                x$setreverse(rev)
                return(rev)
        }
}
