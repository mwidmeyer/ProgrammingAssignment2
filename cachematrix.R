## Collectively, these two functions allow the user
# to cache the results of a matrix inversion after the
# first invocation ans reuse the result instead of 
# calculating it again on the fly, thereby saving
# time and inmproving performance for the user/calling program

## This function accepts a matrix as it's input.
## It then creates constructor/getter/setter methods

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function accepts a makeCacheMatrix object.  It first  checks
# to see if anything is already in the cache.  If so it uses it,
# otherwise it calculates the inverse.
# in either case, it then returns the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                print("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
