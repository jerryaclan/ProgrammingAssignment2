 
## The two functions below simply calculates the inverse of a matrix and cache/save the result.
## this is to improve performance by not having to re-calculate the inverse of the same matrix
## everytime.  pre-calculated inverse of a matrix, in short.


## Write a short comment describing this function
##
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        get <- function() x

        setInverse <- function(solve)m <<- solve

        getInverse <- function() m

        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

 
## retrieves the solve's resulting matrix.
## if getInverse returns a null, it means this is a new, uncached, matrix.
## if uncached, it will compute the inverse, then cached it.
## if already cached, it will get the value of m from the cached environment.
cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

        m <- x$getInverse()
        if(!is.null(m)) {
             message ("getting cached data")
                return(m)
        }

        data <- x$get()

        m <- solve(data, ...)
        x$setInverse(m)
        m

}


