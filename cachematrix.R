## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## In order to improve efficiency of computing, here using two functions to implement the data caching.

## This function creates a special "matrix" object that can cache its inverse.
## There are four function in it.They are get, set, getInverse and setInverse function.
##the argument "x" is a matrix, you can directly create a matrix which is invertable when you recall this function.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() {
                x
        }
        setInverse <- function(matrix_inverse) {
                i <<- matrix_inverse
        }
        getInverse <- function() {
                i
        }
        list(
                set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse
        )
        
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#the argument "x" is a list which is created by the makeCacheMatrix function.
cacheSolve <- function(x) {
        i <- x$getInverse()
        if (!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}
