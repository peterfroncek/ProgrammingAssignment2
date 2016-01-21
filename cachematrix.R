## The two functions are able to store matrices and perform inverse operations on##them in cached environments.



## creates a list containing 1) a function to set the value of the matrix, 2) get ##the matrix, 3) get the inverse of the matrix and 4) set the inverse of the ##matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(sol) inverse <<- sol
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## solves the inverse of a cached matrix and stores its value into the cached ##list. If the inverse has been already computed previously just returns its ##value.

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
