##Programming Assignment 2
##Aliza Shoop
##Cache and return inverse matrix


## Create a matrix object that caches the inverse
## Return the inverse of a matrix, checking cache first

## first cache inverse of a matrix





makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## if inverse already exists in x, return x. if not, then compute inverse of a matrix.  

cacheSolve <- function(x, ...) {
        inverse <- x$getinv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse
}


