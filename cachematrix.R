## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x

        setinv <- function(inverse) m <<- inverse
        getinv <- function() m

        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if (!is.null(m)) {
                message("getting inverse cached")
                return(m)
        }

        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

# Examples
# m <- matrix(c(-5, -3, 2, 1), nrow = 2, ncol = 2)
# cacheSolve(makeCacheMatrix(m))
#
# m <- matrix(c(4, 3, 1, 1), nrow = 2, ncol = 2)
# cacheSolve(makeCacheMatrix(m))
#
# m <- matrix(c(4, 3, 3, 2), nrow = 2, ncol = 2)
# cacheSolve(makeCacheMatrix(m))
