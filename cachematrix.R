## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        Cacheinv <- NULL
        set <- function(y) {
                x <<- y
                Cacheinv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) Cacheinv <<- inverse
        getInverse <- function() Cacheinv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  Cacheinv <- x$getInverse()
        if (!is.null(Cacheinv)) {
                message("getting cached data")
                return(Cacheinv)
        }
        mat <- x$get()
        Cacheinv <- solve(mat, ...)
        x$setInverse(Cacheinv)
        Cacheinv
}
