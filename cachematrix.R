## The functions store a matrix (given as argument) and its inversed matrix
## in the cache

## The first function takes in the matrix and returns a list of functions set, get,
## setInverse and getInverse. 'set' puts the matrix in the cache. 'get' returns the matrix,
## 'setInverse' is called from the cacheSolve function with the inversed matrix as argument,
## 'getInverse' is called from the cacheSolve function with 'm' as argument. m is Null when called first,
## m contains the cached inversed matrix after the first call


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## this function first checks is there is already a matrix in m (from makeCacheMatrix())
## if m is Null, the inverse of the matrix from the argument gets stored in m,
## m is cached by passing it as argument to setInverse() of makeCacheMatrix().
## m is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
