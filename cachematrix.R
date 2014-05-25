
## Below are two functions that creates a special object
## that stores a matrix and can caches its inverse. 
## The first function, makeCacheMatrix creates a special "matrix",
## which is a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                    x <<- y
                    m <<- NULL      ## <<- operator assign a value to an object in an environment
                                    ## that is different from the current environment.
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following function computes the inverse of the special "matrix"
## created with the above function.
## However, it first checks to see if the inverse has already been computed.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it computes the inverse of the matrix
## and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
