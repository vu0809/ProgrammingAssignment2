## These are two functions that are used to create a special object
## that stores a matrix  and cache's its inverse.


## The first function, makeCacheMatrix creates a special "matrix" object, 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		iv <- NULL
        set <- function(y) {
                x <<- y
                iv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) iv <<- inverse
        getinverse <- function() iv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function computes the inverse of the special"matrix" created by makeCacheMatrix.
## However, it first checks to see if the inverse has been computed already
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it computes the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		iv <- x$getinverse()
        if(!is.null(iv)) {
                message("getting cached data")
                return(iv)
        }
        data <- x$get()
        iv <- solve(data, ...)
        x$setinverse(iv)
        iv
		
}
