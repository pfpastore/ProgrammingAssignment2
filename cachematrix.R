## Assignment: Caching the Inverse of a Matrix
## By: Paulo Pastore

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ##Set m to null first time it is called
        m <- NULL
        
        ##In case it is needed to reset the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ##Return the initial matrix
        get <- function() x
        
        ##Store the content of the inverse
        setsolve <- function(solve) m <<- solve
        
        ##Return the content of the inverse     
        getsolve <- function() m
        
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        print("x$getsolve()")
        print(x$getsolve())
        
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        print("x$get()")
        print(x$get())
        
        data <- x$get()
        m <- solve(data)
        x$setsolve(m)
        m
}
