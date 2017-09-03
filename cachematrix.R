## This program aims to optimize the time-consuming 
## computation of matrix inversion by caching.


## This function creates a special "matrix" object 
## that can cache its inverse.

##First, initialize x and m
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## setter function: assign the input function
        ## to the x object in the parent environment,
        ## and assign the value of NULL to the m object
        ## in the parent environment, so that prior
        ## caches would be cleared.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## getter function: retrieve x from the parent 
        ## environment because x is not defined in get()
        get <- function() x
        
        ## the code uses <<- to assign the input argument
        ## to the value of m in the parent environment,
        ## because m is defined in the parent environment
        ## and we need to access it after setsolve() completes
        setsolve <- function(solve) m <<- solve
        
        ## getter of m
        getsolve <- function() m
        
        ## assign each of the functions as an element
        ## within the list
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The following function computes the inverse of the
## special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## If the result of !is.null(m) is FALSE,
        ## cacheSolve() gets the matrix from the
        ## input object
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
