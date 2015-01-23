## Functions to create environment and functions to calculate, cache and get the cached inverse of a matrix.

## makeCacheMatrix creates an environment for variables for matrix, inverse, and that matrix 
## that the actual stored inverse is calculated on. 
## returns a list of functions.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        orig <- NULL
        set <- function(y) {
                x <<- y
        }
        get <- function() x
        setinverse <- function(data) {
                orig <<- data
                m <<- solve(data)
        }
        getorig <- function() orig
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse,
             getorig = getorig)
}

## cacheSolve returns inverse of a matrix that is eventually stored in the lexical scope of the
## functions defined in makeCacheMatrix. Check if the inverse is calculated on actually stored matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of matrix stored in lexical scope of 'x'
		
        m <- x$getinverse()
             
        if (is.null(m)) {
                message("no cached data")
        
                x$setinverse(x$get())
                return(x$getinverse())
                
        }
        o <- x$getorig()
        n <- x$get()
        #message(identical(n,o))
        if(!identical(n,o)) {
                message("data changed since caching")
                x$setinverse(x$get())
                return(x$getinverse())
        }
        else {
                message("cached data")
                m
        }     
}
