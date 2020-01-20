## Put comments here that give an overall description of what your
## functions do

## Make a function generating a list of functions composed of set(), get(), setinverse() and getinverse() 
## in response to matrix x
## m is intialized as a null everytime the function is called. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Return a matrix that is the inverse of the matrix x. Inputting variable needs to be the list returned by
## makeCacheMatrix. Would directly get the inverse if it is already stored in the cache instead of calculating it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                m <- x$getinverse()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setinverse(m)
                m
}

