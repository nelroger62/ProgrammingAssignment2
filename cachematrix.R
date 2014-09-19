## makeCacheMatrix creates a new matrix object contains a list of functions
## used to set and get the actual matrix as well as getting and setting the
## calculated and cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    ## create a variable m to receive the cached matrix
    m <- NULL
    
    ## passes and caches a new matrix into the object
    ## clears the cached inverse matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## gets the matrix stored by the object
    get <- function() x
    
    ## creates and caches the inverse of the stored matrix
    setinverse <- function(inverse) m <<- inverse
    
    ## gets the inverse matrix from cache
    getinverse <- function() m
    
    ## store the functions in a list so they can be called in other functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve receives the object created by makeCacheMatrix and used the
## function defined in the list to determine if it needs to call "solve"
## function to calculate the inverse matrix
cacheSolve <- function(x, ...) {
    ## retrieve the cached inverse matrix
    m <- x$getinverse()
    
    ## if there is already a cached inverse matrix the return the matrix
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## when there is no cached inverse matrix retrieve the stored matrix
    ## from the object, do the inverse and call the setinverse function
    ## to cache the result
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
