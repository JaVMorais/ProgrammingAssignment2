## Functions fot the week 3 assignment of the course "R programming". 

## Function that creates a matrix structurethat can contain the matrix x and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # caches the value of the matrix to y and sets its inverse to NULL 
    set <- function(y) {
        x <<- y
        m <<- NULL
    }  
    # retrieves the value of the matrix
    get <- function() x  
    # sets the value of the inverse to inverse
    setinverse <- function(inverse) m <<- inverse  
    # retrieves the value of inverse
    getinverse <- function() m  
    
    list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## Function that takes a matrix structure and calculates its inverse if it has not yet been calculated. 
cacheSolve <- function(x, ...) {
    # retrieve the saved value of the inverse
    m <- x$getinverse()  
    data <- x$get()
    # if the inverse is not defined, solve(x) for the inverse, save it in the cache and return it
    if(is.null(m)) {
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        return(m)
    }   
    # if the value saved in cache is the inverse of x, then return it
    message("Retrieving cached data.")
    if(identical(data%*%m,diag(dim(data)[1]))){
        return(m)
    }   
    # if not, re-calculate the inverse, save it in cache and return it
    else{
        warning(paste("The value saved in cache was not correct. Calculating the inverse of",x))
        m <- solve(data, ...)
        x$setinverse(m)
        return(m)
    }  
}
