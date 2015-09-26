## Put comments here that give an overall description of what your
## functions do

## Function to create a special "matrix" object that can cache
## its inverse in a separate environment

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv )
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the cachesolve should retrieve the inverse from the cache 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()

    ## else, if the inverse has already been calculated and return cached inv
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## otherwise, recalc and return
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
