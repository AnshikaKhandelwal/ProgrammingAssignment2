##The first function, makeCacheMatrix creates a matrix


makeCacheMatrix <- function(x = matrix()) {
me <- NULL
    set <- function(y) {
        x <<- y
        me <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) me <<- solve
    getsolve <- function() me
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## The following function calculates the inverse of matrix created with the above function
cacheSolve <- function(x, ...) {
me <- x$getsolve()
    if(!is.null(me)) {
        message("getting cached data")
        return(me)
    }
    data <- x$get()
    me <- solve(data, ...)
    x$setsolve(me)
    me
        ## Return a matrix that is the inverse of 'x'
}
