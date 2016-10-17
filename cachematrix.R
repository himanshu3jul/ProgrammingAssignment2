## Assignment: Caching the Inverse of a Matrix

## This function creates a vector which lists the functions to set a matrix, get a matrix, set inverse of a matrix and get inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL }
	get <- function() x
	setinv <- function(inv) i <<- inv
	getinv <- function() i
	list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function creates inverse of the matrix passed in the above function. First checks if inverse is already calculated. Result is returned from cache if already calculated else result is calcualted and saved in cache

cacheSolve <- function(x, ...) {
	i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}