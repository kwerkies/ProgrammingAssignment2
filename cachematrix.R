## Put comments here that give an overall description of what your
## functions do
## This pair of functions allows us to cache the inverse of a matrix.

## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) s <<- solve
        getmatrix <- function() s
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## Write a short comment describing this function
## Computes the inverse of the special "matrix" returned by the makeCacheMatrix function
## above. If the inverse has already been calculated, then the cacheSolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getmatrix()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data)
        x$setmatrix(s)
        s
}
