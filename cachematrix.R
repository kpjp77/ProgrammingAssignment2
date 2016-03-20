## This file contains 2 functions makeCacheMatrix and cacheSolve
## makeCacheMatrix creates a list containing a function to set and get the contents of a matrix and to set and get the inverse of the matrix
## cacheSolve calculates the inverse of the matrix after first checking if there is any cached data

## makeCacheMatrix creates a list containing a function to set and get the contents of a matrix and to set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function (solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve calculates the inverse of the matrix after first checking if there is any cached data

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
