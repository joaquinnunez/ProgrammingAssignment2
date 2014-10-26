## Caching the Inverse of a Matrix
## This file contains two functions, makeCacheMatrix that stores a matrix and its inverse
## And cacheSolve that returns the inverted matrix from the cache if the inverted matrix 
## was already computed, or, it computes the inverted matrix and saves the inverted matrix
## value


makeCacheMatrix <- function(x = matrix()) {
        # used to store the inverted matrix
        invertedMatrix <- NULL

        # when the matrix changes, the invertedMatrix is set to NULL
        # so it gets computed again
        set <- function(y) {
                x <<- y
                invertedMatrix <<- NULL
        }

        get <- function() x

        setinverse <- function(inverse) invertedMatrix <<- inverse

        getinverse <- function() invertedMatrix

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## gets the cached inverse of the matrix or computes a new one
cacheSolve <- function(x) {
        ## Returns a matrix that is the inverse of 'x'
        invertedMatrix <- x$getinverse()
        if(!is.null(invertedMatrix)) {
                message("getting cached data")
                return(invertedMatrix)
        }
        matrix <- x$get()
        invertedMatrix <- solve(matrix)
        x$setinverse(invertedMatrix)
        invertedMatrix
}
