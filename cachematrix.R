##Caching the Inverse of a matrix
##The following functions help to compute the inverse of a matrix.
 

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
         i_matrix <- NULL
        set <- function(y) {
                x <<- y
                i_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i_matrix <<- inverse
        getinverse <- function() i_matrix
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##This function computes the inverse of the special matrix created in the above function - makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i_matrix <- x$getinverse()
        if (!is.null(i_matrix)) {
                message("getting cached data")
                return(i_matrix)
        }
        dat <- x$get()
        i_matrix <- solve(dat, ...)
        x$setinverse(i_matrix)
        i_matrix

}
