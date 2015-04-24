# instructions to run this
#
# source("Matrix.R")
# m=matrix(1:4,2,2)
# a=makeCacheMatrix(m)
# cacheSolve(a)
#
# if you run the above statement again - it will get the inverse crom Cache
# But if you change the matrix  and run it, it will re-calculate the Inverse
# m=matrix(2:5,2,2)
# a=makeCacheMatrix(m)
# cacheSolve(a)
#

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() {
                x
        }
        
        setinverse <- function(inverse) {
                m <<- inverse
        }
        
        getinverse <- function() 
        {
                m
        }
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        im <- x$getinverse()
        
        # check if the inverse is in the cache
        
        if(!is.null(im)) {
                message("getting Inverse of the Matrix from cached data")
                return(im)
        }
        else{
                message("calculating the Inverse Matrix")
        }
        
        # get the matrix
        
        data <- x$get()
        
        # calculate the inverse of the matrix
        
        im <- solve(data, ...)
        
        # Cache the inverse
        
        x$setinverse(im)
        
        # return the inverse
        im
}