## Put comments here that give an overall description of what your
## functions do

##  makeCacheMatrix : This function creates a special "matrix" 
##  object that can cache its inverse
##  cacheSolve : This function computes the inverse of the 
##  special "matrix" returned by  makeCacheMatrix. 
##  If the inverse has already been calculated (and the matrix
##  has not changed), then  cacheSolve  retrieves the inverse
##  from the cache.
##  Assumption: the matrix supplied is always invertible

## Write a short comment describing this function

##  Creates a matrix x and caches the inverse of the matrix
##  Sample call:
##  > a <- makeCacheMatrix( matrix(c(1,2,3,4), nrow = 2,
##                        ncol = 2));

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

##  Returns a matrix that is the inverse of 'x'
##  Sample call:
##  > cacheSolve(a)    OR
##  > b <- cacheSolve(a), then > print (b) or > b

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
