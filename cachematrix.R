## 
## README for cacheSolve()
##
## This code snippet is the programming assignemnt2 of R Programming course
## on Coursera
## It implementes a cachable inversed matrix function to increase efficiency. 
##
##
## CODE SAMPLE:
##
##   The following three lines make a 2x2 matrix and apply
##   the cacheSolve() we implemente in this code snnipet
##
## > mb <- matrix(c(3.0, 3.2, 3.5, 3.6), nrow = 2, ncol = 2)
## > ab <- makeCacheMatrix(mb)
## > cacheSolve(ab)
##
## If you would like to compare the result computed by native R, try
## > solve(mb)
##

## make a caheable object to store the inversed matrix as well as
## the corresponding original matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # initialize the object
    m <- NULL
    
    # initialize the cache with the original matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # retrieve the original matrix
    get <- function() {
        x
    }
    
    # cache the inversed matrix
    setinverse <- function(matrix) {
        m <<- matrix
    }
    
    # retrieve the cached invsersed matrix
    getinverse <- function() {
        m
    }
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function computes the inverse of a matrix
## using the makeCacheMatrix() declared above.
## It leverage the cachable object when the result has been
## computed before by directly fetching the result rather than
## computing it another time. When the result is not cached
## the result will be computed and saved to the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # query the cache
    m <- x$getinverse()
    if (!is.null(m)) {
        # cache hit and return the cached result
        message("getting cached result")
        return(m)
    }
    
    # retrive the original matrix from the object
    data <- x$get()
    # solve the inversed matrix by native R solve() function
    m <- solve(data, ...)
    # save the result to the cache
    x$setinverse(m)

    # return the inversed matrix
    m
}

