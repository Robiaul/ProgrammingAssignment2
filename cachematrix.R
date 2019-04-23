## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## Caching the Inverse of a Matrix:
    ## Matrix inversion is usually a costly computation and there may be some 
    ## benefit to caching the inverse of a matrix rather than compute it repeatedly.
    ## Below are a pair of functions that are used to create a special object that 
    ## stores a matrix and caches its inverse.
    
    ## This function creates a special "matrix" object that can cache its inverse.
    
    makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
    }
    
    
    ## This function computes the inverse of the special "matrix" created by 
    ## makeCacheMatrix above. If the inverse has already been calculated (and the 
    ## matrix has not changed), then it should retrieve the inverse from the cache.
    
    cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        dat <- x$get()
        inv <- solve(dat,...)
        x$setInverse(inv)
        inv
    }
    
    
    ##=======TESTING====================================
    # source("ProgrammingAssignment2/cachematrix.R")
    # my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
    # my_matrix$get()
    # my_matrix$getInverse()
    # cacheSolve(my_matrix)
    # cacheSolve(my_matrix)
    # my_matrix$getInverse()
    # my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
    # my_matrix$get()
    # my_matrix$getInverse()
    # cacheSolve(my_matrix)
    # cacheSolve(my_matrix)
    # my_matrix$getInverse()
    ##=======END TESTING====================================
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
