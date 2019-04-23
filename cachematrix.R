## Lexical Scoping: Caching the Inverse of a Matrix
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

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
    
    
    ##=======TESTING THE FUNCTIONS====================================
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
