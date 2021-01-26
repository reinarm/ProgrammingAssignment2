## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.


## This function creates a special matrix object that can cache it inverse.

# Initialize input x as empty matrix
makeCacheMatrix <- function(x = matrix()) {
        
        # Initialize object s
        s <- NULL
        
        # Assign ("set") input and value to objects in parent environment
        set <- function(y) {
                x <<- y     
                s <<- NULL
        }
        
        # Retrieve ("get") values from parent environment
        get <- function() x
        setsolve <- function(solve) s <<- solve    # access m
        getsolve <- function() s                   # retrieve m   
        
        # Assign each function as an element and return to parent environment
        list(set = set,
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

# Compute the inverse of the special matrix from makeCacheMatrix
cacheSolve <- function(x, ...) { 
        
        # Retrieve solve from argument
        s <- x$getsolve()
        
        # Check whether the result is NULL
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        # If result is FALSE, get data, solve, set and print
        data <- x$get() 
        s <- solve(data, ...) 
        x$setsolve(s)
        s
}