## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## It returns a list of functions to set and get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    
    # Function to set a new matrix and reset the cached inverse
    set <- function(m) {
        x <<- m
        inv <<- NULL
    }
    
    # Function to get the current matrix
    get <- function() x
    
    # Function to set the inverse of the matrix
    setinv <- function(inverse) inv <<- inverse
    
    # Function to get the cached inverse of the matrix
    getinv <- function() inv
    
    # Return a list of the functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" created by makeCacheMatrix.
## If the inverse is already cached, it retrieves it instead of recalculating.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()  # Retrieve the cached inverse if available
    
    # If the inverse is cached, return it with a message
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise, calculate the inverse, cache it, and return it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

# Example usage
matrixObj <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2, 2))
inverseObj <- cacheSolve(matrixObj)  # Calculates and caches the inverse
inverseObj <- cacheSolve(matrixObj)  # Retrieves the cached inverse
