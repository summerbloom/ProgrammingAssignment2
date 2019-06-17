## This function creates a special "matrix" object and cache its inverse after having its inverse computed by the cachesolve function.

makecachematrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, 
             get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" defined by the makecachematrix function above. 
## If the inverse has already been calculated and the matrix has not changed, then the cachesolve should retrieve the inverse value from the cache. 
## Otherwise, the inverse computation of a square matrix will be done with the Solve function in R. 

cachesolve <- function(x) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setinverse(inv)
        inv
}
