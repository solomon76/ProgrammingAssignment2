## This source code contains the two functions need to allow the caching the Inverse of a Matrix and to computed the inverse of a matrix if not done already

## The makeCacheMatrix sets the value of the matrix,get the value of the matrix, set the value of matrix inverse, get the value of the matrix inverse. 
## In essence, it creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	invsqmatm <- NULL
        set <- function(y) {
                x <<- y
                invsqmatm <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) invsqmatm <<- solve 
        getsolve <- function() invsqmatm
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


##This function computes the inverse of the special "matrix"  
##If the inverse has been computed, it will retrieve from the cache
cacheSolve <- function(x, ...) {
	invsqmatm <- x$getsolve()
        if(!is.null(invsqmatm)) {
                message("getting cached data")
                return(invsqmatm)
        }
        data <- x$get()
        invsqmatm <- solve(data, ...)
        x$setsolve(invsqmatm)
        invsqmatm 
}

## Test matrix = matrix(1:4,nrows=2)
## The inversquare matrix %*% with Test matrix gives the identify matrix
