## makeCacheMatrix - This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {                     
                        x <<- y                         ## y is input argument/matrix
                        inv <<- NULL                    ## set inverse to NULL 
                }
                get <- function() x                     ## function to get the input matrix
                setinverse <- function(inverse) inv <<- inverse ## function to calculate inverse of input matrix
                getinverse <- function() inv            ## function to get the inverse
                list(set = set, get = get,              ## return list of functions
                     setinverse = setinverse,
                     getinverse = getinverse)
}


## cacheSolve - computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {               ## takes makeCacheMatrix as input
        inv <- x$getinverse()
        if(!is.null(inv)) {                    ## check if inverse is already cached
                message("getting cached data") 
                return(inv)                    ## if yes, return it and end cacheSolve function        
        }
        data <- x$get()
        inv <- solve(data, ...)                ## calculate inverse of input matrix
        x$setinverse(inv)
        inv                                    ## return inverse of matrix
}
