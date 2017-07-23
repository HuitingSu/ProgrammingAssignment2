## This function returns the inverse of the input matrix. Before calculating,  
## the function will check the cache whether the inverse has been calculated
## before. If yes, it will return the cache, thus to save time.

## makeCacheMatrix function creates a "matrix" object, which can cache its
## inverse if the inverse is calculated. 

makeCacheMatrix <- function(x = matrix()) {
        minverse <- NULL
        set <- function(y){
                x <<- y
                minverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) minverse <<- inverse
        getinverse <- function() minverse
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function calls makeCacheMatrix function to see if the inverse has
## been calculated. If yes, print the cache; if not, calculate the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minverse <- x$getinverse()
        if(!is.null(minverse)) {
                message("getting cached data")
                return(minverse)
        }
        data <- x$get()
        minverse <- solve(data)
        x$setinverse(minverse)
        minverse
}



