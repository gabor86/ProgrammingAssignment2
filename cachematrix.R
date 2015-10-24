## makeCacHeMatrix takes a matrix as an argument and creates a list of functions

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##cacheSolve takes a list created by the makeCacheMatrix as its argument and checks if 
##it already contains an inverse. If yes, it prints the inverse. If not, it inverts the matrix, sets the inverse in
## the list created by makeCacheMatrix and also prints the inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
