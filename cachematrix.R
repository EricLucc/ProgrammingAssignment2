#Cache the given matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(matrix) {
                mtx <<- matrix
                inv <<- NULL
        }
        get <- function() x 
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
}


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

#Test codes
matrix1 = matrix(c(1,1,1,2,3,4,3,4,4),3,3) #Creating a non-singular matrix
a <- makeCacheMatrix(matrix1) 
b <- cacheSolve(a)
a$get() %*% a$getinverse() #It should returns a identity matrix