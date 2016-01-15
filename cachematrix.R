
# The following two functions are used to cache the inverse of a matrix.

#Function 1
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



# Function 2 (Assumes Matrix is always invertible)
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data.")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

##Example Run
x <- rbind(c(1, 3), c(2, 4))
x

##[,1] [,2]
##[1,]    1    3
##[2,]    2    4

z <- makeCacheMatrix(x)

cacheSolve(z)

##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5


##
##
