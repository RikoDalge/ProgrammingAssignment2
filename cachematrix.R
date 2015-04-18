## makeCacheMatrix returns the inverse of a Matrix in a faster manner. 
#It considers previous calculus and reduce the processing time


#This function tests if the result is already stored into cache (memory)
#and if it does returns the value without lose processing time
#with not it calls casheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# This function assumes that the matrix is always invertible and returns
#the matrix's inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

#Riko Dalge - 04/18/2015
