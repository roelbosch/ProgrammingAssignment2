## Put comments here that give an overall description of what your functions do
## Write a short comment describing this function

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {     ## Function default mode is x = matrix
        inv <- NULL                             ## will hold value inverse 
        set <- function(y) {                    ## define the set function to assign new value of matrix in upper or parent environment
                x <<- y                         
                inv <<- NULL                    ## will reset inv to NULL when there is a new matrix
        }
        get <- function() x                     ## will return the value of the matrix argument x
        
        setinverse <- function(inverse) inv <<- inverse  ## will give the value of inv in upper or parent environment
        getinverse <- function() inv                     ## will give the value of inv when calles
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## to define the functions with the $ operator later in scripting
}


## This function will inverse the matrix i get by calling the script makeCacheMatrix as shown above.


cacheSolve <- function(x, ...) {      ## This will return a matrix that is the inverse of 'x' which is matrix
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
