## The following pair of functions are used to create a special object that 
## stores a numeric invertible matrix and caches the inverse of it.

## This function creates a special "matrix" object, which can cashe the inverse
## of an invertible matrix. In analogy to the example in the instructions, this 
## function creates a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function gives you the inverse of the special "matrix" returned by the
## above function "makeCacheMatrix". If the inverse has already been calculated, 
## cacheSolve gets the inverse from the cache. Otherwise, it calculates it by 
## using the solve function and sets the inverse in the cache via the setinverse
## function. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


## Sample code:
## a <- matrix(1:4,2,2)
## myMatrix <- makeCacheMatrix(a)
## invMatrix <- cacheSolve(myMatrix)
## invMatrix <- cacheSolve(myMatrix)