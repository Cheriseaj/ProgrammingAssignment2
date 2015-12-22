## makeCacheMatrix creates a special matrix object that can cache its inverse
## 

## The first function, makeCacheMatrix creates a special matrix, which is really a list contianing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function (y) { 
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get, 
                setinverse = setinverse, 
                getinverse = getinverse)
}

## cacheSolve calculates the inverse of the matrix. But it first checkes to see if it has already been created. If so, it returns the
## cached inverse, if not it calculates the inverse matrix via the getinverse function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("Getting Cached Data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

