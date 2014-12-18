## The object of this assignment is to cache the inverse of a matrix in 
## order to call upon it later instead of recalculating it, resulting in a gain
## of time. The following two functions will do this.

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function returns the inverse of the special matrix. It will
## first check if the result is already in the cache and if so will return it 
## without any other computation. If not, it will then proceed to calculate
## the inverse and cache it.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

## Sample test
## > x<- matrix(rnorm(16),4)
## > m=makeCacheMatrix(x)
## > m$get()
## [,1]       [,2]       [,3]      [,4]
## [1,] -0.23940112 -0.7990949 -0.5018423 0.9560346
## [2,] -0.04909691 -0.7173924  1.1542377 0.1002856
## [3,] -0.36436440  0.7416197 -0.9177909 0.2550885
## [4,] -0.76250456  2.4287415 -0.3571589 1.0358884
## > cacheSolve(m)
## [,1]       [,2]       [,3]      [,4]
## [1,]  0.6947519 -2.8560683 -4.2357352 0.6783577
## [2,] -0.2123333 -0.4660022 -0.6235214 0.3946220
## [3,] -0.1845769  0.5271896 -0.4115036 0.2206437
## [4,]  0.9455949 -0.8279616 -1.7978455 0.6155304
## > cacheSolve(m)
## getting cached data.
## [,1]       [,2]       [,3]      [,4]
## [1,]  0.6947519 -2.8560683 -4.2357352 0.6783577
## [2,] -0.2123333 -0.4660022 -0.6235214 0.3946220
## [3,] -0.1845769  0.5271896 -0.4115036 0.2206437
## [4,]  0.9455949 -0.8279616 -1.7978455 0.6155304
