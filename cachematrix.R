## Functions below involve the construction and caching of 
## a matrix's inverse in cache  

## Function 'makeCacheMatrix' creates a special "matrix" 
## object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ## Parent Scope variable to be be modified
        set <- function(y) {
                x <<- y     ## Modify parent scope variable
                i <<- NULL  ## Modify parent scope variable
        }
        get <- function() x ## Get parent object 'x'
        setinverse <- function(inverse) i <<- inverse ## Set parent scope variable
        getinverse <- function() i ## Return cached 'i' value
        list(set = set, get = get, ## Return functions that construct, set and get a 
             setinverse = setinverse, ## matrix and it's inverse from cache
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" 
## returned by 'makeCacheMatrix'

cacheSolve <- function(x, m, ...) {
        i <- x$getinverse() ## Get cached 'i'
        
        ## If 'i' is set in cache and matrix has not changed
        if(!is.null(i) && identical(m, x$get())) { 
                message("getting cached inverse")
                return(i)
        }
        
        ## If the matrix has changed then reset the stored matrix in cache
        if(!identical(m, x$get())) x$set(m)
        
        data <- x$get() ## Get cached matrix object 'x'
        i <- solve(data, ...) ## Invert 'x'
        x$setinverse(i) ## Set 'i' in cache
        return(i) ## Return i
}