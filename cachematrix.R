## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        rmtx <- NULL
        
        setmtx <- function(y) {   # set the value of the matris
                x <<- y
                rmtx <<- NULL
        }
        getmtx <- function() x    #get the value of the matrix 
        setinver <- function(solve) rmtx <<- solve #set the value of the inverse matrix  
        getinver <- function() rmtx  #get the value of the inverse matrix  
        
        list(getmtx = getmtx,
             setinver = setinver,
             getinver = getinver)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        rmtx <- x$getinver() # set inverse values of the 'x'
        # if store inverse vlaues are exist return values in the cache. 
        if(!is.null(rmtx)){
                message("getting cached data")
                return(rmtx)
        }
        # if store inverse vlaues are not exist
        mtx <- x$getmtx()    # store matrix value of the 'x' into 'mtx'
        rmtx <- solve(mtx)   # store inverse matrix value of the 'x' into 'rmtx'
        x$setinver(rmtx)     # store inverse value into cache
        rmtx
}
