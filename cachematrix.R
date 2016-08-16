## This pair of functions caches the inverse of a matrix




## makeCacheMatrix:  This function creates a special "matrix" object that can cache its inverse.
## It generates list of four functions to be accessed by cacheSolve

## Initialize object x as matrix and function argument
## Initialize object xinv set to NULL
makeCacheMatrix <- function(x = matrix()) { 
          xinv <- NULL                      
          
## Define 4 functions to be accessed by cacheSolve          

          ## 1. Define a set function with argument y
          ## Set x in parent environment equal to y; Set xinv in parent environment to NULL
          set <- function(y) {              
                  x <<- y                   
                  xinv <<- NULL             
          }
          
          ## 2. Define get function to return value of x from parent environment
          get <- function() x 
          
          ## 3. Define setinverse function to set xinv in parent environment to inverse
          setinverse <- function(inverse) 
            xinv <<- inverse                
          
          ## 4. Define getinverse function to return value of xinv from parent environment      
          getinverse <- function() xinv 
          
# Generate a list object containing the four functions and name them
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}









## cacheSolve:  This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the function retrieves the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        
        ## Set xinv equal to the cached inverse of the argument matrix
        xinv <- x$getinverse()
        
        ## If xinv is not null (i.e., the inverse for this matrix has arleady
        ## been calculated and cached) then return the cached value
        if (!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        
        ## Otherwise calculate the inverse for the matrix using solve function
        ## Note: this code assumes the matrix is invertible. If not invertible the 
        ## function will fail and return an error.
        data <- x$get()
        xinv <- solve(data, ...)
        
        ## Cache the calculated inverse for future reference
        x$setinverse(xinv)
        
        ## And return the inverse matrix
        xinv
}
