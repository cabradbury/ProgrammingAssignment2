## R Programing Assignment 2
## 
## This code will create a fucntion which will calculate the inverse of a matrix.
## Initially, this will calcualte the inverse, afterwards, when called, it will 
## determine if the matix has changed or not, if not, it will pull the inverse 
## frome cache, rather than calculating it. If the matrix has changed, it will 
# calculate the new inverse and store it into cache. 
##
## 1. makeCacheMatrix: This function creates a special "matrix" object that can
##    cache an inverse.
##
## 2. cacheSolve: This function computes the inverse of the special "matrix" by 
##    makeCacheMatrix abobe. If the inverse has already been calculated (and 
##    the matrix has not changed), then the cachesolve() function should pull the 
##    inverse from cache. 
##
## 

## makeCacheMatrix(): This will create a special "matrix" object that can cache 
## an inverse. 
##
## 1. Set the value of the matrix.
## 2. Get the value of the matrix.
## 3. Set the value of the inverse of the matrix.
## 4. Get the value of the inverse of teh matrix. 

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        
        get <- function() x
       
        setMatrix <- function(solve) inverseMatrix <<- solve
        getMatrix <- function() inverseMatrix
        list(set=set, get=get, setMatrix=setMatrix, getMatrix=getMatrix)
}


## cacheSolve() will compute the inverse of the special "matrix" returned by
## makeCacheMatirx(), if the inverse has already been calculated and the 
## matrix has not changed, then it will retreive the inverse from cache. 
##
## This checks first to see if the inverse has already been calcuated. If
## it has, it returns the cached version. if it hasn't, it computes the 
## inverse and then stores the inverse in cache via setMatrix().

cacheSolve <- function(x = matrix(), ...) {
        inverseMatrix <- x$getMatrix()
        if(!is.null(inverseMatrix)) {
                
                # The line of code below is for testing - to assure that any
                # calcuation after the initial one is actually retreived 
                # from cache. I typically don't leave these print outs in 
                # final code, but for this exercise, I thought it would be 
                # good to keep this message.
                message("Retreiving inverse from cache...")
                
                # Return the cached inverse of the matrix. 
                return(inverseMatrix)
        }
        
        # If matrix has not had an inverse calcuated for it or the matrix has
        # changed, calcuate the inverse and allow it to be cached for future use.
        
        matrix <- x$get()
        inverseMatrix <- solve(matrix, ...)
        x$setMatrix(inverseMatrix)
        inverseMatrix
}
