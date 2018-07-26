## makeCacheMatrix() defines the hadling of the matrix object
##It conteisn the following elements
##invmatrix variable of class matrix containing the inverted matrix
##funtions:
## setmatrix sets the working matrix
## getmatrix to obtain the current value
## setinvmatrix assigns the inverse of the matrix
## getinvmatrix to obtain the current value of the inverse matrix

##It returns a list of the previosuly mentioned functions

makeCacheMatrix <- function(x = matrix()) {
    invmatrix <- NULL #initializes the invmatrix to NULL
    
    setmatrix <- function(y) {
        x <<- y
        invmatrix <<- NULL
    }
    
    getmatrix <- function() x
    setinvmatrix <- function(inv) invmatrix <<- inv
    getinvmatrix <- function() invmatrix
    list(setmatrix=setmatrix, getmatrix=getmatrix, setinvmatrix=setinvmatrix, getinvmatrix=getinvmatrix)
}

## cacheSolve() uses makeCacheMatrix to calculate and cache the inverse of a matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invmatrix <- x$getinvmatrix()
    
    if(!is.null(invmatrix)) {
        message("getting cached data.")
        return(invmatrix)
    }
    
    data <- x$getmatrix()
    invmatrix <- solve(data)
    x$setinvmatrix(invmatrix)
    invmatrix
}
