##This function will contain the functions to set and get the matrix 
##and its inverse

makeCacheMatrix <- function(x = matrix()) {
    ##Setting the inverse of the Matrix
    matinv<-NULL
    set<-function(y){
        x<<-y
        matinv<<-NULL
    }
    ##Getting the value of the Matrix
    get<- function() x
    ##Setting the inverse  
    setinverse<-function(solve) matinv <<- solve
    ##Getting the inverse of the Matrix
    getinverse<- function() matinv
    ##Listing the functions
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


##This function checks if the inverse has been calculated and returns it
##and if it has not it calculates it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matinv<-x$getinverse()
    ##Checks if value already exists
    if (!is.null(matinv)) {
        message("Getting cached inverse...")
        return(matinv)
    }
    ##Gets non-cached matrix inverse if it needs to
    data<-x$get()
    matinv<-solve(data)
    x$setinverse(matinv)
    matinv
}


