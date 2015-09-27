## assignment to write a pair of functions that cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
        cachedinverse<- NULL # create inverse
        set <- function(userValue = matrix()) {
                x <<- userValue 
                cachedinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(invvalue) {
                cachedInv <<- invvalue 
                return(cachedinverse)
        }
        getinverse  <- function() cachedinverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
cacheResult <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) { 
        resultinverse <- x$getinverse() 
        if(!is.null(resultinverse) && is.matrix(resultinverse)) { 
                message("Cached data and saved values")
                return(resultinverse)
        }
        NewMatrix <- x$get()  
        resultinverse <- tryCatch({ 
                solve(NewMatrix)
        }, warning=function(w) {
                message("Correct?")
                message(w)
        }, error=function(e) {
                message("Error")
                message(e)
                message("\n")
        })
        message("Setting the value of inverse to:") 
        x$setinverse(resultinverse)
}