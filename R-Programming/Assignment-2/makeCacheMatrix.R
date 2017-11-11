## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

    invX <- NULL
    set <- function(y) {
        x <<- y
        invX <<- NULL
    }
    get <- function() x
    setInv <- function(inv) invX <<- inv
    getInv <- function() invX
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Return: a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    
    ## validate if the input object x has "getInv" method
    if (!("getInv" %in% names(x))) {
        message("Not a Cache Matrix object")
        return(NULL)
    }
    
    ## check if the cached data presented
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## validate the input object x has method "get"
    if (!("get" %in% names(x))) {
        message("Not a Cache Matrix object")
        return(NULL)
    }
    
    ## calculate the inverse matrix from original matrix of input object x
    inv <- try(solve(x$get(),...),silent=T)
        
    ## if class of result is not a matrix, the original matrix is not invertible
    if (class(inv) != "matrix") {
        message("Not a Invertible Matrix")
        return(NULL)
    }
    
    ## populate the inverse matrix to cache data
    x$setInv(inv)
    inv
}