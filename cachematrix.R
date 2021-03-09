makeCacheMatrix <- function(m = matrix()) {
    inv.m <- NULL
    
    #upon resetting matrix 'm' we lose
    #the information of the inverse
    set <- function(new.m) {
        m     <<- new.m
        inv.m <<- NULL 
    }
    
    get <- function() {
        m
    }
    
    setInverse <- function(new.inv.m) {
        inv.m <<- new.inv.m
    }
    
    getInverse <- function() {
        inv.m
    }
    
    # return a list of functions through which we can set/get the 
    # stored value of m(matrix) and inv.m(inverse of m)
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


cacheSolve <- function(x, ...) {
    #check whether the complex matrix object has the inverse stored or not
    m.inv <- x$getInverse()
    if (!is.null(m.inv)) {
        # if so then return tha cached inverse
        message("loading inverse from cache")
        return(m.inv)
    }
    
    # if not then we have to do the inversion here
    # before returning the inverse we'd like to cache it first for future use
    m     <- x$get()
    m.inv <- solve(m, ...)
    x$setInverse(m.inv)
    
    m.inv
}

test <- function() {
    matrix.obj <- makeCacheMatrix(matrix(rnorm(1e6), 1e3, 1e3))
    
    t0 <- Sys.time()
    inv.expected <- cacheSolve(matrix.obj)
    print(paste("Runtime of uncached inversion:", Sys.time() - t0))
    
    t0 <- Sys.time()
    inv.cached <- cacheSolve(matrix.obj)
    print(paste("Runtime of cached inversion:", Sys.time() - t0))
    
    # check if the two returned inverse matrix are the same
    print(identical(inv.expected, inv.cached))
} 
