## makeCacheMatrix() sets up the objects we'll use for caching our inverted matrix.

## cacheSolve() determines if we've already cached this particular matrix previously, return
## the existing inversion. If not, then do the solve() and then cache that result. 

## In makeCacheMatrix(), we need to create a very special kind of matrix object, so that it will 
## exist in the parent environment and allow us to cache a matrix.  Nothign good happens
## if we start with a non-squre matrix, so check for that first. 
## Ultimately, we've setup an object with various set and get functions that can be
## used. 

makeCacheMatrix <- function(x = matrix()) {

## we can't invert a matrix that isn't square, so error out if it is not
    myDims <- dim (x)
    if (myDims[1] != myDims[2]) stop("not a square matrix")
    
    m <- NULL
    
    set <- function ( y ) {
       x <<- y
       m <<- matrix()
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve is intended to take its argument, and return the inverse, using the solve() 
## function.  If the matrix has been previously inverted, we will have cached that 
## inversion and can simply return that matrix.  Otherwise, perform the inversion and cache it 
## in the matrix object we created in the makeCacheMatrix() function.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    m <- x$getinverse()
    
    ## if m now has a non-NULL value, then we've been here before and can use the 
    ## cached value. 
    
    if (!is.null(m)) {
        message ("getting cached data")
        return(m)
    }
   
    ## In this case, we are seeing a new matrix that we have not inverted previously.
    ## We get the matrix from the input object, performs a solve() and then uses the setinverse()
    ## action in the object to cache the inversion result we just acquired.

    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m) 
    m
}
