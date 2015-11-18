#Store the inverse matrix in the memory to speed up future calculations 
#makeMatrix - Contractor function to a structure to be used with the cacheInverse function  
#Input : invertible matrix 
#Output - a list of functions  that contain globally set a head variables 
##Usage example 
#a<-matrix(runif(9),3)
#c<-makeMatrix(a)
#c1<-cacheInverse(c)

makeMatrix <- function(x = matrix(numeric())) {
    m <- NULL
    #set /change the matrix stored in memeory
    set <- function(y) {
        #superassignment  is used to change the global variable 
        x <<- y
        m <<- NULL
    }
    # get the matrix
    get <- function() x
    #stores the inverse matrix
    setInverse <- function(inverse) m <<- inverse
    #returns the inverse matrix stored in memory
    getInverse <- function() m
    list(#set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#cacheInverse : Returns an inverse matrix from memory if pre calculated
#else it calculates the inverse matrix , stores it an return it 
#input : the makeMatrix output
#Output - the inverted matrix

cacheInverse <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
